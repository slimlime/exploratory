;;Bresenham's line algorithm
;;attempt 1
;;where DX>DY - i.e. X major
;;and Y1>Y0     i.e. +ve
;;The input will be x-lo x-hi dx and dy/dx-lo dy/dx-hi which will be only the most
;;significant bits, thought this mask will only be important when we want to
;;store the delta in some compact form.

;;TODO could be a problem in the first pixel where #x7fff is insufficient to
;;     breach into the next y step for diagonal lines. Will need to add a test
;;     case for that.

;;IDEA For 8 bit error accumulator, we can work out if we will land on the
;;     wrong pixel at the end and supply an offset. Even if we do land on
;;     the right pixel we might want to supply an offset so that it looks
;;     symmetrical, e.g. for long shallow lines.

;;IDEA Unroll the loop into bits. On the last byte, i.e. width ==

(defun calculate-grad (maj1 min1 maj2 min2)
  "Calculate the 16 bit integer gradient between two points. The major
axis distance must be greater or equal to the minor axis distance."
  (assert (>= (abs (- maj1 maj2))
	      (abs (- min1 min2))))
  (assert (> (abs (- maj1 maj2)) 0))
  (floor (* #xffff (abs (/ (- min1 min2)
			   (- maj1 maj2))))))

(assert (= #x0 (calculate-grad 0 0 100 0)))
(assert (= #xffff (calculate-grad 0 0 100 100)))
(assert (= #x7fff (calculate-grad 0 0 100 50)))

(defun x1y1x2y2-xywgrad (x1 y1 x2 y2)
  "This function converts x1 y1 x2 y2 coordinates into
   values x y width gradient. Additionally, it re-orders
   the points so that x1 < x2.
TODO Make work for y major"
  (assert (/= x1 x2))
  (if (> x1 x2)
      (x1y1x2y2-xywgrad x2 y2 x1 y1)
      (values x1 y1 (- x2 x1 -1)
	      (calculate-grad x1 y1 x2 y2))))

;;; 0,0             |
;;;                 |
;;;                 |
;;;                 |
;;;                 |
;;;                 |           XMAJ
;;;                 |            -VE
;;;                 |
;;; ----------------+---------------
;;;                 |
;;;                 |           XMAJ
;;;                 |            +VE
;;;                 |
;;;                 |
;;;                 |
;;;                 | 
;;;                 |        320,200

;;; cycles for draw-test2 '(0 0 319 199) '(0 0 319 0) = 34552
;;;                                  branch less take = 34515
;;;                                  bcc              = 33706
;;;                                  lsr beq          = 33550
;;;                                  iny              = 33354

;;; fast lines                                        =


(defun draw-line-code ()
  (label :draw-line)
  (zp-w :x)
  (zp-w :y)
  (zp-w :width)
  (zp-w :delta)
  (label :draw-line2)
  (with-namespace :draw-line2
    (alias :ptr :y)
    (label :draw-byte)
    (mul40.zp :ptr)
    (adc16.zp (scradd 0 0) :ptr)
    (dc "Now scr contains the screen address plus the vertical offset")
    (dc "Let us now get the bit and byte horizontal offset")
    (LDA.ZP (lo-add :x))
    (AND.IMM #x7 "Bit position")
    (TAX "Hold onto the bit position")
    (LDA.ZP (hi-add :width))
    (LSR)
    (LDA.ZP (lo-add :width))
    (ROR)
    (LSR)
    (LSR)
    (STA.ZP (lo-add :width))
    (LDA.ZP (hi-add :x))
    (LSR)
    (LDA.ZP (lo-add :x))
    (ROR)
    (LSR)
    (LSR)
    (TAY "Y now contains the x pixel byte offset")
    (LDA.AB "delta:0")
    (dc "Copy the delta into the immediate adds")
    ;;this takes 32 cycles, saving 1 cycle per bit, so it only pays
    ;;off for lines > 32 horizontal pixels.
    (STA.AB "delta:1")
    (STA.AB "delta:2")
    (STA.AB "delta:3")
    (STA.AB "delta:4")
    (STA.AB "delta:5")
    (STA.AB "delta:6")
    (STA.AB "delta:7")
    (LDA.ABX :entry-points-hi)
    (PHA)
    (LDA.ABX :entry-points-lo)
    (PHA)
    (LDX #x80 "X now holds the fractional part, start halfway")
    (CLC)
    (RTS "Jump in to right place in unrolled loop")
    (let ((entry-points (mapcar #'(lambda (bit) (format nil "bit:~a" bit))
				'(0 1 2 3 4 5 6 7))))
      (apply #'db :entry-points-hi (mapcar #'rts-jmp-hi entry-points))
      (apply #'db :entry-points-lo (mapcar #'rts-jmp-lo entry-points)))    
    (dotimes (bit 8)
      (label (format nil "bit:~a" bit))
      (with-local-namespace
	(LDA (expt 2 (- 7 bit)))
	(ORA.IZY :ptr)
	(STA.IZY :ptr)
	(TXA)
	(dbg-cc)
	(label+1 (format nil "delta:~a" bit) :draw-line2)
	(ADC 0)
	(TAX)
	(BCC :next-bit)
	(TYA)
	(dbg-cs)
	(ADC (1- +screen-width-bytes+))
	(TAY)
	(BCC :next-bit)
	(INC.ZP (hi-add :ptr))
	(CLC)
	(label :next-bit)))
    (label :byte-done)
    (dc "Have we done all the bytes?")
    (DEC.ZP (lo-add :width))
    (BEQ :end-bits)
    (INY)
    (JMP "bit:0")
    (label :end-bits)
    (dc "Now lets draw the straggly end bits")
    (RTS))
  
  (label :draw-line1)
  (dc "For this entry point we require x, y and width
       all set in the zero-page, and A to contain the direction
       in which the vertical changes. grad-hi and grad-lo absolute
       should also be set.")

  (with-namespace :draw-line
    ;;debugging variables
    (let ((xval nil)
	  (yval nil)
	  (width nil)
	  (steps nil)
	  (delta nil)
	  (last-scr nil)
	  (last-bit nil))
      (dbg
	(setf steps 0) ;;how many x steps did we take so far?
	(setf xval (monitor-peek16 :x))
	(setf yval (monitor-peek16 :y))
	(setf delta (monitor-peek16 :delta))
	(setf width (monitor-peek16 :width))
	(setf last-bit nil)
	(setf last-scr nil)
	(dbg-assert (/= 0 width))
	(dbg-assert (<= (+ xval width) +screen-width+))
	(dbg-assert (< xval +screen-width+))
	(dbg-assert (< yval +screen-height+)))
      (mul40.zp :y)
      (dbg-assert (= (* 40 yval) (monitor-peek16 :y)))
      (adc16.zp (scradd 0 0) :y)
      (dbg-assert (= (scradd yval 0) (monitor-peek16 :y)))
      (dc "Now scr contains the screen address plus the vertical offset")
      (dc "Let us now get the bit and byte horizontal offset")
      (LDA.ZP (lo-add :x))
      (AND.IMM #x7 "Bit position")
      (TAX "Hold onto the bit position")
      (dc "Divide the x coord by 8, note that the high byte only has")
      (dc "one significant bit, so only one shift required and the")
      (dc "answer fits into A")
      (LSR.ZP (hi-add :x))
      (LDA.ZP (lo-add :x))
      (ROR)
      (LSR)
      (LSR)
      (dc "Now add the horizontal offset")
      (CLC)
      (ADC.ZP (lo-add :y))
      (STA.ZP (lo-add :y))
      (LDA.ZP (hi-add :y))
      (ADC 0)
      (STA.ZP (hi-add :y))
      (dc ":y now contains the address of the screen byte containing our first pixel")
      (dbg-assert (= (scradd yval (floor xval 8)) (monitor-peek16 :y)))
      (LDA.ABX :bits)
      (dbg-assert (= a (expt 2 (- 7 (logand xval 7))))
		  (format nil "a=~a xval=~a xval%7=~a" a xval (logand xval 7)))
      ;;the middle of the pixel is at 32768 in our fractional scheme
      (LDY 0)
      (STY.ZP (lo-add :delta))
      (LDX #x80)
      (STX.ZP (hi-add :delta))
      (dc "Now we are into Bresenham's algorithm")
      (dc "We increment x, and add the gradient to the accumulator")
      (dc "also decrement the width until it is zero")
      (TAX)
      (BNE :plot-point)
      (label :step)
      (TXA)
      (LSR)
      (BNE :ystep)
      (LDA 128)
      (label :ystep)
      (TAX)
      (dc "The carry is set iff and only if we need to go to the next byte")
      (LDA +screen-width-bytes+)
      (ADC.ZP (lo-add :y))
      (STA.ZP (lo-add :y))
      (BCC :skip-add) 
      (INC.ZP (hi-add :y))
      (CLC)
      (label :skip-add)
      (TXA)
      (label :plot-point)
      (dc "A should hold the bit pattern of the point we wish to plot")
      ;;some assertions to ensure the bit pattern rotates from left to
      ;;right
      (dbg-assert (find a '(128 64 32 16 8 4 2 1)))
      (dbg (incf steps))
      (dbg-assert (/= a 0))
      (dbg-assert (or (null last-bit)
		      (or (and (= last-bit 1)
			       (= a 128))
			  (= a (/ last-bit 2))))
		  (format nil "Last Bit = ~a a = ~a" last-bit a))
      (dbg (setf last-bit a))
      (dbg (setf last-scr (monitor-peek16 :y)))
      ;;some assertions to ensure that if the bit pattern rotated to
      ;;the next byte, we are ahead by one byte in the screen address 
      (ORA.IZY :y)
      (STA.IZY :y "plot")
      (DEC.ZP (lo-add :width))
      (BEQ :done)
      (label :add-gradient)
      (dc "Add the gradient. We know the carry is clear as we clear it explicitly
if we only did an x step, and for a y step it is clear as a side-effect.")
      (dbg-cc)
      (label+1 :grad-lo)
      (LDA 0)
      (ADC.ZP (lo-add :delta))
      (STA.ZP (lo-add :delta))
      (label+1 :grad-hi)
      (LDA 0)
      (ADC.ZP (hi-add :delta))
      (STA.ZP (hi-add :delta))
      (dc "Now, if the carry is set, then we went over and need to increment Y")
      (dc "Guess what. To subtract 0x10000 we don't do anything")
      (BCS :step)
      (dc "We advance the X by a bit, then a byte if necessary")
      (TXA)
      (LSR)
      (TAX)
      (CLC "Clear the carry here in readiness for the add gradient")
      (BNE :plot-point)
      (dc "Advance by one screen byte")
      (INY)
      (LDA 128)
      (TAX)
      (BNE :plot-point)
      (label :done)
      (DEC.ZP (hi-add :width))
      (BPL :add-gradient)
      (dc "we are done")
      (dbg-assert (= steps width))
      (dbg
	(multiple-value-bind (x y)
	    (scrpxy last-scr (floor (- 7 (log last-bit 2))))
	  (format t "x2=~a y2=~a~%" x y)))
      (RTS)

      (db :bits 128 64 32 16 8 4 2 1))))

(defun draw-test (x y width grad)
  (reset-compiler)
  (flet ((pass ()
	   (org #x600)
	   (zeropage)
	   (CLD)
	   (label :draw-test)
	   (call-memset 0 (scradd 0 0) +screen-memory-length+)
	   (call-memset 43 *char-memory-address* +char-memory-length+)
	   (label :start)
	   (sta16.zp x :x)
	   (sta16.zp y :y)
	   (LDA (lo grad))
	   (STA.AB '(:draw-line . :grad-lo))
	   (LDA (hi grad))
	   (STA.AB '(:draw-line . :grad-hi))
	   (sta16.zp width :width)
	   (let ((cycles 0))
	     (dbg
	       (monitor-reset-profile)
	       (setf cycles (monitor-cc)))
	     (JSR :draw-line1)
	     (dbg (format t "Draw Line Cycles: ~a~%" (- (monitor-cc) cycles))))
	   (BRK)
	   (draw-line-code)
	   (label :end)
	   (BRK)

	   (memset)))

	 (pass)
	 (let ((end *compiler-ptr*))
	   (pass)
	   (assert (= end *compiler-ptr*) nil "Build was not stable"))
	 (setf *compiler-final-pass* t)
	 (pass)
	 (format t "Build size ~a~%" (- *compiler-ptr* origin)))

  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun draw-test1 (x1 y1 x2 y2)
  (multiple-value-bind (x y w grad)
      (x1y1x2y2-xywgrad x1 y1 x2 y2)
    (draw-test x y w grad)))

(defun draw-test2 (&rest lines)
  "Lines in format (x1 y1 x2 y2)"
  (reset-compiler)
  (flet ((pass ()
	   (org #x600)
	   (zeropage)
	   (CLD)
	   (label :draw-test)
	   (call-memset 0 (scradd 0 0) +screen-memory-length+)
	   (call-memset 43 *char-memory-address* +char-memory-length+)
	   (label :start)
	   (let ((cycles 0))
	     (dbg
	       (monitor-reset-profile)
	       (setf cycles (monitor-cc)))
	   
	     (dolist (line lines)
	       (multiple-value-bind (x y w grad)
		   (apply #'x1y1x2y2-xywgrad line)
		 (sta16.zp x :x)
		 (sta16.zp y :y)
		 (LDA (hi (+ #x7f grad))) ;;round it a bit better
		 (STA.AB '(:draw-line2 . "delta:0"))
		 (sta16.zp w :width)
		 (JSR :draw-line2)))
	     (dbg (format t "Drawing ~a lines Cycles: ~a~%"
			  (length lines)
			  (- (monitor-cc) cycles))))
	   (BRK)
	   (draw-line-code)
	   (label :end)
	   (BRK)
	   
	   (memset)))

	 (pass)
	 (let ((end *compiler-ptr*))
	   (pass)
	   (assert (= end *compiler-ptr*) nil "Build was not stable"))
	 (setf *compiler-final-pass* t)
	 (pass)
	 (format t "Build size ~a~%" (- *compiler-ptr* origin)))

  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))


(let ((*monitor-debug-mode* :assert))
  (draw-test1 10 10 200 11) ;;this one tests rotating the pixel when we don't y step
  )
