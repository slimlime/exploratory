;; 8 bit accumulator Bresenham's algorithm with adjusted offset
;; should be end-point accurate for small screens.

;;TODO. Y overflows on certain values, we don't want to handle that every byte!
;;0,0 319,174!!! 

(defun calculate-grad (maj1 min1 maj2 min2)
  (let ((dx (abs (- maj1 maj2)))
	(dy (abs (- min1 min2))))
  (assert (>= dx dy))
  (assert (> dx 0))
  (let* ((grad (min #xff (ceiling (* #x100 dy) dx))))
    ;; we have the grad, but if we actually do Bresenham here
    ;; then we will potentially land on the wrong pixel.
    ;; Now, we know there is an algorithm to tell us how far
    ;; off we are, and in the worst case it is to run the
    ;; 6502 under test and actually see where it takes us!
    ;; Rather than do that let us see if we can calculate it
    (values grad
	    (- dy (floor (+ #x80 (* dx grad)) 256))))))

(assert (= #x00 (calculate-grad 0 0 100 0)))
(assert (= #xff (calculate-grad 0 0 100 100)))
(assert (= #x80 (calculate-grad 0 0 100 50)))

(defun x1y1x2y2-xywgrad (x1 y1 x2 y2)
  "This function converts x1 y1 x2 y2 coordinates into
   values x y width gradient. Additionally, it re-orders
   the points so that x1 < x2.
TODO Make work for y major"
  (assert (/= x1 x2))
  (if (> x1 x2)
      (x1y1x2y2-xywgrad x2 y2 x1 y1)
      (multiple-value-bind (grad offset)
	  (calculate-grad x1 y1 x2 y2)
	(values x1 y1 (- x2 x1 -1) grad offset))))
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

;;; fast lines                                        = 17307

;; debugging counters to see if we land on the right pixel
(defparameter *ycount* nil)
(defparameter *xcount* nil)

(defun draw-line-fast ()
  (label :draw-line-fast)
  (with-namespace :draw-line-fast
    (alias :x :A0)
    (alias :y :A1)
    (alias :width :A2)
    (alias :gradient "delta:0")
    (alias :ptr :A1)
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
    (label+1 :offset-adjust)
    (LDX #x80)
    (CLC)
    (dbg (setf *ycount* 0)
	 (setf *xcount* -1)) ;;x counting starts at -1 as we always inc on plotting
    (RTS "Jump in to right place in unrolled loop")
    (let ((entry-points (mapcar #'(lambda (bit) (format nil "bit:~a" bit))
				'(0 1 2 3 4 5 6 7))))
      (apply #'db :entry-points-hi (mapcar #'rts-jmp-hi entry-points))
      (apply #'db :entry-points-lo (mapcar #'rts-jmp-lo entry-points)))   

    ;; bit 0 is special because we have to take an horizontal step

    (label :byte)
    (TXA)
    (dbg-cc)
    (label+1 "delta:0")
    (ADC 0)
    (TAX)
    (BCS :xy-step)
    (INY)
    (BNE :plot)
    (BEQ :ptr-overflow)
    (label :xy-step)
    (dbg (incf *ycount*))
    (TYA)
    (dbg-cs)
    (ADC +screen-width-bytes+)
    (TAY)
    (BCC :plot)
    (label :ptr-overflow)
    (INC.ZP (hi-add :ptr))
    (CLC)
    (label "bit:0")
    (label :plot)
    (dbg (incf *xcount*))
    (LDA 128)
    (ORA.IZY :ptr)
    (STA.IZY :ptr)

    (loop for bit from 1 to 7 do
	 (with-local-namespace
	   (TXA)
	   (dbg-cc)
	   (label+1 (format nil "delta:~a" bit) :draw-line-fast)
	   (ADC 0)
	   (TAX)
	   (BCC :plot)
	   (dbg (incf *ycount*))
	   (TYA)
	   (dbg-cs)
	   (ADC (1- +screen-width-bytes+))
	   (TAY)
	   (BCC :plot)
	   (INC.ZP (hi-add :ptr))
	   (CLC)
	   (label :plot)
	   (label (format nil "bit:~a" bit) :draw-line-fast)
	   (dbg (incf *xcount*))
	   (LDA (expt 2 (- 7 bit)))
	   (ORA.IZY :ptr)
	   (STA.IZY :ptr)))
	
    (label :byte-done)
    ;;we don't need to check Y here as it will have been done in the
    ;;y step.
    (dc "Have we done all the bytes?")
    (DEC.ZP (lo-add :width))
    (BEQ :end-bits)
    (JMP :byte)

    (label :end-bits)
    (dc "Now lets draw the straggly end bits")
    (RTS)))

(defun draw-line-fast-test (&rest lines)
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
	   (let ((cycles 0)
		 (total-cycles 0))
	     (dbg
	       (monitor-reset-profile)
	       (setf total-cycles (monitor-cc)))
	     (dolist (line lines)
	       (multiple-value-bind (x y w grad offset)
		   (apply #'x1y1x2y2-xywgrad line)
		 (dbg (setf cycles (monitor-cc)))
		 (sta16.zp x '(:draw-line-fast . :x))
		 (sta16.zp y '(:draw-line-fast . :y))
		 (LDA grad)
		 (STA.AB '(:draw-line-fast . :gradient))
		 (sta16.zp w '(:draw-line-fast . :width))
		 (JSR :draw-line-fast)
 		 
		 (let ((x1 (first line))
		       (y1 (second line))
		       (x2 (third line))
		       (y2 (fourth line)))
		   (dbg (format t " ~a,~a->~a,~a (w,g,o)=~a,~a,~a actual ->~a,~a cycles: ~a~%"
				x1 y1 x2 y2 w grad offset
				(+ *xcount* x1)
				(+ *ycount* y1)
				(- (monitor-cc) cycles))))))
	     (dbg (format t "Drawing ~a lines Cycles: ~a~%"
			  (length lines)
			  (- (monitor-cc) total-cycles))))
	   (BRK)
	   (draw-line-fast)
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

