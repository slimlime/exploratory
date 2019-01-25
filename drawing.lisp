;;Bresenham's line algorithm
;;attempt 1
;;where DX>DY - i.e. X major
;;and Y1>Y0     i.e. +ve
;;The input will be x-lo x-hi dx and dy/dx-lo dy/dx-hi which will be only the most
;;significant bits, thought this mask will only be important when we want to
;;store the delta in some compact form.

(defun draw-line-code ()
  (label :draw-line)
  (zp-w :maj)
  (zp-w :len)
  (zp-w :min)
  (zp-w :grad)
  (zp-b :D0)

  (bit-lookup)
  
  
  (label :draw-line1)

  (let ((y :min)
	(scr :min)
	(x :maj))

    (let ((yval 0) ;;some debugging values
	  (xval 0))
      (dbg (setf yval (monitor-peek16 y))
	   (setf xval (monitor-peek16 x))
	   (print xval))
      (mul40.zp y)
      (dbg (assert (= (monitor-peek16 y) (* 40 yval))))
      (adc16.zp (scradd 0 0) y)
      (dc "Now scr contains the screen address plus the vertical offset")
      (dc "Let us now get the bit and byte horizontal offset")
      (LDA.ZP (lo x))
      (AND.IMM #x7 "Bit position")
      (dbg (assert (= a (logand #x7 xval)) nil "XVAL=~a YVAL=~a Pixel offset was ~a expected ~a"
		   xval yval a (logand #x7 xval)))
      (TAX)
      (dc "Divide the x coord by 8, note that the high byte only has")
      (dc "one significant bit, so only one shift required") 
      (LSR.ZP (hi x))
      (ROR.ZP (lo x))
      (LSR.ZP (lo x))
      (LSR.ZP (lo x))
      (dc "Now add the horizontal offset")
      (CLC)
      (ADC.ZP (lo scr))
      (STA.ZP (lo scr))
      (LDA.ZP (hi scr))
      (ADC 0)
      (STA.ZP (hi scr))
      (LDA.ZPX :bit-0)
      (STA.IZY scr))))
    
(defun draw-test (x y)
  (reset-compiler)
  (flet ((pass ()
	   (org #x600)
	   (zeropage)
	   (CLD)
	   (label :draw-test)

	   (call-memset 0 (scradd 0 0) +screen-memory-length+)
	   (call-memset 43 *char-memory-address* +char-memory-length+)
	   (label :start)
	   (sta16.zp x :maj)
	   (sta16.zp y :min)
	   (sta16.zp 100 :grad)
	   	   
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
