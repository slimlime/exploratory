(defparameter *screen-address* #x8000)

(defun update-vicky ()
  "Update VICKY from the monitor buffer"
  (copy-to-screen-buffer (monitor-buffer) *screen-address*))

(defparameter *font-height* 10)

(defun render-char ()

  ; some scratch area from the zero page
  ; I am going to define this in a preamble

  (zp-w :A0)
  (zp-w :A1)

  (zp-b :D0)
  (zp-b :D1)

  (label :render-char)

  (with-namespace :render-char
    (alias :char :A0)
    (alias :raster :A1)

    (alias :shift :D0)
    (alias :temp-char-index :D1)
    
    (LDY *font-height* (format nil "~a pixel character height" *font-height*))
    (BNE :go)
    (label :next)
    (dc "Move the raster to the next line")
    (LDA.ZP (lo-add :raster))
    (CLC)
    (ADC 39 "40 for screen, -1")
    (STA.ZP (lo-add :raster))
    (BCC :go)
    (INC.ZP (hi-add :raster))
    (label :go)
    (LDA.IZY :char "Get the bit pattern")
    (dc "Just jump to the next screen byte and blat it")
    (dc "if the bit pattern is empty")
    (BEQ :second-half)
    (LDX.ZP :shift)
    (BNE :shift-right)
    (dc "If the shift is zero then we can write the byte")
    (dc "and then clear the next one with no extra work")
    (STA.IZX :raster "We hope X is 0 here")
    (LDA 0)
    (BEQ :second-half)
    (dc "Shift the bit pattern across by the offset")
    (dc "and OR it with the screen")
    (label :shift-right)
    (LSR)
    (DEX)
    (BNE :shift-right)
    (ORA.IZX :raster "We hope X is 0 here")
    (STA.IZX :raster)
    (dc "Now shift the same character byte left by 8-offset")
    (LDA 8)
    (SEC)
    (SBC.ZP :shift)
    (TAX)
    (dc "Get the bit pattern again")
    (LDA.IZY :char)
    (label :shift-left)
    (ASL)
    (DEX)
    (BNE :shift-left)
    (label :second-half)
    (inc16.zp :raster)
    (STA.IZX :raster "X better be 0 here")
    (DEY)
    (BNE :next)
    (dc "Now, the first byte of the character data holds the")
    (dc "width of the character. Add it to the offset and")
    (dc "if necessary, increment the screen ptr")
    (LDA.IZY :char)
    (CLC)
    (ADC.ZP :shift)
    (CMP 8)
    (BCC :not-wrapped)
    (dc "There is a case where we will have a char of width 9")
    (dc "this means we need to advance an extra byte")
    (dc "Doing this compare clears the carry when we don't")
    (CMP 16)
    (AND.IMM #x7)
    (STA.ZP :shift)
    (sbc16.zp (1- (* 40 (1- *font-height*))) :raster)
    (RTS)
    (label :not-wrapped)
    (dc "Move the raster back up, and left 1 byte")
    (STA.ZP :shift)
    (sub16.zp (1+ (* 40 (1- *font-height*))) :raster)
    (RTS)

))

(defun render-test ()
  
  (reset-compiler)
  
  (flet ((pass ()
	   (org #x600)
	   (CLD)
	   (label :render-test)

	   (zp-b :count)
	   (zp-w :y)
	   
	   (sta16.zp #x80F0 :y)

	   (LDA 7)
	   (STA.ZP :count)
	   (label :doit)
	   (LDA.ZP :count)
	   (STA.ZP (cons :render-char :shift))

	   (LDA.ZP (lo-add :y))
	   (CLC)
	   (ADC #xB8)
	   (STA.ZP (lo-add :y))
	   (LDA.ZP (hi-add :y))
	   (ADC #x01)
	   (STA.ZP (hi-add :y))

	   (cpy16.zp :y (cons :render-char :raster))
	   	   
	   (sta16.zp '(:past . #\C) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\o) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\v) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\e) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\f) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\e) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\f) (cons :render-char :char))
	   
	   (JSR :render-char)
	   (sta16.zp '(:past . #\e) (cons :render-char :char))
	   
	   (JSR :render-char)	   
	   
	   (DEC.ZP :count)
	   (BNE :doit)

	   (BRK)

	   (render-char)
	   
	   (font-data)))

    (pass)
    (setf *compiler-ensure-labels-resolve* t)
    (pass))
 
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky)
)



