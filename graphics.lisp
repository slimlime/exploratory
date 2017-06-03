(defparameter *screen-address* #x8000)

(defun update-vicky ()
  "Update VICKY from the monitor buffer"
  (copy-to-screen-buffer (monitor-buffer) *screen-address*))

(defun render-char ()

  ; some scratch area from the zero page
  ; I am going to define this in a preamble

  (zp-w :A0)
  (zp-w :A1)
  (zp-w :A2)

  (zp-b :D0)
  (zp-b :D1)

  (label :render-char)

  (with-namespace :render-char
    (alias :char :A0)
    (alias :top :A1)
    (alias :raster :A2)

    (alias :shift :D0)
    (alias :temp-char-index :D1)

    ;todo swap x and y might free up the raster.
    ;we can always move it back to the top and across
    
    (dc "Get the screen pointer and transfer it to a temporary")
    (dc "zero page word, we will use that as our raster")
    (cpy16.zp :top :raster)
    (LDY 10 "10 pixel character height")
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
    (CLC)
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
    (inc16.zp :top)
    (AND.IMM #x7) 
    (label :not-wrapped)
    (STA.ZP :shift)
    (RTS)

))

(defun render-test ()
  
  (reset-compiler)
  
  (flet ((pass ()
	   (org #x600)
	   (CLD)
	   (label :render-test)
	   (LDA #x4)
	   (STA.ZP (cons :render-char :shift))
	   (sta16.zp #x80F0 (cons :render-char  :top))

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



