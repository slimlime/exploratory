(defparameter *screen-address* #x8000)

(defun update-vicky ()
  "Update VICKY from the monitor buffer"
  (copy-to-screen-buffer (monitor-buffer) *screen-address*))

(defparameter *font-height* 10)

(defun typeset ()
  
  ; some scratch area from the zero page
  ; I am going to define this in a preamble

  (zp-w :A0)
  (zp-w :A1)
  (zp-w :A2)
  
  (zp-b :D0)
  (zp-b :D1)
  (zp-b :D2)

  (when *symbol-table*

    (let ((first-two-char nil)
	  (first-three-char nil))
    
      (loop for i from (1- (length *symbol-table*)) downto 0 do
	   (let ((sym (aref *symbol-table* i)))
	     (when (= (length sym) 3)
	       (setf first-three-char i))
	     (when (= (length sym) 2)
	       (setf first-two-char i))))

      (with-namespace :typeset-cs

	(alias :sym :D2)
	(alias :str :A2)
      
	(label :next)
	(INC.ZP (lo-add :str))
	(BNE :typeset-cs)
	(INC.ZP (hi-add :str))
	(BEQ :done "Gone off the edge of the map")
	(label :typeset-cs nil)		; in the global namespace
	(LDY #x0)
	(LDA.IZY :str)
	(BEQ :done)
	(CMP first-three-char)
	(BCC :2char)
	(STA.ZP :sym)
	(TAX)
	(dc "Get the first character from the three character table")
	(dc "note that the address to :3ch-0 is offet, so that we")
	(dc "can use the value in A without subtracting")
	(LDA.ABX (- (resolve :3ch-0) first-three-char))
	(JSR :emit)
	(LDX.ZP :sym)
	(LDA.ABX (- (resolve :3ch-1) first-three-char))
	(JSR :emit)
	(LDX.ZP :sym)
	(LDA.ABX (- (resolve :3ch-2) first-three-char))
 	(dc "The third character might be EOS")
	(BEQ :done)
	(JSR :emit)
	(JMP :next)
	(label :2char)
	(CMP first-two-char)
	(BCC :1char)
	(STA.ZP :sym)
	(TAX)
	(LDA.ABX (- (resolve :2ch-0) first-two-char))
	(JSR :emit)
	(LDX.ZP :sym)
	(LDA.ABX (- (resolve :2ch-1) first-two-char))
	(dc "The second character might be EOS")
	(BEQ :done)
	(label :1char)
	(JSR :emit)
	(JMP :next)
	(label :emit)
	(dc "Look up address of character data")
	(dc "table is offset by one as EOS is not present")
	(TAX)
	(LDA.ABX (1- (resolve :1ch-hi)))
	(STA.ZP (hi-add '(:typeset . :char)))
	(LDA.ABX (1- (resolve :1ch-lo)))
	(STA.ZP (lo-add '(:typeset . :char)))
	(JMP :typeset)
	(label :done)
	(RTS)

	(dc "Three character string table split into three")
	(dc "so that each character can be retrieved by indexing")
	(dc "without multiplication")

	(flet ((dtbl (label start end char-pos)
		 (let ((bytes (list label)))
					;label is the first argument to db, followed by the bytes
		   (loop for i from start to end do
			(let ((sym (aref *symbol-table* i)))
			  (push (position (string (char sym char-pos))
					  *symbol-table* :test 'equal)
				bytes)))
		   (apply #'db (nreverse bytes)))))

	  (dtbl :3ch-0 first-three-char 255 0)
	  (dtbl :3ch-1 first-three-char 255 1)
	  (dtbl :3ch-2 first-three-char 255 2)
    
	  (dc "Two character string table")

	  (dtbl :2ch-0 first-two-char (1- first-three-char) 0)
	  (dtbl :2ch-1 first-two-char (1- first-three-char) 1))
  
	(dc "Addresses for the character data table in two tables")
	(dc "hi-byte and lo-byte, less one, for use by rts")
      	
	(let ((lo (list :1ch-lo))
	      (hi (list :1ch-hi)))
	  (loop for i from 1 to (1- first-two-char) do
	       ;start at 1 as 0 is EOS
	       (let ((label (cons :past (char (aref *symbol-table* i) 0))))	   
		 (push (lo label) lo)
		 (push (hi label) hi)))

	  (apply #'db (nreverse lo))
	  (apply #'db (nreverse hi))))))

  (label :typeset)
  
  (with-namespace :typeset
    (alias :char :A0)
    (alias :raster :A1)

    (alias :shift :D0)
    
    (LDX 0 "We need X to be 0")
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

(defun render-test2 ()
  (reset-compiler)
  (reset-symbol-table)
  (flet ((pass ()
	   
	   (org #x600)
	   (CLD)
	   (label :render-test2)
   
	   (sta16.zp :ctb :A2)
	   
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (BRK)

	   (typeset)
	   (font-data)

	   (dcs :ctb "King Chadric and his russet steed, Portia")))

    (pass)
    
    (build-symbol-table)
    
    (pass)
    
    (setf *compiler-final-pass* t)
    
    (pass)

  (monitor-reset #x600)
  (monitor-run)
  (update-vicky)


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
	   (STA.ZP (cons :typeset :shift))

	   (LDA.ZP (lo-add :y))
	   (CLC)
	   (ADC #xB8)
	   (STA.ZP (lo-add :y))
	   (LDA.ZP (hi-add :y))
	   (ADC #x01)
	   (STA.ZP (hi-add :y))

	   (cpy16.zp :y (cons :typeset :raster))
	   	   
	   (sta16.zp '(:past . #\C) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\o) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\v) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\e) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\f) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\e) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\f) (cons :typeset :char))
	   
	   (JSR :typeset)
	   (sta16.zp '(:past . #\e) (cons :typeset :char))
	   
	   (JSR :typeset)	   
	   
	   (DEC.ZP :count)
	   (BNE :doit)

	   (BRK)

	   (typeset)
	   
	   (font-data)))

    (pass)
    (setf *compiler-final-pass* t)
    (pass))
 
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky)
)



