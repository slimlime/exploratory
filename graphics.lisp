(defparameter *line-spacing* (* (1+ *font-height*) 40))

(defun update-vicky ()
  (let ((buf (monitor-buffer))) ;need to abstract out the memory, ditch cl-6502
    (loop for i from 0 to +char-memory-length+ do
	 (setf (aref buf (+ i *char-memory-address*)) #x79))
    (setmem-copy buf)))

(defun typeset ()
    
  (zp-w :font)

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
	(alias :tmp-raster :A3)

	(label :typeset-cs nil)
	(LDA 0)
	(STA.ZP '(:typeset . :shift))
	(STA.ZP '(:typeset . :prev-width))
	(label :typeset-cs-continue nil)
	(cpy16.zp '(:typeset . :raster) :tmp-raster)
	(JMP :first)
	(label :next)
	(INC.ZP (lo-add :str))
	(BNE :first)
	(INC.ZP (hi-add :str))
	(BEQ :done "Gone off the edge of the map")
	(label :typeset-cs-cont-test nil) ;don't reset shift position
	(label :first)
	(LDY 0)
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
	(CMP 1)
	(BEQ :newline)
	(dc "Look up address of character data")
	(dc "table is offset by two as EOS=0 NEWLINE=1")
	(TAX)
	(LDA.ABX (- (resolve :1ch-lo) 2))
	(CLC)
	(dc "Add the offset of the font")
	(ADC.ZP (lo-add :font))
	(STA.ZP (lo-add '(:typeset . :char)))	
	(LDA.ABX (- (resolve :1ch-hi) 2))
	(ADC.ZP (hi-add :font))
	(STA.ZP (hi-add '(:typeset . :char)))
	(JMP :typeset)
	(label :newline)
	(dc "Carry is clear from the cmp 1")
	(LDA (lo (1- (* 40 (1+ *font-height*)))))
	(ADC.ZP (lo-add :tmp-raster))
	(STA.ZP (lo-add :tmp-raster))
	(STA.ZP (lo-add '(:typeset . :raster)))
	(LDA (hi (1- (* 40 (1+ *font-height*)))))
	(ADC.ZP (hi-add :tmp-raster))
	(STA.ZP (hi-add :tmp-raster))
	(STA.ZP (hi-add '(:typeset . :raster)))
	(LDA 0)
	(STA.ZP '(:typeset . :shift))
	(STA.ZP '(:typeset . :prev-width))
	(label :done)
	(RTS)

	(dc "Three character string table split into three")
	(dc "so that each character can be retrieved by indexing")
	(dc "without multiplication")

	(flet ((dtbl (label start end char-pos)
		 (let ((bytes (list label)))
		   ;;label is the first argument to db, followed by the bytes
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
	(dc "hi-byte and lo-byte")
      	
	(let ((lo (list :1ch-lo))
	      (hi (list :1ch-hi)))
	  (loop for i from 2 to (1- first-two-char) do
	       ;start at 2 as 0 is EOS and 1 is NEWLINE
	       (let* ((c (char (aref *symbol-table* i) 0))
		      (label (cons :present c)))
		 
		 ;;compiler check that characters in the other two typefaces are present
		 
		 (resolve (cons :past c))
		 (resolve (cons :future c))

		 (when (and *compiler-debug* 
			    *compiler-final-pass*)
		   (format t "~4,'0X ~4,'0X ~c~%" (resolve label)
			   (resolve '(:font . :present))
			   c))

		 ;;store the relative offset into the font, this means of course
		 ;;that the characters in the font are in the same order
		 (setf label (- (resolve label)
				(resolve '(:font . :present))))
		 		 
		 (push (lo label) lo)
		 (push (hi label) hi)))

	  (apply #'db (nreverse lo))
	  (apply #'db (nreverse hi))))))
  
  (label :typeset)
  
  (with-namespace :typeset
    (alias :char :A0)
    (alias :raster :A1)

    (alias :shift :D0)
    (alias :prev-width :D1) ;;note that the 'width' includes the kerning bit
    
    (LDX 0 "Ensure we can use X indexed addressing later")
    (LDA.ZP :prev-width)
    (BEQ :start)
    (dc "Prior to this shift, bit 6 set if previous char admits to the right")
    (ASL "Now that flag is in bit 7")
    (dc "Bit 7 set iff prev char admits to right and current char admits to left")    
    (AND.IZX :char)
    (EOR #x80)
    (ASL)
    (dc "Now the carry is set iff there is no kerning between the two characters")
    (LDA.ZP :prev-width)
    (AND.IMM #xf)
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
    (JMP :start)
    (label :not-wrapped)
    (dc "Move the raster back up, and left 1 byte")
    (STA.ZP :shift)
    (sub16.zp (1+ (* 40 (1- *font-height*))) :raster)
    (label :start)
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
    (inc16.zp :raster)
    (BNE :skip-clear)
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
    (dc "don't clear the next character")
    (CMP 0)
    (BEQ :skip-clear)
    (STA.IZX :raster "X better be 0 here")
    (label :skip-clear)
    (DEY)
    (BNE :next)
    (dc "Now, the first byte of the character data holds the")
    (dc "width of the character. Store it, so we can use it")
    (dc "before we draw the next character")
    (LDA.IZY :char)
    (STA.ZP :prev-width)
    (RTS)))

(defun live-row (i)
  (+ 3 (* (+ i 13) *line-height*)))

(defun scroller (label lines)
  (label label)
  (call-memcpy (scradd (live-row 1) 0)
	       (scradd (live-row 0) 0)
	       (* lines +screen-width-bytes+ *line-height*))
  (call-memset 0 (scradd (live-row lines) 0)
	       (* +screen-width-bytes+ *line-height*))
  (RTS))

(defun print-message ()
  (scroller :scroll-text 3)

  ;;these entry points are a bit expensive 160 bytes!
  ;;TODO Some fancy scheme
  
  (label '(:print-message . 3))

  (call-memset 0 (scradd (live-row 0) 0)
	       (* 3 +screen-width-bytes+ *line-height*))

  (sta16.zp (scradd (live-row 0) 0) '(:typeset . :raster))
  
  (JMP '(:print-message . :print))
  
  (label '(:print-message . 2))

  (call-memcpy (scradd (live-row 2) 0)
	       (scradd (live-row 0) 0)
	       (* 1 +screen-width-bytes+ *line-height*))

  (call-memset 0 (scradd (live-row 1) 0)
	       (* 2 +screen-width-bytes+ *line-height*))

  (sta16.zp (scradd (live-row 1) 0) '(:typeset . :raster))
  
  (JMP '(:print-message . :print))
  
  (label '(:print-message . 1))
  
  (call-memcpy (scradd (live-row 1) 0)
	       (scradd (live-row 0) 0)
	       (* 2 +screen-width-bytes+ *line-height*))

  (call-memset 0 (scradd (live-row 2) 0)
	       (* 1 +screen-width-bytes+ *line-height*))

  (sta16.zp (scradd (live-row 2) 0) '(:typeset . :raster))
  
  (with-namespace :print-message
    (label :print)
    (alias :str '(:typeset-cs . :str))

    (JSR :deref-w)
    (STX.ZP (lo-add :str))
    (STA.ZP (hi-add :str))

    (sta16.zp :prompt '(:typeset . :char))
    (cpy16.zp '(:typeset . :raster) '(:typeset-cs . :tmp-raster))
    (LDA 0)
    (STA.ZP '(:typeset . :prev-width))
    (STA.ZP '(:typeset . :shift))
    (JSR :typeset)
    
    ;;put the raster back where it started. This is a bit boring.
    (cpy16.zp '(:typeset-cs . :tmp-raster) '(:typeset . :raster))

    (LDA 0)
    (STA.ZP '(:typeset . :prev-width))
    (LDA 5)
    (STA.ZP '(:typeset . :shift))

    (JMP :typeset-cs-continue)))

;;TODO make this build specific to this file, and make specific versions
;;for elsewhere
(defun build (pass)
  (funcall pass)
  (build-symbol-table)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defun odyssey ()
  (reset-compiler)
  (reset-symbol-table)
  (let ((font :past))
    (flet ((pass ()
	     (zeropage)	     
	     (org #x600)
	     (CLD)
	     (label :render-test2)
   
	     (sta16.zp :str '(:typeset-cs . :str))
	     (sta16.zp (cons :font font) :font)
	     (sta16.zp #x8000 '(:typeset . :raster))

	     (JSR :typeset-cs)

	     (BRK)

	     (typeset)
	     (font-data)

	     (dcs :str (justify *odyssey* :width (font-width font)))))
      (build #'pass))) 
  
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun render-test3 ()

  ;;Test an OBOE error where the M is being overwritten, presumably by the
  ;;thing that clears the next position.
  (reset-compiler)
  (reset-symbol-table)

  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :render-test3)
   
	   (sta16.zp :str1 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (LDA 5)
	   (STA.ZP '(:typeset . :shift))
	   (sta16.zp :str1 '(:typeset-cs . :str))
           (sta16.zp (+ #x80F0 *line-spacing*) '(:typeset . :raster))

	   (JSR :typeset-cs-preserve-shift)

	   (BRK)

	   (typeset)
	   (font-data)

	   (dcs :str1 "Millions of sad eyes peer out from the slime.")))
    
    (build #'pass))
  
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun render-test4 ()

  (reset-compiler)
  (reset-symbol-table)

  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :render-test4)

	   (sta16.zp :str1 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (JSR '(:print-message . 2))
	   (dw nil :str1)

	   (BRK)

	   (typeset)
	   (font-data)
	   (print-message)
	   (memcpy)
	   (memset)
	   (deref-w)
	   (dcs :str1 "Millions of sad eyes peer out from the
slime.")))
    
    (build #'pass))
  
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun render-test2 ()
  (reset-compiler)
  (reset-symbol-table)

  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :render-test2)
   
	   (sta16.zp :str1 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (sta16.zp :str2 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :past) :font)
           (sta16.zp (+ #x80F0 *line-spacing*) '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (sta16.zp :str3 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :future) :font)
	   (sta16.zp (+ #x80F0 (* 2 *line-spacing*)) '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (sta16.zp :kern '(:typeset-cs . :str))
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp (+ #x80F0 (* 3 *line-spacing*)) '(:typeset . :raster))

	   (JSR :typeset-cs)
	   
	   (sta16.zp :kern '(:typeset-cs . :str))
	   (sta16.zp '(:font . :past) :font)
	   (sta16.zp (+ #x80F0 (* 4 *line-spacing*)) '(:typeset . :raster))

	   (JSR :typeset-cs)
	 
	   (sta16.zp :kern '(:typeset-cs . :str))
	   (sta16.zp '(:font . :future) :font)
	   (sta16.zp (+ #x80F0 (* 5 *line-spacing*)) '(:typeset . :raster))

	   (JSR :typeset-cs)

	   (BRK)

	   (typeset)
	   (font-data)

	   (dcs :str1 "Chad Jenkins and his red Porsche.")
	   (dcs :str2 "King Chadric and his russet steed, Portia.")
	   (dcs :str3 "Galacto Imperator Chadrix.")
	   (dcs :kern "To The fefifof Potato Flibbly Tomato V,V.Vo")))
    
    (build #'pass))
  
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun render-test ()
  
  (reset-compiler)
  (reset-symbol-table)
  
  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :render-test)

	   (zp-b :count)
	   (zp-w :y)
	   
      	   (sta16.zp '(:font . :present) :font)
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

	   (sta16.zp :covefefe '(:typeset-cs . :str))

	   (LDA 0)
	   (STA.ZP '(:typeset . :prev-width))
	   
	   ;; call into the entry point that does not
	   ;; reset the shift position so we can check
	   ;; the rendering at every bit offset
	   (JSR :typeset-cs-cont-test)

	   (DEC.ZP :count)
	   (BNE :doit)

	   (BRK)

	   (dcs :covefefe "Covefefe")
	   (dcs :fluff " ")

	   (typeset)
	   
	   (font-data)))

    (build #'pass))

  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))



