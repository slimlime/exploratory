(defparameter *line-spacing* (* (1+ *font-height*) 40))
(defparameter *act-font* :present)
(defparameter *act-colour* #x10)

(defun update-vicky ()
  (let ((buf (monitor-buffer))) ;need to abstract out the memory, ditch cl-6502
    (loop for i from 0 to +char-memory-length+ do
	 (setf (aref buf (+ i *char-memory-address*)) #x79))
    (setmem-copy buf)))

(defun typeset ()
    
  (zp-w :font)

  ;;TODO the namespaces here are a bit arbitrary, might want to clean it up

  ;;need to profile this
  (with-namespace :typeset-cs
    (alias :sym :D2)
    (alias :tmp-raster :A3)
    (alias :str :huffman-ptr)
    (label :typeset-cs nil)
    (LDA 0)
    (STA.ZP '(:typeset . :shift))
    (STA.ZP '(:typeset . :prev-width))
    (label :typeset-cs-continue nil) ;;but with a new string
    (cpy16.zp '(:typeset . :raster) :tmp-raster)
    (sta16.zp :general-letters :huffman-pop-table)
    (LDA 1)
    (STA.ZP :huffman-bits)
    (label :next)
    (JSR :huffman-next)
    (LDA.ABX :hi-char-offsets)
    (CMP *word-lo-page*)
    (BGE :is-word)
    (JSR :emit)
    (JMP :next)
    (label :is-word)
    (dc "Hi address contains length-2 in low three bits")
    (TAY)
    (AND.IMM #x7)
    (CLC)
    (ADC #x2)
    (STA.AB (1+ (resolve :word-offset)))
    (TYA)
    (LSR)
    (LSR)
    (LSR)
    (dc "Now we have a page offset to the dictionary, 1 based")
    (CLC)
    (ADC (1- (hi :dictionary)))
    (STA.AB (hi-add (1+ (resolve :word-ptr))))
    (LDA.ABX :lo-char-offsets)
    (STA.AB (lo-add (1+ (resolve :word-ptr))))
    (label :next-character)
    (label :word-offset)
    (LDY 0 "Offset")
    (DEC.AB (1+ (resolve :word-offset)))
    (BMI :next)
    (label :word-ptr)
    ;;THE end of string marker does not have to appear in the dictionary
    ;;it could be omitted and the length reduced by one
    (LDA.ABY 0 "Word address -1");;self modified parameter
    (TAX)
    (JSR :emit)
    (JMP :next-character)
    (label :emit)
    (CPX (nil->0 (eos-index)))
    (BEQ :done)
    (CPX (nil->0 (eol-index)))
    (BEQ :newline)
    (LDA.ABX :lo-char-offsets)
    (CLC)
    (ADC.ZP (lo-add :font))
    (STA.ZP (lo-add '(:typeset . :char)))
    (LDA.ABX :hi-char-offsets)
    (ADC.ZP (hi-add :font))
    (STA.ZP (hi-add '(:typeset . :char)))
    (JMP :typeset)
    (label :newline)
    (LDA 0)
    (STA.ZP '(:typeset . :shift))
    (STA.ZP '(:typeset . :prev-width))
    (SEC)
    (LDA (lo (1- (* 40 (1+ *font-height*)))))
    (ADC.ZP (lo-add :tmp-raster))
    (STA.ZP (lo-add :tmp-raster))
    (STA.ZP (lo-add '(:typeset . :raster)))
    (LDA (hi (1- (* 40 (1+ *font-height*)))))
    (ADC.ZP (hi-add :tmp-raster))
    (STA.ZP (hi-add :tmp-raster))
    (STA.ZP (hi-add '(:typeset . :raster)))
    (RTS)
    (label :done)
    (dc "Directly return to caller")
    (PLA)
    (PLA)
    (RTS))
  
  (label :typeset)
  
  (with-namespace :typeset
    (alias :char :A0)
    (alias :raster :A1)

    (alias :shift :D0)
    (alias :prev-width :D1) ;;note that the 'width' includes the kerning bit
    (zp-b :left-shift)
    
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
    (LDA.ZP :shift)
    (dc "Self modify branch to perform correct shift")
    (STA.AB (1+ (resolve :shift-left)))
    (CLC)
    (EOR #xFF)
    (ADC 9)
    (STA.AB (1+ (resolve :shift-right)))
    (LDY *font-height* (format nil "~a pixel character height" *font-height*))
    (LDX 0)
    (BEQ :go)
    (label :next)
    (CLC)
    (dc "Move the raster to the next line")
    (LDA.ZP (lo-add :raster))
    (ADC 39 "40 for screen, -1")
    (STA.ZP (lo-add :raster))
    (BCC :go)
    (INC.ZP (hi-add :raster))
    (label :go)
    ;;  X needs to be 0 throughout
    (LDA.IZY :char "Get the bit pattern")
    (dc "Just jump to the next screen byte and blat it")
    (dc "if the bit pattern is empty")
    (BEQ :skip-clear)
    (dc "Shift the bit pattern across by the offset")
    (dc "and OR it with the screen")
    (label :shift-right)
    (BNE 0)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (ORA.IZX :raster)
    (STA.IZX :raster)
    (dc "Now shift the same character byte left by 8-offset")
    (dc "Get the bit pattern again")
    (LDA.IZY :char)
    (label :shift-left)
    (BNE 0)
    ;;todo splitting :go into two functions depending on size of shift might save 20ms
    (ASL)
    (ASL)
    (ASL)
    (ASL)
    (ASL)
    (ASL)
    (ASL)
    (ASL)
    (BEQ :skip-clear)
    ;;clauses are duplicated for performance
    (dc "Clear the next byte")
    (inc16.zp :raster)
    (STA.IZX :raster)
    (DEY)
    (BNE :next)
    (LDA.IZY :char)
    (STA.ZP :prev-width)
    (RTS)
    (label :skip-clear)
    (dc "Don't clear the next byte")
    ;;side effect is overexuberant clearing which can paste over
    ;;the following line if we ignore this
    ;;I thought to replace this by setting the carry and calling into
    ;;next, but it didn't work
    (inc16.zp :raster)
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

(defun print-message ()

  ;; TODO The calling convention for this is terrible.
  ;; a) The number of lines should be stored in the string itself
  ;; b) VM-PR1 etc should be removed. This will vastly simplify things
  ;; c) All calls to print-message should either go through
  ;;     print-message           (address in registers) 
  ;;     print-message-inline    (address by deref-w)
  
  ;; Both entry points expect 1, 2 or 3 in Y for the number of lines
  ;; :print-message-no-deref we expect the typesetter already has the address
  ;; of the string in '(:typeset-cs . :str)
  ;; :print-message we expect the string address to be interned after the
  ;; call site so it can be retrieved with JSR :deref-w
    
  (labels ((cpy-src (lines) (scradd (live-row lines) 0))
	   (cpy-len (lines) (* (- 4 lines) +screen-width-bytes+ *line-height*))
	   (set-dst (lines) (scradd (live-row (- 4 lines)) 0))
	   (set-len (lines) (* lines +screen-width-bytes+ *line-height*)))
    
    (mapc #'(lambda (table-name fn)
	      (apply #'db (cons table-name :lo) (mapcar #'lo (mapcar fn '(1 2 3))))
	      (apply #'db (cons table-name :hi) (mapcar #'hi (mapcar fn '(1 2 3)))))
	  '(:scrcpy-src :scrcpy-len :scrset-dst :scrset-len)
	  (list #'cpy-src #'cpy-len #'set-dst #'set-len)))

  (label :print-message)

  (STY.ZP :lines)
  (JSR :deref-w)
  (STX.ZP (lo-add '(:typeset-cs . :str)))
  (STA.ZP (hi-add '(:typeset-cs . :str)))
  (LDY.ZP :lines)
  
  (label :print-message-no-deref)
  
  ;;Note that scrset-len is not strictly needed
  ;;as scrset-len+scrcpy-len=C
  ;;Call with number of lines 1, 2 or 3 in Y
  (with-namespace :print-message
    (alias :lines (first (free-dregs :memcpy :memset)))
    ;;scroll out the number of lines we need 
    ;;note that the tables are 1 based, so we take one off
    ;;the addresses.
    (STY.ZP :lines)
    (LDA.ABY (1- (resolve '(:scrcpy-src . :lo))))
    (STA.ZP (lo-add :A0))
    (LDA.ABY (1- (resolve '(:scrcpy-src . :hi))))
    (STA.ZP (hi-add :A0))
    (sta16.zp (scradd (live-row 0) 0) :A1)
    (LDA.ABY (1- (resolve '(:scrcpy-len . :lo))))
    (LDX.ABY (1- (resolve '(:scrcpy-len . :hi))))
    (JSR :memcpy)
    ;;now erase the gap for the new text
    (LDY.ZP :lines)
    (LDA.ABY (1- (resolve '(:scrset-dst . :lo))))
    (STA.ZP (lo-add :A0))
    (LDA.ABY (1- (resolve '(:scrset-dst . :hi))))
    (STA.ZP (hi-add :A0))    
    (LDA.ABY (1- (resolve '(:scrset-len . :lo))))
    (LDX.ABY (1- (resolve '(:scrset-len . :hi))))
    (STA.ZP :D0)
    (LDA 0)
    (JSR :memset)
    ;;now print the text- we can use the same address as the
    ;;place we started erasing...
    (LDY.ZP :lines)
    (LDA.ABY (1- (resolve '(:scrset-dst . :lo))))
    (STA.ZP (lo-add '(:typeset . :raster)))
    (STA.ZP (lo-add '(:typeset-cs . :tmp-raster)))
    (LDA.ABY (1- (resolve '(:scrset-dst . :hi))))
    (STA.ZP (hi-add '(:typeset . :raster)))
    (STA.ZP (hi-add '(:typeset-cs . :tmp-raster)))
    ;;first render the prompt
    (sta16.zp :prompt '(:typeset . :char))
    (LDA 0)
    (STA.ZP '(:typeset . :prev-width))
    (STA.ZP '(:typeset . :shift))
    (JSR :typeset)
    ;;put the raster back where it started
    (cpy16.zp '(:typeset-cs . :tmp-raster) '(:typeset . :raster))
    ;;now draw the text with and offset of 5 pixels which we
    ;;all know is the width of the prompt
    (LDA 0)
    (STA.ZP '(:typeset . :prev-width))
    (LDA 5)
    (STA.ZP '(:typeset . :shift))
    (JMP :typeset-cs-continue)))

;;TODO make this build specific to this file, and make specific versions
;;for elsewhere
(defun build (pass)
  (funcall pass)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defun odyssey (&optional (dictionary #()))
  (reset-compiler)
  (reset-strings)
  (let ((font :past))
    (flet ((pass ()
	     (zeropage)	     
	     (org #x600)
	     (label :start)
	     (CLD)
	     (label :render-test2)
	     (sta16.zp :str '(:typeset-cs . :str))
	     (sta16.zp (cons :font font) :font)
	     (sta16.zp #x8000 '(:typeset . :raster))
	     (JSR :typeset-cs)
	     (BRK)
	     (typeset)
	     (dcs :str (justify *odyssey* :width (font-width font)))
	     (huffman-decoder)
	     (string-table dictionary)
	     (label :end)
	     (font-data)))
      (build #'pass))) 
  
  (monitor-reset #x600)
  (monitor-run)
  (update-vicky))

(defun render-test3 ()

  ;;Test an OBOE error where the M is being overwritten, presumably by the
  ;;thing that clears the next position.
  (reset-compiler)
  (reset-strings)

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
  (reset-strings)

  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :render-test4)

	   (sta16.zp :str1 '(:typeset-cs . :str))
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)
	   (LDY 2)
	   (JSR :print-message)
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
  (reset-strings)

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
  (reset-strings)
  
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



