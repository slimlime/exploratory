; assembler routines for building a string table in the first
; pass. Each instance of (dcs "") will be processed in a string
; table. In the next pass, the string table can be emitted and
; the labels resolved.

(defparameter *word-table* nil)
(defparameter *symbol-table* nil)
(defparameter *strings* nil)

(defun reset-symbol-table ()
  (setf *symbol-table* nil)
  (setf *strings* nil)
  (setf *word-table* (make-hash-table :test 'equal)))

(defun add-to-word-table (str)
  (let ((count (gethash str *word-table*)))
    (setf (gethash str *word-table*)
	  (if (null count)
	      1
	      (1+ count)))))

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun process-string (str)
  (push str *strings*)
  (setf str (append-eos str))
  (loop for i from 0 to (- (length str) 1) do
       (add-to-word-table (subseq str i (+ 1 i))))
  (loop for i from 0 to (- (length str) 2) do
       (add-to-word-table (subseq str i (+ 2 i))))
  (loop for i from 0 to (- (length str) 3) do
       (add-to-word-table (subseq str i (+ 3 i)))))

(defun sort-word-table ()
  (let ((wordlist nil)
	(len 0))
    (maphash #'(lambda (k v)
		 (incf len)
		 (push (cons k v) wordlist)) *word-table*)
    (let ((words (make-array len)))
      (dotimes (i len)
	(setf (aref words i) (pop wordlist)))
      (setf len (min 256 len))
      (setf words
	    (subseq
	     (sort words #'> :key #'(lambda (symbol)
				      (if (= 1 (length (car symbol)))
					  (- 10000000 (char-code (aref (car symbol) 0)))
					  (* (cdr symbol) (length (car symbol))))))
	     0
	     len))
      ;;now it is sorted with the letters at the front alphabetically
      ;;followed by words by frequency*length
      ;;the the cdr of the cons pairs, set the index
      (stable-sort words #'< :key #'(lambda (symbol) (length (car symbol))))
      (dotimes (i len)
	(setf (aref words i)
	      (car (aref words i))))
      ;assert that code 0 is the eos indicator as we use BEQ
      ;to detect it in the code later
      (assert (char= (char (aref words 0) 0)  #\nul))
      words
      )))
 
(defun encode-string (str emit)
  (setf str (append-eos str))
  (let ((strend (1- (length str))))
    (loop for i from 0 to strend do
	 (loop for j from (1- (length *symbol-table*)) downto 0 do
	      (let ((word (aref *symbol-table* j)))
		(when (and (<= (+ i -1 (length word)) strend)
			   (equal word (subseq str i (+ i (length word)))))
		  (funcall emit j word)
		  (incf i (1- (length word)))
		  (return)))))))

(defun build-symbol-table ()
  (setf *symbol-table* (sort-word-table)))

(defun process-test ()
  (reset-symbol-table)
  (process-string "The cat sat on")
  (process-string "the mat")
  (process-string "and didn't like it it it one bit")
  (process-string "The quick brown fox killed the lazy dog and ate his innards")
  (process-string "Sing a song of sixpence, a pocket full of eyes")
  (process-string "Shall I compare thee to a summer's ham?")
  (build-symbol-table))

(defun process-lines (file)
  (reset-symbol-table)
  (let ((in (open file :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do (process-string line))
      (close in)))
  (setf *symbol-table* (sort-word-table)))

; check that all the strings we used as input
; can be reproduced
(defun validate-strings ()
  (let ((symcount 0))
    (dolist (str *strings*)
      (let ((str2 ""))
	    (encode-string str #'(lambda (i word)
				   (declare (ignore word))
				   (incf symcount)
				   (setf str2 (format nil "~a~a" str2
						      (aref *symbol-table* i)))))
	    (setf str (append-eos str))
	    (assert (equal str str2)
	      (str str2) 
	      (format nil "Expected ~a, but decoded to ~a" str str2))))
    (format t "Total bytes of string data ~d" symcount)))

; assembler commands

(defun dcs (label str)
  "Define compressed string"
  (if *symbol-table*
      ;if the string table is built, emit the string and supply
      ;the label
      (progn
	(when label
	  (label label))
	(let ((len 0))
	  (encode-string str #'(lambda (i word)
				 (declare (ignore word i))
				 (incf len)))
	  (add-hint len (format nil "DCS '~a'" str))
	  (encode-string str #'(lambda (i word)
				 (declare (ignore word))
				 (push-byte i)))))
      ;in the first pass, add the string to the table
      (process-string str)))

(defun emit-char-label (c)
  (format nil "EMIT-~a" c))

(defun strtable ()
  
  ;these variables refer to the first occurance of
  ;a symbol of that length in the symbol table
  
  (let ((first-two-char nil)
	(first-three-char nil))
    
    (loop for i from (1- (length *symbol-table*)) downto 0 do
	 (let ((sym (aref *symbol-table* i)))
	   (when (= (length sym) 3)
	     (setf first-three-char i))
	   (when (= (length sym) 2)
	     (setf first-two-char i))))

    (zp-b :rstr-sym 0)
    (zp-w :rstr-add 0)

    (label :rstr-nxt)
    (INC.ZP (lo :rstr-add))
    (BNE :rstr)
    (INC.ZP (hi :rstr-add))
    (BEQ :rstr-done)
    (dc "Render the string at RSTR-ADD")
    (label :rstr)
    (LDY.IMM #x0)
    (LDA.IZY :rstr-add)
    (BEQ :rstr-done)
    (CMP.IMM first-three-char)
    (BCC :2char)
    (STA.ZP :rstr-sym)
    (TAX)
    (dc "Get the first character from the three character table")
    (dc "note that the address to :3ch-0 is offset, so that we")
    (dc "can use the value in A without subtracting")
    (LDA.ABX (- (resolve :3ch-0) first-three-char))
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX (- (resolve :3ch-1) first-three-char))
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX (- (resolve :3ch-2) first-three-char))
    (dc "The third character might be EOS")
    (BEQ :rstr-done)
    (JSR :rstr-emit)
    (JMP :rstr-nxt)
    (label :2char)
    (CMP.IMM first-two-char)
    (BCS :1char)
    (STA.ZP :rstr-sym)
    (TAX)
    (LDA.ABX (- (resolve :2ch-0) first-two-char))
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX (- (resolve :2ch-1) first-two-char))
    (dc "The second character might be EOS")
    (BEQ :rstr-done)
    (label :1char)
    (JSR :rstr-emit)
    (JMP :rstr-nxt)
    (label :rstr-emit)
    (dc "Look up address of rendering function in jump table")
    (TAX)
    (LDA.ABX :1ch-hi)
    (PHA)
    (LDA.ABX :1ch-lo)
    (PHA)
    (label :rstr-done)
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
  
    (dc "Addresses for the character jump table in two tables")
    (dc "hi-byte and lo-byte, less one, for use by rts")

    (let ((lo (list :1ch-lo))
	  (hi (list :1ch-hi)))
      (loop for i from 0 to (1- first-two-char) do
	   (let ((label (emit-char-label (char (aref *symbol-table* i) 0))))	   
	     (push (lo (1- (resolve label))) lo)
	     (push (hi (1- (resolve label))) hi)))
      
      (apply #'db (nreverse lo))
      (apply #'db (nreverse hi)))))

(defun string-test ()
  (org #x600)
  (label :start)
  (dc "Emit a string into a buffer")
  (LDA.IMM (lo :str))
  (print (resolve :rstr-add))
  (STA.ZP :rstr-add)
  (LDA.IMM (hi :str))
  (print (1+ (resolve :rstr-add)))
  (STA.ZP (1+ (resolve :rstr-add)))
  (JSR :rstr)
  (BRK)
  (ds :str-buffer "                ")

  (dc "Each character must have its own labelled function")
  (dc "e.g. to render to the screen.")
  (dc "In this test program we will just")
  (dc "write it to memory at an index")

  (zp-b :output-string-index 0)

  (when *symbol-table*
    (loop for i from 0 to 255 do
	 (let ((c (aref *symbol-table* i)))
	   (when (= 1 (length c))
	     (label (emit-char-label c))
	     (LDA.IMM (char-code (char c 0)))
	     (JMP :emit)))))
  
  (label :emit)
  (LDY.ZP :output-string-index)
  (STA.ABY :str-buffer)
  (INC.ZP :output-string-index)
  (RTS)
  
  (when *symbol-table*
    (strtable))

  (dcs nil "This is a bunch of strings")
  (dcs nil "which appear in the program")
  (dcs nil "They will be analysed in the")
  (dcs nil "first pass of the compiler")
  (dcs nil "to build a string table")
  (dcs nil "in the second pass, the ")
  (dcs nil "string table will be built")
  (dcs nil "and the strings actually encoded")
  (dcs nil "a third pass is now required")
  (dcs nil "as the string table is variable")
  (dcs nil "length and so are the strings")

  (dcs :str "This is the test string")

  (label :end))

(defun compile-string-test ()
  (reset-compiler)
  (reset-symbol-table)
  
  ; pass 1, get strings

  (string-test)

  ; pass 2, build table, intern strings

  (build-symbol-table)

  (string-test)

  ; pass 3, resolve all labels

  (setf *compiler-ensure-labels-resolve* t)

  (string-test))




