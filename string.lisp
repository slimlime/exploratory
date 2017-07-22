; assembler routines for building a string table in the first
; pass. Each instance of (dcs "") will be processed in a string
; table. In the next pass, the string table can be emitted and
; the labels resolved.

(defparameter *word-table* nil)
(defparameter *symbol-table* nil)
(defparameter *processed-strings* nil)
(defparameter *compiled-strings* nil)
(defparameter *string-table* nil)

;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass
(defparameter *string-table* nil)

(defun add-to-word-table (str)
  (let ((count (gethash str *word-table*)))
    (setf (gethash str *word-table*)
	  (if (null count)
	      1
	      (1+ count)))))

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun process-string (str)
  (push str *processed-strings*)
  (setf str (append-eos str))
  (loop for i from 0 to (- (length str) 1) do
       (add-to-word-table (subseq str i (+ 1 i))))
  (loop for i from 0 to (- (length str) 2) do
       (add-to-word-table (subseq str i (+ 2 i))))
  (loop for i from 0 to (- (length str) 3) do
       (add-to-word-table (subseq str i (+ 3 i)))))

(defun reset-symbol-table ()
  (setf *compiled-strings* nil)
  (setf *symbol-table* nil)
  (setf *processed-strings* nil)
  (setf *word-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  ;; Adding newline now means it will be in a predictable
  ;; location, namely 1
  (process-string (string #\Newline)))

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
      (assert (char= (char (aref words 0) 0) #\nul))
      (assert (char= (char (aref words 1) 0) #\Newline))
      words)))

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

; check that (in theory) all the strings we used as input
; can be reproduced. Obviously the 6502 assembler to do
; it has to be right too...
(defun validate-strings ()
  (let ((symcount 0))
    (dolist (str *processed-strings*)
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
    (when *compiler-debug*
      (format t "Checked ~d bytes of string data~%" symcount))))

(defun build-symbol-table ()
  (setf *symbol-table* (sort-word-table))
  (when (< (length *symbol-table*) 256)
	   (setf *symbol-table*
		 (concatenate 'vector 
			      *symbol-table*
			      (make-array (- 256 (length *symbol-table*))
					  :initial-element "   "))))
  (validate-strings))

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

; assembler commands

(defun dcs (label str)
  "Define compressed string and inline it here"
  (let ((address *compiler-ptr*))
    (if *symbol-table*
	;;if the string table is built, emit the string and supply
	;;the label
	(progn
	  (when label
	    (label label))
	  (let ((len 0))
	    (encode-string str #'(lambda (i word)
				   (declare (ignore word i))
				   (incf len)))
	    (when *compiler-final-pass*
	      (push (cons *compiler-ptr* str) *compiled-strings*))
	    (add-hint len (format nil "DCS '~a'" str))
	    (encode-string str #'(lambda (i word)
				   (declare (ignore word))
				   (push-byte i)))
	    (setf (gethash str *string-table*) address)))
	;;in the first pass, add the string to the table
	(process-string str))
    address))

(defun dstr (str)
  "Define a compressed string and inline it later"
  (if *symbol-table*
      (if *compiler-final-pass*
	  (let ((address (gethash str *string-table*)))
	    (if address
		;;we know the address of the string, so
		;;return it.
		address
		(progn
		  ;;add it to the hashtable so we know
		  ;;we need to actually put it in the
		  ;;memory at some point.
		  (setf (gethash str *string-table*) 0)
		  0)))
	  (progn
	    (process-string str)
	    (push str *strings-for-string-table*)
	    0))))

(defun string-table ()
  (let ((strings nil))
    (maphash #'(lambda (str address)
		 (declare (ignorable address))
		 (push str strings))
	     *string-table*)
    (dc "String Table ~a entries" (length strings))
    (dolist (str strings)
      (dcs nil str))))
	       

(defun emit-char-label (c)
  (format nil "EMIT-~a" c))

(defun print-symbol-table ()
  (loop for i from 0 to 255 do
       (format t "~2,'0X:~5a" i (aref *symbol-table* i))
       (when (zerop (rem i 8))
	 (terpri))))

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

    (zp-w :decode-string-add 0)

    (with-namespace :decode-string

      (zp-b :sym 0)
      
      (label :next)
      (INC.ZP (lo-add :decode-string-add))
      (BNE :decode-string)
      (INC.ZP (hi-add :decode-string-add))
      (BEQ :done)
      (label :decode-string nil) ; in the global namespace
      (LDY #x0)
      (LDA.IZY :decode-string-add)
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
      (dc "Look up address of rendering function in jump table")
      (TAX)
      (LDA.ABX :1ch-hi)
      (PHA)
      (LDA.ABX :1ch-lo)
      (PHA)
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
  
      (dc "Addresses for the character jump table in two tables")
      (dc "hi-byte and lo-byte, less one, for use by rts")

      (let ((lo (list :1ch-lo))
	    (hi (list :1ch-hi)))
	(loop for i from 0 to (1- first-two-char) do
	     (let ((label (emit-char-label (char (aref *symbol-table* i) 0))))	   
	       (push (lo (1- (resolve label))) lo)
	       (push (hi (1- (resolve label))) hi)))
      
	(apply #'db (nreverse lo))
	(apply #'db (nreverse hi))))))

(defun string-test (string)
  (org #x600)
  (label :start)
  (dc "Emit a string into a buffer")
  (LDA (lo string))
  (STA.ZP (lo-add :decode-string-add))
  (LDA (hi string))
  (STA.ZP (hi-add :decode-string-add))
  (JSR :decode-string)
  (BRK)

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
	     (LDA (char-code (char c 0)))
	     (JMP :emit)))))
  
  (label :emit)
  (LDY.ZP :output-string-index)
  (STA.ABY :str-buffer)
  (INC.ZP :output-string-index)
  (RTS)
  
  (when *symbol-table*
    (strtable))

  ;; define some encoded strings

  (dcs nil "This is a bunch of strings")
  (dcs nil "which appear in the program")
  (dcs nil "They will be analysed in the")
  (dcs nil "first pass of the compiler")
  (dcs nil "to build a string table")
  (dcs nil "in the second pass, the")
  (dcs nil "string table will be built")
  (dcs nil "and the strings actually encoded")
  (dcs nil "a third pass is now required")
  (dcs nil "as the string table is variable")
  (dcs nil "length and so are the strings")

  (dcs :test-str "This is a test string")
  
  (dw :str-buffer 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

  (label :end))

(defun compile-string-test (&optional (string :test-str))
  (reset-compiler)
  (reset-symbol-table)

  (flet ((pass ()
	   (string-test string)))
    
    ;; pass 1, get strings
    
    (pass)
    
    ;; pass 2, build table, intern strings
    
    (build-symbol-table)
    
    (pass)
    
    ;; pass 3, resolve all labels
    
    (setf *compiler-final-pass* t)
    
    (pass)))

(defun test-decoder ()
  
  ; compile the program, *compiled-strings* will
  ; then contain a list of all its strings
  ; we can use it to recompile a test application
  ; and run it to make sure all the strings
  ; can be recovered in 6502

  (compile-string-test :test-str)

  (dolist (str *compiled-strings*)

    (compile-string-test (car str))
    (monitor-reset #x600)
    (monitor-run :print nil)

    (let ((*compiler-buffer* (monitor-buffer)))
      (let ((output 
	     (map 'string #'code-char
		  (subseq *compiler-buffer*
			  (resolve :str-buffer)
			  (position 0 *compiler-buffer* :start (resolve :str-buffer))))))
	(format t "~a~%" output)
	(assert (equal output (cdr str)))))))
