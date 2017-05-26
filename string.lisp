; assembler routines for building a string table in the first
; pass. Each instance of (dcs "") will be processed in a string
; table. In the next pass, the string table can be emitted and
; the labels resolved.

(defparameter *word-table* nil)
(defparameter *string-table* nil)
(defparameter *strings* nil)

(defun reset-string-table ()
  (setf *string-table* nil)
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
  (setf str (append-null str))
  (let ((strend (1- (length str))))
    (loop for i from 0 to strend do
	 (loop for j from (1- (length *string-table*)) downto 0 do
	      (let ((word (aref *string-table* j)))
		(when (and (<= (+ i -1 (length word)) strend)
			   (equal word (subseq str i (+ i (length word)))))
		  (funcall emit j word)
		  (incf i (1- (length word)))
		  (return)))))))

(defun build-string-table ()
  (setf *string-table* (sort-word-table)))

(defun process-test ()
  (reset-string-table)
  (process-string "The cat sat on")
  (process-string "the mat")
  (process-string "and didn't like it it it one bit")
  (process-string "The quick brown fox killed the lazy dog and ate his innards")
  (process-string "Sing a song of sixpence, a pocket full of eyes")
  (process-string "Shall I compare thee to a summer's ham?")
  (build-string-table))

(defun process-lines (file)
  (reset-string-table)
  (let ((in (open file :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
         while line do (process-string line))
      (close in)))
  (setf *string-table* (sort-word-table)))

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
						      (aref *string-table* i)))))
	    (setf str (append-eos str))
	    (assert (equal str str2)
	      (str str2) 
	      (format nil "Expected ~a, but decoded to ~a" str str2))))
    (format t "Total bytes of string data ~d" symcount)))

; assembler commands

(defun dcs (label str)
  "Define compressed string"
  (if *string-table*
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

(defun string-test ()
  (org #x600)
  
  (label :start)
  (ORA.AB :my-string)
  (dcs nil "This is an important string")
  
  (BRK)
  (BRK)
  (dcs nil "about which we need to sing")
  (dcs :my-string "a tale of gold and treasure")
  (label :end)
  (CLD))

(defun compile-string-test ()
  (reset-compiler)
  (reset-string-table)
  
  ; pass 1, get strings

  (string-test)

  ; pass 2, build table, intern strings

  (build-string-table)

  (string-test)

  ; pass 3, resolve all labels

  (setf *compiler-ensure-labels-resolve* t)

  (string-test))

; WIP decoding... not tested at all

(defun strtable (str)
  (let ((table (tunstall str 3))
	(one-chars nil)
	(two-chars nil)
	(three-chars nil))
    (loop for i from 0 to 255 do
	 (let ((sym (aref table i)))
	   (when (and (null three-chars)
		      (< (length sym) 3))
	     (setf three-chars i))
	   (when (and (null two-chars)
		      (< (length sym) 2))
	     (setf two-chars (- i three-chars)))))
    (setf one-chars (- 256 two-chars three-chars))
    
    (zp-b :rstr-sym)
    (zp-w :rstr-add)

    (label :rstr-nxt)
    (INC.ZP (lo :rstr-add))
    (BNE :rstr)
    (INC.ZP (hi :rstr-add))
    (BEQ :rstr-done)
    (dc "Render the string at STR-ADD")
    (label :rstr)
    (LDY.IMM #x0)
    (LDA.IZY :rstr-add)
    (BEQ :rstr-done)
    (CMP.IMM three-chars)
    (BCS :2char)
    (STA.ZP :rstr-sym)
    (TAX)
    (dc "Get the first character from the three character table")
    (dc "note that the address to :3ch-0 is offset, so that we")
    (dc "can use the value in A without subtracting")
    (LDA.ABX :3ch-0)
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX :3ch-1)
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX :3ch-2)
    (dc "The third character might be EOS")
    (BEQ :rstr-done)
    (JSR :rstr-emit)
    (JMP :rstr-nxt)
    (label :2char)
    (CMP.IMM (+ three-chars two-chars))
    (BCS :1char)
    (STA.ZP :rstr-sym)
    (TAX)
    (LDA.ABX :2ch-0)
    (JSR :rstr-emit)
    (LDX.ZP :rstr-sym)
    (LDA.ABX :2ch-1)
    (dc "The second character might be EOS")
    (BEQ :rstr-done)
    (label :1char)
    (JSR :rstr-emit)
    (JMP :rstr-nxt)
    (label :rstr-emit)
    (dc "Look up address of rendering function in jump table")
    (TAX)
    (LDA.ABX :1ch-h)
    (PHA)
    (LDA.ABX :1ch-l)
    (PHA)
    (label :rstr-done)
    (RTS)    

    (dc "Three character string table split into three")
    (dc "so that each character can be retrieved by indexing")
    (dc "without multiplication")

    (flet ((dtbl (label start end char-pos)
	     (label label)
	     (loop for i from start to end do
		  (let ((sym (aref table i)))
		    (db nil (aref sym char-pos)))))))

    (dtbl :3ch-0 0 (1- three-chars) 0)
    (dtbl :3ch-1 0 (1- three-chars) 1)
    (dtbl :3ch-2 0 (1- three-chars) 2)
    
    (dc "Two character string table")

    (dtbl :2ch-0 three-chars (+ -1 two-chars three-chars) 0)
    (dtbl :2ch-1 three-chars (+ -1 two-chars three-chars) 1)
  
    (dc "Addresses for the character jump table in two tables")
    (dc "hi-byte and lo-byte, offset by one for use by rts")

    (loop for i from (+ two-chars three-chars) do
	 (let ((label (format nil "render-~a" (aref table i))))
	   (lo label)
	   (hi label)))
))



