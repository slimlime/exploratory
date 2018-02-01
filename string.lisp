(defparameter *freq-table* nil)
(defparameter *processed-strings* nil)
(defparameter *compiled-strings* nil)
(defparameter *string-table* nil)
(defparameter *defined-strings* nil) ;;strings defined this pass
                                     ;;cleared when building string table

;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass
(defparameter *string-table* nil)

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun process-string (str)
  (push str *processed-strings*)
  (loop for c across str do
       (incf (gethash c *freq-table*))))
  
(defun reset-symbol-table ()
  (setf *compiled-strings* nil)
  (setf *symbol-table* nil)
  (setf *processed-strings* nil)
  (setf *freq-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
  (loop for c across *charset* do
       (setf (gethash c *freq-table*) 0))
  (setf (gethash #\Nul *freq-table*) 0))


;; step 1. Make a function to huffman encode a string
;; step 2. Make a function to huffman decode a string

(defun build-huffman-string-table (freqs)
  (let ((l))
    (maphash #'(lambda (c f)
		 (when (> f 0)
		   (push (list c f) freqs)))
	     freqs)
    (huffman2 l)))

(defun build-huffman-bit-pattern-lookup (table)
  (let ((lookup (make-hash-table)))
    (dolist (e table)
      (assert (<= (fourth table) 16)
	      nil
	      "~a has a ~a bit pattern." (first table) (fourth table))
      (setf (gethash (first e) lookup) e))
    lookup))

(defun huffman-encode-string (lookup str)
  (let ((vec (make-array (length str)
			 :fill-pointer 0
			 :adjustable t
			 :element-type '(unsigned-byte 8)))
	(let ((word 0) ;;24 bits
	      (bits 0))
	  (loop for c across str do
	       (let ((e (gethash c lookup)))
		 (assert e nil "character ~a not in lookup" c)
		 ;;shift the bit patter to the right so it would be at the
		 ;;24 bit position if the word buffer were empty
		 (setf word (logior word (ash (- 8 bits) (fourth e))))
		 ;;now add on the number of bits
		 (incf bits (second e))
		 (when (>= 8 bits)
		   ;;now we can emit
		   (vector-push-extend (ash -16 word))
		   (setf word (logand #xffff00 (ash 8 word)))
		   (


(defun huffman-encoding-test ()
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

;; assembler commands

(defun dcs (label str)
  "Define compressed string and inline it here"
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
	  (add-hint len (format nil "DCS '~a' (~a->~a)" str (length str) len))
	  (setf (gethash str *string-table*) *compiler-ptr*)
	  (encode-string str #'(lambda (i word)
				 (declare (ignore word))
				 (push-byte i)))))
      ;;in the first pass, add the string to the table
      (progn
	(unless (gethash str *defined-strings*)
	  (process-string str))
	(setf (gethash str *string-table*) *compiler-ptr*)))
  ;;ensure that a string is inlined only once per pass
  (setf (gethash str *defined-strings*) t)
  (values))

(defun dstr (str)
  (aif (gethash str *string-table*)
       it
       (progn
	 (setf (gethash str *string-table* nil) 0)
	 0)))

  
;;;this must be called last, after the last use of dcs/dstr
;;;this hack means we don't have to reinitialise a hash set
;;;on each pass
(defun string-table ()
  (dc "String Table")
  (maphash #'(lambda (str _)
	       (declare (ignorable _))
	       (unless (gethash str *defined-strings*)
		 (dcs nil str)))
	   *string-table*)
  (clrhash *defined-strings*))

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
