(defparameter *freq-table* nil)
(defparameter *string-table* nil)
(defparameter *defined-strings* nil) ;;strings defined this pass
                                     ;;cleared when building string table
(defparameter *charset* " !'`,-.0123456789?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass

(defparameter *string-table* nil)
(defparameter *huffman-table* nil)
(defparameter *huffman-lookup* nil)

(defparameter *test-strings*
  '("The cat sat on"
    "the mat"
    "and didn't like it it it one bit"
    "The quick brown fox killed the lazy dog and ate his innards"
    "Sing a song of sixpence, a pocket full of eyes"
    "Shall I compare thee to a summer's ham?"))

;;TODO can probably save 256 bytes by packing the 'bits left' table
;;somewhere else. We could pack it in the lo byte of the prefix and
;;and it off, if all the symbols have codes < 12 bits. 

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun process-string (str)
  (setf str (append-eos str))
  (loop for c across str do
       (incf (gethash c *freq-table*))))
  
(defun reset-strings ()
  (setf *freq-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
  (setf *huffman-table* nil)
  (setf *huffman-lookup* nil)
  (loop for c across *charset* do
       (setf (gethash c *freq-table*) 0))
  (setf (gethash #\Nul *freq-table*) 0)
  (setf (gethash #\Newline *freq-table*) 0))

(defun build-huffman-string-table (freqs)
  (let ((l))
    (maphash #'(lambda (c f)
		 (when (> f 0)
		   (push (list c f) l)))
	     freqs)
    (assert l nil "The frequency table was empty")
    (huffman l)))

(defun build-huffman-bit-pattern-lookup (table)
  (let ((lookup (make-hash-table)))
    (dolist (e table)
      (assert (<= (second e) 16)
	      nil
	      "~a has a ~a bit pattern." (first e) (fourth e))
      (setf (gethash (first e) lookup) e))
    lookup))

(defun huffman-encode-string (lookup str)
  (setf str (append-eos str))
  (let ((vec (make-array (length str)
			 :fill-pointer 0
			 :adjustable t
			 :element-type '(unsigned-byte 8))))
    (let ((word 0) ;;24 bits
	  (bits 0))
      (flet ((emit ()
	       (vector-push-extend (ash word -16) vec)
	       (setf word (logand #xffff00 (ash word 8)))
	       (decf bits 8)))
	(loop for c across str do
	     (let ((e (gethash c lookup)))
	       (assert e nil "character ~a not in lookup" c)
	       ;;shift the bit patter to the right so it would be at the
	       ;;24 bit position if the word buffer were empty
	       (setf word (logior word (ash (third e) (- 8 bits))))
	       ;;now add on the number of bits
	       (incf bits (second e))
	       (when (>= bits 8)
		 (emit))))
	(when (> bits 0)
	      (emit))
	(when (> bits 0)
	      (emit))))
    vec))

;;decode a string using an input vector and a huffman population
;;count. Done in the manner it will have to be implmented in 6502
(defun huffman-decode-string (pop eos vec)
  (let ((maxlen (1- (length pop))) 
	(acc 0)    ;;accumulator- 16 bit?
	(next (aref vec 0))
	(pos 0)    ;;position in input- must be 1+ eos at end
	(bits 8)
	(out nil))
    (flet ((rol ()
	     (when (= bits 0)
	       (setf bits 8)
	       (incf pos)
	       (setf next (aref vec pos)))
	     (setf next (ash next 1))
	     (setf acc (logior (ash acc 1) (if (>= next 256) 1 0)))
	     (setf next (logand #xff next))
	     (decf bits)))
      (do ()
	  ((= pos (length vec)))
	(loop for i from 0 to maxlen do
	     (let ((p (aref pop i)))
	       (rol)
	       (when (> (first p) 0)
		 ;;some symbols exist at this length
		 (when (or (= i maxlen)
			   (<= acc (second p)))
		   (push (- acc (third p)) out)
		   (when (= (car out) eos)
		     (return-from huffman-decode-string (nreverse out)))
		   (setf acc 0)
		   (return)))))))
    (assert nil nil "Blew past eos for ~a, got ~a" vec out)))

;;turn a vector of symbols back into a string
(defun symbols-string (table-vec symbols)
  (let ((str (make-string (length symbols))))
    (loop for s in symbols
       for i from 0 to (1- (length symbols)) do
	 (setf (char str i) (first (aref table-vec s))))
    str))

(defun huffman-encoding-test ()
  (reset-strings)
  (mapc #'process-string *test-strings*)
  (let ((total 0))
    (dolist (s *test-strings*)
      (incf total) ;;nul
      (incf total (length s)))
  (let* ((table (build-huffman-string-table *freq-table*))
	 (lookup (build-huffman-bit-pattern-lookup table)))
    ;;(print-huffman table)
    (assert (equalp (huffman-encode-string lookup "the mat") #(106 33 162 182 0)))
    (assert (equalp (huffman-encode-string lookup " ") #(48)))
    (assert (equalp (huffman-encode-string lookup "sssssss") #(181 173 107 90 216)))
    (assert (equalp (huffman-encode-string lookup "    ") #(0 192)))
    ;;roundtrip all strings
    (let ((pop (huffman-population table)))
      (setf table (coerce table 'vector))
      (dolist (s *test-strings*)
	(assert (equal (append-eos s)
		       (symbols-string table (huffman-decode-string pop 11 (huffman-encode-string lookup s))))))))))

(huffman-encoding-test)

(defun dcs (label str)
  "Define compressed string and inline it here"
  (if *huffman-lookup*
      ;;if the string table is built, emit the string and supply
      ;;the label
      (progn
	(when label
	  (label label))
	(let* ((data (huffman-encode-string *huffman-lookup* str))
	       (len (length data)))
	  (add-hint len (format nil "DCS '~a' (~a->~a)" str (length str) len))
	  (setf (gethash str *string-table*) *compiler-ptr*)
	  (loop for byte across data do
	       (push-byte byte))))
      ;;in the first pass, add the string to the table
      (setf (gethash str *string-table*) *compiler-ptr*))
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
  ;;Get the strings that need to be inlined at the end
  (let ((additions nil))
    (dohashkeys (str *string-table*)
      (unless (gethash str *defined-strings*)
	(push str additions)))
    (dolist (str additions)
      (dcs nil str)))
  (clrhash *defined-strings*)
  ;;Now process them for frequency analysis
  (dohashkeys (str *string-table*)
    (process-string str))
  (setf *huffman-table* (build-huffman-string-table *freq-table*))
  (setf *huffman-lookup* (build-huffman-bit-pattern-lookup *huffman-table*))
  (huffman-pop-table :string-pop-table
		     *huffman-table*
		     "General strings huffman population"))

(defun eos-index ()
  (position #\Nul *huffman-table* :key #'car))

(defun eol-index ()
  (position #\Newline *huffman-table* :key #'car))

(defun string-test (string)
  (org #x600)
y
  (assert string nil "String was empty")
  
  (label :start)
  
  (sta16.zp string :huffman-ptr)
  (sta16.zp :string-pop-table :huffman-pop-table)
  (LDX 1)
  (STX.ZP :huffman-bits)
  (label :another)
  (JSR :huffman-next)
  (LDY.ZP :output-string-index)
  (TXA)
  (STA.ABY :str-buffer)
  (INC.ZP :output-string-index)
  (CMP (nil->0 (eos-index)))
  (BNE :another)
  (BRK)

  (zp-b :output-string-index 0)
  
  ;; define some encoded strings, labelled with themselves

  (mapc #'(lambda (s) (dcs s s)) *test-strings*)

  (dbs :str-buffer 256)
       
  (huffman-decoder)
  
  (string-table)
  
  (label :end))

(defun compile-string-test (&optional (string :test-str))
  (reset-compiler)
  (reset-strings)
  
  (flet ((pass ()
	   (string-test string)))
    
    ;; pass 1, get strings
    
    (pass)
    
    ;; pass 2, build table, intern strings
        
    (pass)
    
    ;; pass 3, resolve all labels
    
    (setf *compiler-final-pass* t)
    
    (pass)))

(defun test-decoder ()
  ;;6502 string decode
  (compile-string-test "the mat")
  (let ((huffvec (coerce *huffman-table* 'vector)))
    (dohashkeys (str *string-table*)
      (compile-string-test str)
      (monitor-reset :start)
      (monitor-run :print nil)
      (let ((*compiler-buffer* (monitor-buffer)))
	(let ((output 
	       (symbols-string huffvec
			       (coerce
				(subseq *compiler-buffer*
					(resolve :str-buffer)
					(position (eos-index) *compiler-buffer* :start (resolve :str-buffer)))
				'list))))
	  ;;we must check that the huffman-ptr is left on the last byte + 1
	  ;;or it will mess up things that rely on it, i.e. the VM print commands
	  (assert (= (+ (resolve str)
			(length (huffman-encode-string *huffman-lookup* str)))
		     (peek-addr :huffman-ptr))
		  nil "Pointer not in right place for '~a' Expected ~4,'0X was ~4,'0X"
		  str (peek-addr :huffman-ptr) (+ (resolve str)
			 (length (huffman-encode-string *huffman-lookup* str))))
	  (assert (equal output str)))))))

(test-decoder)

