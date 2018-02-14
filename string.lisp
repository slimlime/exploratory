(defparameter *string-table* nil)
(defparameter *defined-strings* nil)
(defparameter *charset* " !'`,-.0123456789?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
;;These parameters are set after the string table has been built
;;so that on the following pass the dcs routine can inline string
;;data
(defparameter *word-dictionary* nil)
(defparameter *huffman-lookup* nil)
(defparameter *huffman-table* nil)

;;TODO warning if goes over 16 bit code
;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun fmt-str (str &optional (spaces t))
  (when (characterp str)
    (setf str (format nil "~a" str)))
  (let ((s (make-string (length str))))
    (loop for c across str
       for i from 0 do
	 (if (and spaces (char= c #\  ))
	     (setf c #\_ )
	     (if (char= c #\Newline)
		 (setf c #\↵)
		 (when (char= c #\Nul)
		   (setf c #\¶))))
	 (setf (char s i) c))
       s))

(defun dump-frequency-table (freqs)
  (let ((col 0)
	(items nil))
    (do-hashtable (k v freqs)
      (push (cons k v) items))
    (dolist (f (sort items #'> :key #'cdr))
      (format t "~a ~6d | " (fmt-str (car f)) (cdr f))
      (when (= (incf col) 8)
	(terpri)
	(setf col 0)))))

(defun reset-strings ()
  (setf *huffman-table* nil)
  (setf *huffman-lookup* nil)
  (setf *word-dictionary* nil)
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal)))
  
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

(defun greedy-replace1 (str table emit)
  (let ((strend (1- (length str))))
    (loop for i from 0 to strend do
	 (tagbody
	    (loop for j from 0 to (1- (length table)) do
		 (let ((word (aref table j)))
		   (when (and (<= (+ i -1 (length word)) strend)
			      (equal word (subseq str i (+ i (length word)))))
		     (funcall emit j word)
		     (incf i (1- (length word)))
		     (go :next))))
	    (funcall emit nil (char str i))
	    :next))))

(defun pre-encode (dictionary str)
  (setf str (append-eos str))
  (let ((s (make-array 0 :fill-pointer 0 :element-type 'extended-char :adjustable t)))
    (greedy-replace1 str dictionary
		     #'(lambda (i c)
			 (vector-push-extend (if i (code-char (+ i 256)) c) s)))
    s))

(defun encode (lookup dictionary str)
  (setf str (pre-encode dictionary str))
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
	(loop for c across str
	     for j from 0 do
	     (let ((e (gethash c lookup)))
	       (assert e nil "character ~a not in lookup" c)
	       ;;shift the bit patter to the right so it would be at the
	       ;;24 bit position if the word buffer were empty
	       (setf word (logior word (ash (third e) (- 8 bits))))
	       ;;now add on the number of bits
	       (incf bits (second e))
	       (do ()
		   ((< bits 8))
		 (emit))))
	(when (> bits 0)
	      (emit))
	(when (> bits 0)
	  (emit)))
      (values vec (+ (* 8 (length vec)) bits)))))

;;turn a vector of symbols back into a string
(defun symbols-string (table-vec symbols)
  (let ((str (make-string (length symbols))))
    (loop for s in symbols
       for i from 0 to (1- (length symbols)) do
	 (setf (char str i) (first (aref table-vec s))))
    str))

(defun dcs (label str)
  "Define compressed string and inline it here"
  (if *huffman-lookup*
      ;;if the string table is built, emit the string and supply
      ;;the label
      (progn
	(when label
	  (label label))
	(let* ((data (encode *huffman-lookup* *word-dictionary* str))
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

(defun subsequences (str min max fn)
  (let* ((len (length str))
	 (end (- len min)))
    (loop for i from 0 to end do
	 ;;oboemabo, oboemabo
	 (loop for j from (+ i min) to (min len (+ i min max -2)) do
	      (funcall fn (subseq str i j))))))

(defmacro do-subsequences ((var str min max) &body body)
  `(subsequences ,str ,min ,max
		    #'(lambda (,var)
			,@body)))

(defun count-frequencies (str freqs)
  (loop for c across str do
       (if (gethash c freqs)
	   (incf (gethash c freqs))
	   (setf (gethash c freqs) 1))))

(defun find-best-word (dictionary strings)
  (let ((matches (make-hash-table :test 'equal))
	(best-size 100000)
	(best-word nil)
	(freqs (make-hash-table :test 'equal))
	(table nil)
	(lookup nil))
    (do-hash-keys (str strings)
      (do-subsequences (sub (append-eos str) 2 16)
	(if (gethash sub matches)
	    (incf (gethash sub matches))
	    (setf (gethash sub matches) 1))))
    (let ((dict (make-array (1+ (length dictionary)))))
      (loop for word across dictionary
	 for i from 0 do
	   (setf (aref dict i) word))
      (do-hashtable (sub f matches)
	  (when (> f 3)
	    (setf (aref dict (1- (length dict))) sub)
	    (do-hash-keys (str strings)
	      (count-frequencies (pre-encode str dict) freqs))
	    (setf table (build-huffman-string-table freqs))
	    (setf lookup (build-huffman-bit-pattern-lookup table))	    
	    (let ((compressed-size 0))
	      (do-hash-keys (str strings)
		(incf compressed-size
		      (length (encode lookup dictionary str))))
	      (when (< compressed-size best-size)
		(setf best-size compressed-size)
		(setf best-word sub)))))
      (setf (aref dict (1- (length dict))) best-word)
      ;;(format t "~4d ~s~%" best-size dict)
      dict)))

(defun create-dictionary (max-entries strings)
  (let ((dictionary #()))
    (dotimes (_ max-entries)
      (let ((new-dictionary (find-best-word dictionary strings)))
	(if (and (> (length dictionary) 1)
		 (equal (aref new-dictionary (1- (length new-dictionary)))
			(aref dictionary (1- (length dictionary)))))
	    (return-from create-dictionary dictionary)
	    (setf dictionary new-dictionary))))))

(defparameter *end-of-word* #xff)

;;;this must be called last, after the last use of dcs/dstr
;;;this hack means we don't have to reinitialise a hash set
;;;on each pass.
(defun string-table (dictionary)
  (dc "String Table")
  ;;Get the strings that need to be inlined at the end
  (let ((additions nil))
    (do-hash-keys (str *string-table*)
      (unless (gethash str *defined-strings*)
	(push str additions)))
    (dolist (str additions)
      (dcs nil str)))
  (clrhash *defined-strings*)
  (let ((uncompressed-size 0)
	(freqs (make-hash-table :test 'equal)))
    ;;Now process them for frequency analysis
    (do-hash-keys (str *string-table*)
      (incf uncompressed-size (length str))
      (count-frequencies (pre-encode dictionary str) freqs))
    ;;Ensure each character is represented at least once in
    ;;the table.
    (loop for c across *charset* do
	 (unless (gethash c freqs)
	   (setf (gethash c freqs) 0.00001)))
    (let* ((table (build-huffman-string-table freqs))
	   (lookup (build-huffman-bit-pattern-lookup table)))
      (setf *huffman-lookup* lookup)
      (setf *huffman-table* table)
      (setf *word-dictionary* dictionary)
      ;; A bit wasteful, but let's compress all the strings and see how long they
      ;; are
      (when *compiler-final-pass*
	(let ((compressed-size 0))
	  (do-hash-keys (str *string-table*)
	    (incf compressed-size
		  (length (encode lookup dictionary str))))
	  (format t "Strings ~a -> ~a (~d%)~%" uncompressed-size compressed-size
		  (round (/ compressed-size uncompressed-size 0.01)))))
      ;;let's hope we don't get this
      (assert (/= 0 (eos-index)) nil "EOS index cannot be 0 as it conflicts with the end of dictionary word terminator")
      (loop for word across dictionary do
	   (label word :word)
	   (let ((word-data nil))
	     (loop for c across word do
		  (let ((index (position c *huffman-table* :key #'first)))
		    (assert index nil "'~a' was not found in the huffman table" c)
		    (push index word-data)))
	     (push *end-of-word* word-data)
	     (apply #'db nil (nreverse word-data))))
      (label :dictionary-end)
      
      ;;  (huffman-pop-table :first-letters	     
      ;;		     *first-letter-huffman-table*
      ;;		     "First letters")
      (huffman-pop-table :general-letters
			 table
			 "General letters"))))
        
  ;;May dig out the first letter huffman table as a title case thing
  
    ;;  (dc "A lookup of first letters to general letter index")
    ;;  (apply #'db :first-letter-indexes
    ;;	 (if *huffman-table*
    ;;	     (mapcar #'(lambda (e) (position (car e) *huffman-table* :key #'car))
    ;;		     *first-letter-huffman-table*)
    ;;	     (list 0))))
    
(defun eos-index () (position #\Nul *huffman-table* :key #'car))
(defun eol-index () (position #\Newline *huffman-table* :key #'car))

(defun char-table (dictionary &optional (test-only nil))
  (when *huffman-table*
    (let ((lo (list :lo-char-offsets))
	  (hi (list :hi-char-offsets)))
      (dolist (e *huffman-table*)
	(let ((c (car e)))
	  (if (or (eq c #\Newline)
		  (eq c #\Nul))
	      (progn
		;;no character data for these, but we still need an entry
		;;better a gap here than a gap in the font table
		(push 0 lo)
		(push 0 hi))
	      (let ((code (char-code c)))
		(if (> code 255) ;this is a word not a letter
		    (let ((w (resolve (cons :word (aref dictionary (- code 256))))))
		      (push (lo w) lo)
		      (push (hi w) hi))
		      (if test-only
			  (progn
			    ;;dummy test character address
			    (push 0 lo)
			    (push #xD0 hi))
			  (progn
			    ;;check that we actually have the typeface data
			    ;;for this character
			    (resolve (cons :present c))
			    (resolve (cons :past c))
			    (resolve (cons :future c))
			    ;;store the relative offset into the font
			    (let ((offset (- (resolve (cons :present c))
					     (resolve '(:font . :present)))))
			      (push (lo offset) lo)
			      (push (hi offset) hi)))))))))
      (apply #'db (nreverse lo))
      (apply #'db (nreverse hi)))))

(defparameter *test-strings*
  '("The quick brown fox killed the lazy dog and ate his innards"
    "The cat sat on"
    "the mat"
    "and didn't like it it it one bit"
    "Sing a song of sixpence, a pocket full of eyes"
    "Shall I compare thee to a summer's ham?"))

(defun string-test (string dictionary)
  (org #x600)

  (assert string nil "String was empty")
  
  (label :start)

  (zp-w :ptr)
  
  (sta16.zp string :huffman-ptr)
  (LDX 1)
  (STX.ZP :huffman-bits)
  (sta16.zp :general-letters :huffman-pop-table)
  (label :another)
  (JSR :huffman-next)
  (CPX (nil->0 (eos-index)))
  (BEQ :done)
  (LDA.ABX :hi-char-offsets)
  (CMP (1+ (hi :dictionary-end)))
  (BLT :is-word)
  (TXA)
  (LDY.ZP :output-string-index)
  (STA.ABY :str-buffer)
  (INC.ZP :output-string-index)
  (BNE :another)
  (label :is-word)
  (STA.ZP (hi-add :ptr))
  (LDA.ABX :lo-char-offsets)
  (STA.ZP (lo-add :ptr))
  (LDY 0)
  (label :next-char)
  (LDX.ZP :output-string-index)
  (LDA.IZY :ptr)
  (CMP *end-of-word*)
  (BEQ :another) ;;need assertion that eos is NOT 0, the word terminator
  ;;can omit the null terminator if the last char is eos.
  (CMP (nil->0 (eos-index)))
  (BEQ :done)
  (STA.ABX :str-buffer)
  (INY)
  (INC.ZP :output-string-index)
  (BNE :next-char)

  (label :done)
  (BRK)

  (char-table dictionary t)
  
  
  (zp-b :output-string-index 0)
  
  ;; define some encoded strings, labelled with themselves

  (mapc #'(lambda (s) (dcs s s)) *test-strings*)

  (dbs :str-buffer 256 #xff)
       
  (huffman-decoder)
  
  (string-table dictionary)
  
  (label :end))

(defun compile-string-test (string dictionary)
  (reset-compiler)
  (reset-strings)
  
  (flet ((pass ()
	   (string-test string dictionary)))
    
    ;; pass 1, get strings
    
    (pass)
    
    ;; pass 2, build table, intern strings
        
    (pass)
    
    ;; pass 3, resolve all labels
    
    (setf *compiler-final-pass* t)
    
    (pass)))

(defun test-decoder (&optional (dictionary #()))
  ;;6502 string decode
  (compile-string-test "the mat" dictionary)
  (let ((huffvec (coerce *huffman-table* 'vector)))
    (do-hash-keys (str *string-table*)
      (compile-string-test str dictionary)
      (monitor-reset :start)
      (monitor-run :print nil)
      (let* ((*compiler-buffer* (monitor-buffer))
	     (buffer (subseq *compiler-buffer*
			     (resolve :str-buffer)
			     (position #xff *compiler-buffer*
				       :start
				       (resolve :str-buffer)))))
	(assert (> (length buffer) 0) nil "Output string buffer was empty for '~a'" str)
	(let ((output (symbols-string huffvec (coerce buffer 'list))))
	  (assert (equal output str))
	  ;;we must check that the huffman-ptr is left on the last byte + 1
	  ;;or it will mess up things that rely on it, i.e. the VM print commands
	  (assert (= (+ (resolve str)
			(length (encode *huffman-lookup* dictionary str)))
		     (peek-addr :huffman-ptr))
		  nil "Pointer not in right place for '~a' Expected ~4,'0X was ~4,'0X"
		  str (peek-addr :huffman-ptr) (+ (resolve str)
			 (length (encode *huffman-lookup* dictionary str)))))))))

(test-decoder)
(test-decoder #("The"))
(test-decoder #(" the "))
(test-decoder #("mat"))

