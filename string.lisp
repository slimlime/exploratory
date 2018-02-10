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

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun fmt-str (str &optional (spaces t))
  (when (characterp str)
    (setf str (format nil "~a" str)))
  (let ((s (make-string (length str))))
    (loop for c across str
       for i from 0 do
	 (if (and spaces (char= c #\  ))
	     (setf c #\_)
	     (if (char= c #\Newline)
		 (setf c #\↵)
		 (when (char= c #\Nul)
		   (setf c #\¶))))
	 (setf (char s i) c))
       s))

(defun reset-frequency-table ()
  (setf *freq-table* (make-hash-table :test 'equal))
  (loop for c across *charset* do
       (setf (gethash c *freq-table*) 0))
  (setf (gethash #\Nul *freq-table*) 0)
  (setf (gethash #\Newline *freq-table*) 0))

(defun dump-frequency-table ()
  (let ((col 0)
	(items nil))
    (do-hashtable (k v *freq-table*)
      (push (cons k v) items))
    (dolist (f (sort items #'> :key #'cdr))
      (format t "~a ~6d | " (fmt-str (car f)) (cdr f))
      (when (= (incf col) 8)
	(terpri)
	(setf col 0)))))

(defun process-string (str)
  (setf str (append-eos str))
  (loop for c across str do
       (incf (gethash c *freq-table*))))

(defun reset-strings ()
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
  (setf *huffman-table* nil)
  (setf *huffman-lookup* nil))
  
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

(defun huffman-encode-string (lookup str &key (no-eos nil))
  "Returns the huffman encoded string and the length in bits"
  (unless no-eos
    (setf str (append-eos str)))
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
	  (emit)))
      (values vec (+ (* 8 (length vec)) bits)))))

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
  (reset-frequency-table)
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

(defun analyse-string-table (&key (sort #'>) (key #'seventh) (top 32))
  (let ((matches (make-hash-table :test 'equal)))
    (do-hash-keys (str *string-table*)
      ;;build a hash table of all word frequencies
      (do-subsequences (sub (append-eos str) 2 16)
	(if (gethash sub matches)
	    (incf (gethash sub matches))
	    (setf (gethash sub matches) 1))))
    ;;we want only full English words
    (let ((words (make-hash-table :test 'equal)))
      (do-hash-keys (sub matches)
	(when (and (= (count #\  sub) 2)
		   (char= (char sub 0) #\ )
		   (char= (char sub (1- (length sub))) #\ ))
	  (setf (gethash (subseq sub 1 (1- (length sub))) words) t)))
      ;;now get a better estimate of the word frequencies
      (let ((all nil))
	(do-hash-keys (str words)
	  (let ((ftot 0))
	    (aif (gethash (format nil " ~a." str) matches) (incf ftot it))
	    (aif (gethash (format nil " ~a," str) matches) (incf ftot it))
	    (aif (gethash (format nil " ~a " str) matches) (incf ftot it))
	    (aif (gethash (format nil " ~a~a" str #\Newline) matches) (incf ftot it))
	    (when (> ftot 2)
	      (push (cons (format nil " ~a" str) ftot) all))))
	(flet ((addstr (str)
		 (aif (gethash str matches)
		      (push (cons str it) all))))
	  ;;and some interesting digraphs
	  (addstr (format nil "~a~a" #\. #\Nul))
	  (addstr (format nil "~a~a" #\. #\Newline))
	  (addstr (format nil "~a~a" #\  #\Newline))
	  (addstr "...")
	  (addstr ". ")
	  (addstr ", "))
	(format t "+------------------+------+------+------+------+------+---------+~%")
	(format t "| Word             |    f |  ΣfL |   Lh |  ΣLh |  b/c | Δd(est) |~%")
	(format t "+------------------+------+------+------+------+------+---------+~%")
	(let ((output nil))
	  (dolist (e all)
	    (let* ((s (fmt-str (car e)))
		   (f (cdr e))
		   (l (length (car e)))
		   (lh (multiple-value-bind (_ len)
			   (huffman-encode-string *huffman-lookup* (car e) :no-eos t)
			 (declare (ignore _))
			 (/ len 8.0)))
		   (sl (* l f))
		   (slh (* lh f)))
	      (push (list s f sl lh (round slh) (/ (* 8.0 lh) l)
			  (round (- slh (* 1.5 f))))
		    output)))
	  (setf output (subseq (sort output sort :key key) 0 (min (length output) top)))
	  (let ((totals nil))
	    (loop for col from (1- (length (first output))) downto 0 do
		 (if (numberp (nth col (first output)))
		     (push (round (reduce #'+ output :key #'(lambda (row) (nth col row)))) totals)
		     (push "Total" totals)))
	    
	    (loop for row in output do
		 (apply #'format t "| ~16a | ~4d | ~4d | ~2,2f | ~4d | ~2,2f |    ~4d |~%"
			row))
	    (apply #'format t "| ~16a | ~4d | ~4d | ~4d | ~4d | ~4d |    ~4d |~%"
		   totals))
	  ))))
  (format t "+------------------+------+------+------+------+------+---------+~%"))
			    
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
  (reset-frequency-table)
  ;;Now process them for frequency analysis
  (do-hash-keys (str *string-table*)
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
    (do-hash-keys (str *string-table*)
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

