(defparameter *letter-freqs* (make-hash-table :test 'equal))
(defparameter *first-letter-freqs* (make-hash-table :test 'equal))
(defparameter *string-table* nil)
(defparameter *defined-strings* nil) ;;strings defined this pass
                                     ;;cleared when building string table
(defparameter *charset* " !'`,-.0123456789?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass

(defparameter *string-table* nil)
(defparameter *huffman-table* nil)
(defparameter *huffman-lookup* nil)
(defparameter *first-letter-huffman-lookup* nil)
(defparameter *first-letter-huffman-table* nil)

(defparameter *test-strings*
  '("The cat sat on"
    "the mat"
    "and didn't like it it it one bit"
    "The quick brown fox killed The lazy dog and ate his innards"
    "Sing a song of sixpence, a pocket full of eyes"
    "Shall I compare thee to a Summer's ham?"))

;;note the capitals in the test strings, just to make the test function easier

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

(defun init-frequency-table (freqs)
  (clrhash freqs)
  (loop for c across *charset* do
       (setf (gethash c freqs) 0))
  (setf (gethash #\Nul freqs) 0)
  (setf (gethash #\Newline freqs) 0))

(defun reset-frequency-tables ()
  (init-frequency-table *letter-freqs*)
  (init-frequency-table *first-letter-freqs*))

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

(defun process-string (str)
  (setf str (append-eos str))
  (incf (gethash (char str 0) *first-letter-freqs*))
  (loop for i from 1 to (1- (length str)) do
       (incf (gethash (char str i) *letter-freqs*))))

(defun reset-strings ()
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
  (setf *huffman-table* nil)
  (setf *huffman-lookup* nil)
  (setf *first-letter-huffman-table* nil)
  (setf *first-letter-huffman-lookup* nil))
  
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

(defun huffman-encode-string (first-letter-lookup lookup str &key (no-eos nil))
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
	(loop for c across str
	     for j from 0 do
	     (let ((e (gethash c (if (zerop j)
				     first-letter-lookup
				     lookup))))
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
	(let* ((data (huffman-encode-string *first-letter-huffman-lookup* *huffman-lookup* str))
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

(defun estimate-bits (f freqs)
  (loop for e in *huffman-table* do
       (let ((fs (gethash (first e) freqs)))
	 (if (> f fs)
	     (return-from estimate-bits (second e)))))
  (second (car (last *huffman-table*))))

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
			   (huffman-encode-string *first-letter-huffman-lookup* *huffman-lookup* (car e) :no-eos t)
			 (declare (ignore _))
			 (/ len 8.0)))
		   (sl (* l f))
		   (slh (* lh f)))
	      (push (list s f sl lh (round slh) (/ (* 8.0 lh) l)
			  (round (- slh (* (/ (estimate-bits f *letter-freqs*) 8.0) f))))
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
  (reset-frequency-tables)
  ;;Now process them for frequency analysis
  (do-hash-keys (str *string-table*)
    (process-string str))  
  (setf *first-letter-huffman-table* (build-huffman-string-table *first-letter-freqs*))
  (setf *first-letter-huffman-lookup* (build-huffman-bit-pattern-lookup *first-letter-huffman-table*))
  (setf *huffman-table* (build-huffman-string-table *letter-freqs*))
  (setf *huffman-lookup* (build-huffman-bit-pattern-lookup *huffman-table*))

  (huffman-pop-table :first-letters	     
		     *first-letter-huffman-table*
		     "First letters")
  
  (huffman-pop-table :general-letters
		     *huffman-table*
		     "General letters"))

(defun eos-index ()
  (position #\Nul *huffman-table* :key #'car))

(defun eol-index ()
  (position #\Newline *huffman-table* :key #'car))

(defun string-test (string)
  (org #x600)

  (assert string nil "String was empty")
  
  (label :start)

  ;;initialise the huffman decoder with the "first letters" table
  ;;get the first letter, translate it into general letter index
  ;;reset the population table to "general letters"
  ;;get the reset
  
  (sta16.zp string :huffman-ptr)
  (sta16.zp :first-letters :huffman-pop-table)
  (LDX 1)
  (STX.ZP :huffman-bits)
  (JSR :huffman-next)
  (sta16.zp :general-letters :huffman-pop-table)
  (LDA.ABX :first-letter-indexes)
  (JMP :output)
  (label :another)
  (JSR :huffman-next)
  (TXA)
  (label :output)
  (LDY.ZP :output-string-index)
  (STA.ABY :str-buffer)
  (INC.ZP :output-string-index)
  (CMP (nil->0 (eos-index)))
  (BNE :another)
  (BRK)
 
  (apply #'db :first-letter-indexes
	 (if *huffman-table*
	     (mapcar #'(lambda (e) (position (car e) *huffman-table* :key #'car))
		     *first-letter-huffman-table*)
	     (list 0)))

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
			(length (huffman-encode-string *first-letter-huffman-lookup* *huffman-lookup* str)))
		     (peek-addr :huffman-ptr))
		  nil "Pointer not in right place for '~a' Expected ~4,'0X was ~4,'0X"
		  str (peek-addr :huffman-ptr) (+ (resolve str)
			 (length (huffman-encode-string *first-letter-huffman-lookup* *huffman-lookup* str))))
	  (assert (equal output str)))))))

;(test-decoder)

