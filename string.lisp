(defparameter *freq-table* nil)
(defparameter *processed-strings* nil)
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
  (setf (gethash str *processed-strings*) t)
  (setf str (append-eos str))
  (loop for c across str do
       (incf (gethash c *freq-table*))))
  
(defun reset-strings ()
  (setf *processed-strings* (make-hash-table :test 'equal))
  (setf *freq-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
  (setf *huffman-table* nil)
  (setf *huffman-lookup* nil)
  (loop for c across *charset* do
       (setf (gethash c *freq-table*) 0))
  (setf (gethash #\Nul *freq-table*) 0))

(defun build-huffman-string-table (freqs)
  (let ((l))
    (maphash #'(lambda (c f)
		 (when (> f 0)
		   (push (list c f) l)))
	     freqs)
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

(defun huffman-init ()
  (LDX 1)
  (STX.ZP :huffman-bits))

(defun huffman-decoder ()

  (zp-w :huffman-pop-table)
  (zp-w :huffman-ptr)
  (zp-b :huffman-bits)
  
  (with-namespace :decoder
    ;;these three parameters must be set
    ;;bits must be set to one
    (alias :ptr :huffman-ptr)
    (alias :pop :huffman-pop-table)
    (alias :bits :huffman-bits)

    (zp-b :acc-hi)
    (zp-b :acc-lo)
    (zp-b :next-byte)

    (label :huffman-next nil)
    (LDY 0)
    (STY.ZP :acc-hi)
    (STY.ZP :acc-lo)
    (DEY "Start at zero in pop table, following the first INY")
    (label :fetch-bit)
    (DEC.ZP :bits)
    (BNE :got-bits)
    (dc "New byte required")
    (dc "Save our pop table index")
    (TYA)
    (TAX)
    (LDY 0)
    (LDA.IZY :ptr)
    (STA.ZP :next-byte)
    (inc16.zp :ptr)
    (LDA 8)
    (STA.ZP :bits)
    (dc "Restore the pop table index")
    (TXA)
    (TAY)
    (label :got-bits)
    (dc "Lets rotate a bit from the next byte")
    (dc "into the accumulator")
    (ASL.ZP :next-byte)
    (ROL.ZP :acc-lo)
    (ROL.ZP :acc-hi)
    (INY)
    (LDA.IZY :pop "Any symbols at this length?")
    (BEQ :fetch-bit)
    (INY "Skip row indicator byte")
    (dc "We have some symbols- does the accumulator hold one?")
    (dc "It does if the accumulator is less than the max-code")
    (dc "for codes of this length. So let's compare it.")
    (LDA.IZY :pop)
    (CMP.ZP :acc-hi)
    (BMI :gt1 "acc-hi > max-code(len)-hi")
    (INY)
    (LDA.IZY :pop)
    (CMP.ZP :acc-lo)
    (BMI :gt2 "acc-lo > max-code(len)-lo")
    (INY)
    (dc "Now acc<=max-code(len), this means we have a symbol woo!")
    (dc "To get the symbol index we subtract the offset")
    (dc "which has kindly been pre-computed in the pop table")
    (SEC)
    (LDA.ZP :acc-lo)
    (SBC.IZY :pop "- offset lo")
    (TAX)
    (INY)
    (LDA.ZP :acc-hi)
    (SBC.IZY :pop "- offset hi")
    (dc "Return to caller with index-hi in A and index-lo in X")
    (dc "How nice for them. They can probably use LD?.ABX or something.")
    (RTS)
    (dc "Skip rest of row and get next bit")
    (label :gt1)
    (INY)
    (label :gt2)
    (INY)
    (INY)
    (BNE :fetch-bit)))

(defun pop-table (label huffman-table description)
  "Create a huffman population table"
  (label label)
  (dc description)
  (let ((len 0))
    (loop for p across (huffman-population huffman-table) do
	 (dc (format nil "~a symbols of length ~a" (first p) (incf len)))
	 (dc (format nil "Max Code: ~4,'0X Offset: ~4,'0X" (second p) (third p)))
	 (if (> (first p) 0)
	     (db nil #xff (hi (second p)) (lo (second p)) (lo (third p)) (hi (third p)))
	     (db nil #x00)))))

;;turn a vector of symbols back into a string
;;we won't need to do this on the 6502 as the symbol
;;indices will be used as indices into typeface data
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
    (maphash #'(lambda (s _) (declare (ignore _))
		       (incf total) ;;nul
		       (incf total (length s)))
	     *processed-strings*)  
    (format nil "~a strings for ~a characters.~%"
	    (hash-table-count *processed-strings*)
	    total))
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
      (maphash #'(lambda (s _) (declare (ignore _))
			 (assert
			  (equal (append-eos s)
				 (symbols-string table (huffman-decode-string pop 11 (huffman-encode-string lookup s))))))
	       *processed-strings*))))

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
  (clrhash *defined-strings*)

  ;;all strings will have been processed so lets create the huffman table

  (setf *huffman-table* (build-huffman-string-table *freq-table*))
  (setf *huffman-lookup* (build-huffman-bit-pattern-lookup *huffman-table*))

  (pop-table :string-pop-table *huffman-table* "General strings huffman population"))

(defun eos-index ()
  (position #\Nul *huffman-table* :key #'car))

(defun string-test (string)
  (org #x600)

  (dcs "the mat" "the mat")
  
  (label :start)
  
  (sta16.zp string :huffman-ptr)
  (sta16.zp :string-pop-table :huffman-pop-table)
  
  (huffman-init)
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

  (dw :str-buffer 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

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
  
  ; compile the program, *compiled-strings* will
  ; then contain a list of all its strings
  ; we can use it to recompile a test application
  ; and run it to make sure all the strings
  ; can be recovered in 6502

  (compile-string-test "the mat")
  
  (let ((huffvec (coerce *huffman-table* 'vector))
	(strings nil))
    (maphash #'(lambda (k v) (declare (ignore v)) (push k strings))
	     *processed-strings*)

    (dolist (str strings)

      (compile-string-test (gethash str *string-table*))
      (monitor-reset #x600)
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

	  (assert (= (+ (resolve str) (length (huffman-encode-string *huffman-lookup* str)))
		     (peek-addr :huffman-ptr)))
	  
	  ;;(format t "before=~a after=~a enclen=~a str=~a~%"
	;;	  (resolve str)
	;;	  (peek-addr :huffman-ptr)
	;;	  (length (huffman-encode-string *huffman-lookup* str))
	;;	  str)
	  (assert (equal output str)))))))

(test-decoder)

