(defparameter *freq-table* nil)
(defparameter *processed-strings* nil)
(defparameter *compiled-strings* nil)
(defparameter *string-table* nil)
(defparameter *defined-strings* nil) ;;strings defined this pass
                                     ;;cleared when building string table

;;this string table is basically a hash look-up of strings
;;to addresses, which should be valid on the final pass
(defparameter *string-table* nil)
(defparameter *huffman-table* nil)

;;TODO can probably save 256 bytes by packing the 'bits left' table
;;somewhere else. We could pack it in the lo byte of the prefix and
;;and it off, if all the symbols have codes < 12 bits. 

(defun append-eos (str)
  (format nil "~a~a" str #\nul))

(defun process-string (str)
  (setf str (append-eos str))
  (setf (gethash str *processed-strings*) t)
  (loop for c across str do
       (incf (gethash c *freq-table*))))
  
(defun reset-symbol-table ()
  (setf *compiled-strings* nil)
  (setf *processed-strings* (make-hash-table :test 'equal))
  (setf *freq-table* (make-hash-table :test 'equal))
  (setf *string-table* (make-hash-table :test 'equal))
  (setf *defined-strings* (make-hash-table :test 'equal))
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
	     (rol)
	     (format t "~a ~a~%" i (aref pop i))
	     (when (> (first (aref pop i)) 0)
	       ;;some symbols exist at this length
	       (when (or (= i maxlen)
			 (< acc (second (aref pop (1+ i)))))
		 (push (- acc (second (aref pop i))) out)
		 (print (car out))
		 (when (= (car out) eos)
		   (return-from huffman-decode-string (nreverse out)))
		 (setf acc 0)
		 (return))))))
    (assert nil nil "Blew past eos for ~a, got ~a" vec out)))
    
(defun huffman-encoding-test ()
  (reset-symbol-table)
  (process-string "The cat sat on")
  (process-string "the mat") ;;0110|1010 0|0100|00|1 1001|0101 |0110|0000 106 33 149 96
  (process-string "and didn't like it it it one bit")
  (process-string "The quick brown fox killed the lazy dog and ate his innards")
  (process-string "Sing a song of sixpence, a pocket full of eyes")
  (process-string "Shall I compare thee to a summer's ham?")
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
    (print-huffman table)
    (assert (equalp (huffman-encode-string lookup "the mat") #(106 33 162 176)))
    (assert (equalp (huffman-encode-string lookup " ") #(0)))
    (assert (equalp (huffman-encode-string lookup "    ") #(0)))))

(huffman-encoding-test)
