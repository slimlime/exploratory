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
	       (setf word (logior word (ash (fourth e) (- 8 bits))))
	       ;;now add on the number of bits
	       (incf bits (second e))
	       (when (>= bits 8)
		 (emit))))
	(when (> bits 0)
	      (emit))
	(when (> bits 0)
	      (emit))))
    vec))

(defun huffman-encoding-test ()
  (reset-symbol-table)
  (process-string "The cat sat on")
  (process-string "the mat") ;;1010|1101 0|1111|00|0 1001|1011| 1010^0000 173 120 155 160
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
    (print-huffman table)))
    ;(assert (equalp (huffman-encode-string lookup "the mat") #(173 120 155 160)))
    ;(assert (equalp (huffman-encode-string lookup " ") #(0)))
    (assert (equalp (huffman-encode-string lookup "    ") #(0)))))

(huffman-encoding-test)
