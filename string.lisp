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

(defun huffman-decoder ()
  (with-namespace :decoder
    ;;these two parameters must be set
    (zp-w :huffman-pop-table)
    (zp-w :huffman-ptr)

    ;;for convenience the decoder gets reserved space in the
    ;;zero-page as the state is saved between invocations.
    ;;this saves callers from having to avoid collisions with
    ;;the general purpose D0 etc
 
    ;;Pop table format

    ;; empty-row 00 <- indicator byte
    ;;           FF max-code-hi max-code lo offset-hi offset-lo
    
    (alias :ptr :huffman-ptr)
    (alias :pop :huffman-pop-table)
    (zp-b :acc-hi)
    (zp-b :acc-lo)
    (zp-b :next-byte)
    (zp-b :bits)

    (label :huffman-init nil)
    (PHA)
    (LDA 0)
    (STA.ZP :len)
    (STA.ZP :acc-hi)
    (STA.ZP :acc-lo)
    (LDA 1)
    (dc "Set to 1, to imply we need a bit immediately")
    (STA.ZP :bits)
    (PLA)
    (RTS)
    
    (label :huffman-get nil)
    (LDY #xFF "Start at zero, following the first INY")
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
    (dc "It does if the accumulator is less than the second entry")
    (dc "in the pop table, so let us compare it.")
    (LDA.IZY :pop)
    (CMP.ZP :acc-hi)
    (BMI :gt1 "acc-hi > max-code-hi")
    (INY)
    (LDA.IZY :pop)
    (CMP.ZP :acc-lo)
    (BMI :gt2 "acc-lo > max-code-lo")
    (INY)
    (dc "Now acc<=max-code, this means we have a symbol woo!")
    (dc "To get the symbol index we subtract the offset")
    (SEC)
    (LDA.ZP :acc-lo)
    (SBC.IZY :pop)
    (TAX)
    (INY)
    (LDA.ZP :acc-hi)
    (SBC.IZY :pop)
    (dc "Return to caller with index-hi in A and index-lo in X")
    (dc "How nice for them. They can probably use LD?.ABX or something.")
    (RTS)
    (dc "Skip rest of row and get next bit")
    (label :gt1)
    (INY "Skip max-code lo")
    (label :gt2)
    (INY "Skip offset hi")
    (dc "Don't skip the last byte in the row; we'll do that in fetch")
    (BNE :fetch-bit)))

(defun pop-table (label huffman-table description)
  "Create a huffman population table"
  (label label)
  (dc description)
  (let ((len 0))
    (loop for p across (huffman-population huffman-table) do
	 (dc (format nil "~a symbols of length ~a" (first p) (incf len)))
	 (if (> 0 (first p))
	     (db nil #xff (hi (second p)) (lo (second p)) (hi (third p)) (lo (third p)))
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
  (reset-symbol-table)
  (process-string "The cat sat on")
  (process-string "the mat")
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
			  (equal s (symbols-string table (huffman-decode-string pop 11 (huffman-encode-string lookup s))))))
	       *processed-strings*))))

(huffman-encoding-test)
