(defun huffman1 (symbols)
  (let ((q (sort (copy-list symbols) #'< :key #'second))
	(node nil))
    (do ()
	((null (cdr q)))
      (setf node (let ((n1 (pop q))
		       (n2 (pop q)))
		   (list (list (first n1) (first n2))
			 (+ (second n1) (second n2)))))
      (push node q)
      ;;Eugh. Should really use two lists and push to the back
      (setf q (sort q #'< :key #'second)))
    node))

;;; Build a canonical form huffman table
(defun huffman (symbols)
  (let ((tree (car (huffman1 symbols)))
	(patterns nil))
    (labels ((doit (tr depth)
	       (if (listp tr)
		   (progn
		     (when (> (length tr) 0)
		       (doit (first tr) (1+ depth)))
		     (when (> (length tr) 1)
		       (doit (second tr) (1+ depth))))
		   (push (list tr depth nil) patterns))))
      (doit tree 0))
    (setf patterns (sort patterns #'< :key #'second))
    ;;canonical form
    (setf (third (first patterns)) 0)
    (let ((code 0)
	  (prev-length (second (first patterns))))
      (dolist (p (cdr patterns))	
	(incf code)
	(unless (eq prev-length (second p))
	  (dotimes (_ (- (second p) prev-length))
	    (setf code (ash code 1)))
	  (setf prev-length (second p)))
	(setf (third p) (ash code (- 16 (second p))))))
    patterns))

;;test 'edge' case
(huffman '((#\L 1) (#\C 1) (#\B 1) (#\A 11)))

;;; Return a vector containing the populations of each
;;; level of a huffman table, e.g. how many symbols of
;;; each length.
(defun huffman-population (table)
  (let* ((max-len (apply #'max (mapcar #'second table)))
	 (pop (make-array max-len)))
    (loop for i from (1- max-len) downto 0 do
	 (setf (aref pop i) (list 0 nil nil)))
    (let ((index 0))
      (dolist (p table)
	(let* ((len (1- (second p)))
	       (e (aref pop len)))
	  ;;increment pop count
	  (incf (first e))
	  ;(when (null (second (aref pop len)))	    
	    ;;set highest amount for this level
	    (setf (second e)
		  (ash (third p) (- (second p) 16)))
	    ;;now calculate an offset that can be subtracted
	    ;;off the code to give the index.
	    (setf (third e) (- (second e) index))
	  (incf index))))
    pop))

(defun dump-huffman (pattern)
  (let ((i -1))
    (dolist (p pattern)
      (format t "~3d ~a len:~3a bits:~16,'0b ~a~%"
	      (incf i)
	      (first p)
	      (second p)
	      (third p)
	      (aif (third p)
		   (ash it (- (second p) 16))
		   nil)))))

;;note the commented out entry is the high offset; for less than 256 symbols we don't care
;;what that value is
(defun huffman-pop-table (label huffman-table description)
  "Create a huffman population table"
  (label label)
  (dc description)
  (let ((len 0))
    (loop for p across (huffman-population huffman-table) do
	 (dc (format nil "f(~d)=~2d Max:~4,'0X Del:~4,'0X"
		     (incf len) (first p) (second p) (third p))
	     t)
	 (if (> (first p) 0)
	     (db nil #xff (hi (second p)) (lo (second p)) (lo (third p)) #|(hi (third p))|#)
	     (db nil #x00)))))

(defun huffman-decoder ()

  ;; huffman-bits must be set to 1 before the first call
  ;; huffman-ptr points to the beginning of the data
  ;; huffman-pop-table must be set to the population table
  ;; for the data to be decoded.
  
  (zp-w :huffman-pop-table)
  (zp-w :huffman-ptr)
  (zp-b :huffman-bits)
  
  (with-namespace :decoder
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
    ;commented out as we don't have more than 256 symbols
    ;(INY)
    ;(LDA.ZP :acc-hi)
    ;(SBC.IZY :pop "- offset hi")
    (dc "Return to caller with index-hi in A and index-lo in X")
    (dc "How nice for them. They can probably use LD?.ABX or something.")
    (RTS)
    (dc "Skip rest of row and get next bit")
    (label :gt1)
    (INY)
    (label :gt2)
    (INY)
    ;(INY) ;commented out as we don't have more than 256 symbols
    (BNE :fetch-bit)))
