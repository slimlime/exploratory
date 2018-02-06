(defun ninsert (o l)
  (let ((p nil))
    (do ()
	((or (null l)
	     (< (second o) (second (car l)))))
      (setf p l)
      (setf l (cdr l)))
    (if (consp p) (setf (cdr p) (cons o l)) nil)))

;;doesn't work for odd number of entries!

(defun huffman1 (symbols)
  (let ((q (sort (copy-list symbols) #'< :key #'second))
	(node nil))
    (do ()
	((null q))
      (setf node (let ((n1 (pop q))
		       (n2 (pop q)))
		   (list (list (first n1) (first n2))
			 (+ (second n1) (second n2)))))
      (ninsert node q))
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

;;; Return a vector containing the populations of each
;;; level of a huffman table, e.g. how many symbols of
;;; each length.
(defun huffman-population (table)
  (let* ((max-len (apply #'max (mapcar #'second table)))
	 (pop (make-array max-len)))
    (loop for i from 0 to (1- max-len) do
	 (setf (aref pop i) (list 0 nil)))
    (let ((index 0))
      (dolist (p table)
	(let ((len (1- (second p))))
	  ;;increment pop count
	  (incf (first (aref pop len)))
	  (when (null (second (aref pop len)))	    
	    ;;set lowest amount for this level
	    (setf (second (aref pop len))
		  (ash (third p) (- (second p) 16))))
	  (incf index))))
    pop))

(defun print-huffman (pattern)
  (let ((i -1))
    (dolist (p pattern)
      (format t "~3d ~a len:~3a bits:~16,'0b ~a~%"
	      (incf i)
	      (first p)
	      (second p)
	      (third p)
	      (ash (third p) (- (second p) 16))))))
