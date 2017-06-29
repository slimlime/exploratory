;; So we want to be able to do the following

;; A lookup between words and their meaning ids

(defparameter *word-id-count* 0)
(defparameter *word-ids* nil)
(defparameter *id-meanings* nil)

(defun reset-parser ()
  (setf *word-ids* (make-hash-table :test 'equal))
  (setf *id-meanings* (make-hash-table))
  (setf *word-id-count* 0))

(defun defword (word &rest synonyms)
  (setf word (symbol-name word))
  (assert (null (gethash word *word-ids*)))
  (setf (gethash word *word-ids*) *word-id-count*)
  (setf (gethash *word-id-count* *id-meanings*) word)
  (dolist (synonym synonyms)
    (setf synonym (symbol-name synonym))
    (assert (null (gethash synonym *word-ids*)))
    (setf (gethash synonym *word-ids*) *word-id-count*))
  (incf *word-id-count*))

(defun parser ()
  (label :parse)
  (with-namespace :parser
    (alias :word :A0)
    (let ((words (make-array 0 :fill-pointer 0 :adjustable t))
	  (label-number 1))
      (maphash #'(lambda (k v)
		   (declare (ignorable v))
		   (vector-push-extend k words))
	       *word-ids*)
      (setf words (sort words #'string<))
      (dc (format nil "~a" words))
      (labels ((letter (i k)
		 (let ((w (elt words i)))
		   (if (>= k (length w))
		       0
		       (elt w k))))
	       (split (i j k)
		 (let ((pos nil)
		       (diff 99999))
		   (loop for p from (1+ i) to j do
			(when (not (eq (letter (1- p) k)
				       (letter p k)))
			  (let ((d (abs (- (- p i)
					   (1+ (- j p))))))
			    (when (< d diff)
			      (setf diff d)
			      (setf pos (1- p))))))
		   pos))
	       (generate (i j k y)
		 (if (= i j)
		     (let* ((id (gethash (elt words i) *word-ids*))
			    (prim (gethash id *id-meanings*)))
			 (label prim)
			 (LDA id (format nil "~a" prim))
			 (RTS))
		     (let ((split (split i j k)))
		       (if (null split)
			   (generate i j (1+ k) y)
			   (progn
			     (dc (format nil "; ~a...~a (~a) or ~a...~a (~a)?"
					 (elt words i)
					 (elt words split)
					 (1+ (- split i))
					 (elt words (1+ split))
					 (elt words j)
					 (- j split)))
			     (unless (= k y)
			       (LDY k)
			       (LDA.IZY :word)
			       (setf y k))
			     (CMP (1+ (- (char-code (letter split k))
					 (char-code #\A)))
				  (format nil "[~a] >= ~a" k (letter split k)))
			     (let ((label (write-to-string (incf label-number))))
			       (BCS label)
			       (generate i split k y)
			       (label label)
			       (generate (1+ split) j k y))))))))
	(LDY 0)
	(LDA.IZY :word)
	(generate 0 (1- (length words)) 0 0)))))

(defun build-parse-test (pass)
  (funcall pass)
  (build-symbol-table)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defun parse-test ()
  (reset-compiler)
  (reset-symbol-table)

  (reset-parser)
  
  (defword :PRESS :PUSH)
  (defword :GET :PICK :TAKE)
  (defword :OPEN)
  (defword :CLOSE :SHUT)
  (defword :SAY :SPEAK :TELL)
  (defword :UP :CLIMB)
  (defword :DOWN :DESCEND)
  (defword :OPERATE :USE)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   
	   (parser)
	   
	   (label :end)))
    
    (build #'pass))
  
  (monitor-reset #x600))
  ;(monitor-run)
  
  ;(setmem-copy (monitor-buffer)))

			 

	       
    
