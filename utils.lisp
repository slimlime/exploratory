(defun nil->0 (value)
  (if value value 0))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro do-hashtable ((k v table) &body body)
  `(maphash #'(lambda (,k ,v) ,@body) ,table))

(defmacro do-hash-keys ((k table) &body body)
  (let ((v (gensym)))
    `(maphash #'(lambda (,k ,v) (declare (ignore ,v)) ,@body) ,table)))

(defmacro do-hash-values ((v table) &body body)
  (let ((k (gensym)))
    `(maphash #'(lambda (,k ,v) (declare (ignore ,k)) ,@body) ,table)))

(defun hash-values (table)
  (let ((values nil))
    (do-hash-values (v table)
      (push v values))
    values))

(defun hash-keys (table)
  (let ((keys nil))
    (do-hash-keys (k table)
      (push k keys))
    keys))

(defmacro not-nil (value)
  `(aif ,value
	it
	(assert nil nil ,(format nil "~s was nil" value))))
