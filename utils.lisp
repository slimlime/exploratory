(defun nil->0 (value)
  (if value value 0))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro dohashtable ((k v table) &body body)
  `(maphash #'(lambda (,k ,v) ,@body) ,table))

(defmacro dohashkeys ((k table) &body body)
  (let ((v (gensym)))
    `(maphash #'(lambda (,k ,v) (declare (ignore ,v)) ,@body) ,table)))

(defmacro dohashvalues ((v table) &body body)
  (let ((k (gensym)))
    `(maphash #'(lambda (,k ,v) (declare (ignore ,k)) ,@body) ,table)))


	       
