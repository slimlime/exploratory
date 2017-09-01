(defun nil->0 (value)
  (if value value 0))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


	       
