(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-always
 (defun lambda-reader (stream char)
   (declare (ignore stream char))
   'lambda)
 (set-macro-character #\λ #'lambda-reader))

(defun nil->0 (value)
  (if value value 0))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro do-hashtable ((k v table) &body body)
  `(maphash (λ (,k ,v) ,@body) ,table))

(defmacro do-hash-keys ((k table) &body body)
  (let ((v (gensym)))
    `(maphash (λ (,k ,v) (declare (ignore ,v)) ,@body) ,table)))

(defmacro do-hash-values ((v table) &body body)
  (let ((k (gensym)))
    `(maphash (λ (,k ,v) (declare (ignore ,k)) ,@body) ,table)))

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

(defparameter *failing-tests* (make-hash-table :test 'equal))
(defparameter *warning-tests* (make-hash-table :test 'equal))

(defun test-fn (description body-fn &optional (deferred nil))
  (let ((msg nil))
    (handler-case
	(funcall body-fn)
      (error (e)
	(setf msg (format nil "~a FAILED~%~t'~a'" description e))))
    (when (and (null msg) deferred)
      (setf msg (format nil "The deferred test '~a' actually passed. Undefer it." description))
      (setf deferred nil))
    (when msg
      (format t "~a: ~a~%"
	      (if deferred "WARNING:" "ERROR :")
	      msg)
      (setf (gethash description (if deferred
				     *warning-tests*
				     *failing-tests*))
	    msg))
    (values)))

(defmacro test (description &body body)
  `(test-fn ,description (λ () ,@body)))

(defmacro deferred-test (description &body body)
  `(test-fn ,description (λ () ,@body) t))

(defun clear-test-results ()
  (clrhash *failing-tests*))

(defun failing-tests ()
  (hash-table-count *failing-tests*))
