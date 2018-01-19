(defparameter *bits* nil)
(defparameter *bit->index* nil)

(defun reset-bits ()
  (setf *bit->index* (make-hash-table :test 'equal))
  (setf *bits* (make-hash-table :test 'equal)))

(defun defbit (bit &key (initially-set :not-specified initially-set-supplied-p)
		     (namespace *current-location*))
  (let* ((key (cons namespace bit))
	 (previous-state (gethash key *bits*)))
    ;; It's ok to do defbit multiple times, but we
    ;; fail if we supply an 'initially-set' paramater
    ;; which differs.
    (if previous-state
	(when initially-set-supplied-p
	  (unless (eq previous-state :not-specified)
	    (assert (eq initially-set previous-state)
		    nil
		    (format nil "Bit ~a was previously defined to be initially ~a, redefined to be ~a" bit previous-state initially-set))))
	(setf (gethash (cons namespace bit) *bits*) initially-set))))

(defun dump-bits ()
  (maphash #'(lambda (k v)
	       (if (consp k)
		   (format t "~a ~a:~a~%" v (car k) (cdr k))
		   (format t "~a ~a~%" v k)))
	   *bit->index*))

(defun bit-index (bit)
  (unless (consp bit)
    (setf bit (cons *current-location* bit)))
  (aif (gethash bit *bit->index*)
       it
       (progn
	 (when *compiler-final-pass*
	   (assert it nil "Bit ~a was not found in bit table" bit))
	 0)))

;;Bit packed table
(defun bit-table ()
  (let ((bits nil))
    (maphash #'(lambda (bit initially-set)
		 (push (list
			bit
			(if (eq initially-set :not-specified) nil initially-set)
			(format nil "~a:~a" (car bit) (cdr bit)))
		       bits))
	     *bits*)
    (let ((count 0)
	  (mask 1)
	  (byte 0)
	  (comment nil))
      (game-state-bytes "Bit Table" 
	(label :bit-table)
	(dolist (bit (sort bits #'string< :key #'third))
	  (when (second bit)
	    (setf byte (logior mask byte)))
	  (push (format nil "~a=~a" count (first bit)) comment)
	  (setf (gethash (first bit) *bit->index*) count)
	  (incf count)
	  (setf mask (* 2 mask))
	  (when (= mask 256)
	    (dc (format nil "~a" comment) t)
	    (db nil byte)
	    (setf byte 0)
	    (setf mask 1)
	    (setf comment nil)))
	(when (/= mask 0)
	  (dc (format nil "~a" comment) t)
	  (db nil byte))))))
