(defparameter *bits* nil)
(defparameter *bit->index* nil)

(defparameter *bits-read* nil)
(defparameter *bits-modified* nil)

(defun reset-bits ()
  (setf *bit->index* (make-hash-table :test 'equal))
  (setf *bits* (make-hash-table :test 'equal))
  (setf *bits-read* (make-hash-table :test 'equal))
  (setf *bits-modified* (make-hash-table :test 'equal)))

(defun bit-modified (bit)
  (unless (consp bit)
    (setf bit (cons here bit)))
  (setf (gethash bit *bits-modified*) t))

(defun bit-read (bit)
  (unless (consp bit)
    (setf bit (cons here bit)))
  (setf (gethash bit *bits-read*) t))

(defun defbit (bit &key (initially-set :not-specified initially-set-supplied-p)
		     (namespace here))
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

(defun unique-bit-index (bit)
  "Find a bit, without having to specify the location. Asserts if
there is not matching bit, or if there are duplicates. Fully specified
bits will only match at the location they are defined."
  (aif (gethash bit *bit->index*)
       it
       (let ((indexes nil))
	 (do-hashtable (bit-key index *bit->index*)
	   (when (equal (cdr bit-key) bit)
	     (push index indexes)))
	 (assert (= (length indexes) 1) nil "Could not resolve bit '~a', matches ~a"
		 bit
		 indexes)
	 (car indexes))))

(defun bit-value (bit)
  (let* ((index (unique-bit-index bit))
	 (bit-mask (ash 1 (logand index #x7)))
	 (bit-offset (ash index -3)))
    (not (zerop (logand (monitor-peek (+ (resolve :bit-table) bit-offset))
			bit-mask)))))

(defun assert-bits (bits set)
  (dolist (bit bits)
    (assert (eq (if set t nil) (bit-value bit)) nil
	    "Expected bit '~a' to be ~a but was ~a"
	    bit
	    set
	    (bit-value bit))))

(defun bit-index (bit)
  (unless (consp bit)
    (setf bit (cons here bit)))
  (aif (gethash bit *bit->index*)
       it
       (progn
	 (when *compiler-final-pass*
	   (assert it nil "Bit ~a was not found in bit table" bit))
	 0)))

;;Bit packed table
(defun bit-table ()
  (when *compiler-final-pass*
    (do-hash-keys (bit *bits-read*)
      (unless (gethash bit *bits-modified*)
	(format t "WARNING Bit ~a is read, but never modified.~%" bit)))
    (do-hash-keys (bit *bits-modified*)
      (unless (gethash bit *bits-read*)
	(format t "WARNING Bit ~a is modified, but never read.~%" bit))))
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
