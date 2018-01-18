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
	  
(defun defbits (initially-set &rest bits)
  (dolist (bit bits)
    (defbit bit :initially-set initially-set)))

(defun setbit (bit &optional (set t))
  (defbit bit)
  (if set
      (vm-set bit)
      (vm-clr bit)))

(defun clrbit (bit)
  (defbit bit)
  (vm-clr bit))

(defun move-object (object place)
  "Set the place of the object to be a new place"
  (defplace place)
  (vm-mov object place))

(defun set-act (font colour)
  (setf *act-font* font)
  (setf *act-colour* colour))

(defmacro with-location (location &body body)
  `(let ((*current-location* ,location))
     (measure-size *current-location*
       (with-namespace *current-location*
	 ,@body))))

(defun if-bit-fn (bit then else)
  (defbit bit)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (if then
	  (progn
	    (vm-bclr bit (if else :else :end-if))
	    (with-namespace namespace
	      (funcall then))
	    (when else
	      (vm-bra :end-if)))
	  (progn
	    (assert else nil "Must have at least one clause")
	    (vm-bset bit :end-if)))
      (when else
	(when then (label :else))
	(with-namespace namespace
	  (funcall else)))
      (label :end-if))))

(defmacro if-bit (bit then &optional (else nil else-supplied-p))
  `(if-bit-fn ,bit
	     #'(lambda () ,then)
	     (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro when-bit (bit &body then)
  `(if-bit-fn ,bit
	      #'(lambda () ,@then)
	      nil))

(defmacro unless-bit (bit &body then)
  `(if-bit-fn ,bit
	      nil
	      #'(lambda () ,@then)))

(defmacro if-not-bit (bit then &optional (else nil else-supplied-p))
  `(if-bit-fn ,bit
	     (if ,else-supplied-p #'(lambda () ,else) nil)
	     #'(lambda () ,then)))

;;; Can be refactored with the if bit- the difference is the vm-ops
(defun if-in-place-fn (object place then else)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (if then
	  (progn
	    (vm-boop object place (if else :else :end-if))
	    (with-namespace namespace
	      (funcall then)))
	  (progn
	    (assert else nil "Must have at least one clause")
	    (vm-boip object place :end-if)))
      (when else
	(when then (label :else))
	(with-namespace namespace
	  (funcall else)))
      (label :end-if))))

(defmacro if-in-place (object-name place then &optional (else nil else-supplied-p))
  `(if-in-place-fn ,object-name ,place
		   #'(lambda () ,then)
		   (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro if-not-in-place (object-name place then &optional (else nil else-supplied-p))
  `(if-in-place-fn ,object-name ,place
		   (if ,else-supplied-p #'(lambda () ,else) nil)
		   #'(lambda () ,then)))

;;todo make justification work with the - at the beginning without actually
;;puting it into the string table
(defun respond (message &rest messages)
  (when *compiler-final-pass*
    (dolist (s (cons message messages))
      (warn-if-not-punctuated s)))
  (let* ((text
	  ;;TODO this is an abuse of the justify function, which
	  ;;     is to account for the prompt
	  (justify-with-image (format nil "~a~{~%~a~}" message messages)
			      5 4 *act-font*))
	 (lines (1+ (count #\Newline text))))
    (assert (<= lines 3) nil
	    (format nil "Response would have more than 3 lines~%~a" text))
    (dc text t)
    (let ((str (dstr text)))
	  (case lines
	    (1 (vm-pr1 str))
	    (2 (vm-pr2 str))
	    (3 (vm-pr3 str))))))

(defun navigate (location)
  (vm-nav location))

;;define a action handler for a sentence
(defun words2label (words)
  (string-right-trim "-" (format nil "~{~a-~}" words)))

(defun action-fn (vm words fn)
  ;;Accept a single list of words, or lists of words
  (unless (listp (car words))
    (setf words (list words)))
  (dolist (sentence words)
    (dc (format nil "ON ~{~a ~}" sentence))
    (let ((label (words2label sentence)))
      (defsentence sentence
	  (cons *current-location* label)
	*current-location*)
      (label label *current-location*)))
  (unless vm
    (vm-exe))
  (funcall fn)
  (label :rts)
  (if vm (vm-done) (rts)))

(defmacro action (words &body body)
  "An action which is executed by the VM"
  `(action-fn t ,words #'(lambda () ,@body)))

(defmacro custom-action (words &body body)
  "An action which drops directly into 6502"
  `(action-fn nil ,words #'(lambda () ,@body)))
