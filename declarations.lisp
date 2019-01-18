;; Functions to define an actual game
;; Ideally the game would *only* see the functions defined here

;; TODO get rid of extra vm-done at the end of custom actions
;; TODO (goto label) is fine, but the label it jumps to is somewhat leaky as it is a
;; genuine assembler label. Probably fine.

(defparameter *donothave* "You don't have that.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-this (object-name)
    (if (eq 'this object-name)
	'*current-object*
	object-name))

  (defun translate-here (place)
    (if (eq 'here place)
	'*current-location*
	place)))

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

(defun move-object-fn (object place)
  "Set the place of the object to be a new place"
  (defplace place)
  (vm-mov object place))

(defmacro move-object (object place)
  `(move-object-fn ,(translate-this object)
		   ,(translate-here place)))

(defun set-act (font colour)
  (setf *act-font* font)
  (setf *act-colour* colour))

(defmacro with-location (location &body body)
  `(let ((*current-location* ,location))
     (with-namespace *current-location*
       ,@body)))

(defmacro location (name title img-file text &body body)
  (let ((name-sym (gensym)))
    `(let ((,name-sym ,name))
       (dloc ,name-sym ,title ,img-file ,text)
       (with-location ,name-sym
	 ,@body))))
       
;; TODO refactor these three functions

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
	      (vm-bra :end-if t)))
	  (progn
	    (assert else nil "Must have at least one clause")
	    (vm-bset bit :end-if)))
      (when else
	(when then (label :else))
	(with-namespace namespace
	  (funcall else)))
      (label :end-if))))

(defun if-in-place-fn (object place then else)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (if then
	  (progn
	    (vm-boop object place (if else :else :end-if))
	    (with-namespace namespace
	      (funcall then))
	    (when else    
	      (vm-bra :end-if t)))
	  (progn
	    (assert else nil "Must have at least one clause")
	    (vm-boip object place :end-if)))
      (when else
	(when then (label :else))
	(with-namespace namespace
	  (funcall else)))
      (label :end-if))))

(defun if-object-fn (object then else)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (vm-obj object)
      (if then
	  (progn
	    (vm-brf (if else :else :end-if))
	    (with-namespace namespace
	      (funcall then))
	    (when else    
	      (vm-bra :end-if t)))
	  (progn
	    (assert else nil "Must have at least one clause")
	    (vm-brt :end-if)))
      (when else
	(when then (label :else))
	(with-namespace namespace
	  (funcall else)))
      (label :end-if))))

(defmacro if-object (object then &optional (else nil else-supplied-p))
  `(if-object-fn ,(translate-this object)
	       #'(lambda () ,then)
	       (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro when-object (object &body then)
  `(if-object-fn ,object
	       #'(lambda () ,then)
	       nil))

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

(defmacro if-has (object-name then &optional (else nil else-supplied-p))
  `(if-in-place-fn ,(translate-this object-name) :inventory
		   #'(lambda () ,then)
		   (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro when-has (object-name &body body)
  `(if-in-place-fn ,(translate-this object-name) :inventory
		   #'(lambda () ,@body)
		   nil))

(defmacro if-in-place (object-name place then &optional (else nil else-supplied-p))
  `(if-in-place-fn ,(translate-this object-name) ,(translate-here place)
		   #'(lambda () ,then)
		   (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro when-in-place (object-name place &body then)
  `(if-in-place-fn ,(translate-this object-name) ,(translate-here place)
		   #'(lambda () ,@then)
		   nil))

(defmacro if-not-in-place (object-name place then &optional (else nil else-supplied-p))
  `(if-in-place-fn ,(translate-this object-name) ,(translate-here place)
		   (if ,else-supplied-p #'(lambda () ,else) nil)
		   #'(lambda () ,then)))

;;todo make justification work with the - at the beginning without actually
;;puting it into the string table
(defun justify-response (message messages)
  (when *compiler-final-pass*
    (warn-if-not-punctuated message)
    (dolist (str messages)
      (warn-if-not-punctuated str)))
  (let* ((text
	  ;;TODO this is an abuse of the justify function, which
	  ;;     is to account for the prompt
	  (justify-with-image (format nil "~a~{~%~a~}" message messages)
			      5 4 *act-font*))
	 (lines (1+ (count #\Newline text))))
    (assert (<= lines 3) nil
	    (format nil "Response would have more than 3 lines~%~a"
		    text))
    (values text lines)))

(defun respond (message &rest messages)
  "Respond with message or messages. Applies smart quotes"
  (multiple-value-bind (text lines)
      (justify-response (smart-quote message)
			(mapcar #'smart-quote messages))
    (if (gethash text *defined-strings*)
	  (let ((addr (gethash text *string-table*)))
	    (case lines
	      (1 (vm-pr1 addr text))
	      (2 (vm-pr2 addr text))
	      (3 (vm-pr3 addr text))))
	  (progn
	    (case lines
	      (1 (vm-pri1 text))
	      (2 (vm-pri2 text))
	      (3 (vm-pri3 text)))))))

(defun respond-raw (message &rest messages)
  "Raw code response function"
  (multiple-value-bind (text lines)
      (justify-response message messages)
    (LDY lines)
    (JSR :print-message)
    (dc (format nil "~a" text) t)
    (dw nil (dstr text))))

(defun navigate (location message)
  "Navigate, and show a message"
  (respond message)
  (vm-nav location))

(defun delegate-action ()
  "Call the generic handler for the current action, if it exists"
  (vm-del))

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
  (if vm (vm-done) (rts)))

(defmacro on-entry (&body body)
  "Define actions which happen when the player enters the location"
  `(progn
     (label :on-entry)
     ,@body
     (vm-done)))

(defmacro action (words &body body)
  "An action which is executed by the VM."
  `(action-fn t ,words #'(lambda () ,@body)))

(defmacro custom-action (words &body body)
  "An action which drops into 6502"
  `(action-fn nil ,words #'(lambda () ,@body)))

(defun verb-fn (vm object verbs fn)
  (assert object nil (format nil "~a has no object context" verbs))
  ;;Accept a single list of words, or lists of words
  (unless (listp verbs)
    (setf verbs (list verbs)))
  (let ((label (cons (first verbs) object)))
    (dolist (verb verbs)
      (unless (gethash (symbol-name verb) *word->meaning*)
	(defword verb))
      ;; all these verbs are handled at the same place for
      ;; this object
      (let ((entry (cons verb label)))
	(unless (find entry (gethash object *object->vtable*) :test 'equal)
	  (push (cons verb label) (gethash object *object->vtable*)))))
    (label object (first verbs))
    (unless vm (vm-exe))
    (funcall fn)
    (if vm (vm-done) (rts))))

(defmacro verb (verb-or-verbs &body body)
  "An action for a verb associated with this object"
  `(verb-fn t *current-object* ,verb-or-verbs #'(lambda () ,@body)))

(defmacro it (object)
  "Set the it object, for instance if it was mentioned explicitly."
  `(vm-it ,(translate-this object)))

(defun goto (label)
  (vm-jmp label))

(defmacro ensure-has (object)
  `(if-has ,object
	   nil
	   (progn
	     (respond *donothave*)
	     (vm-done))))
