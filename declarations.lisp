;; Here are the functions which will be used to declare the game data
;; TODO the S flag optimization in the bit check doesn't really work
;; as almost everything affects the S flag.

(defparameter *bits* nil)

(defun reset-bits ()
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

(defun bit-table ()
  (let ((bits nil))
    (maphash #'(lambda (bit initially-set)
		 (push (list
			(format nil "~a" (if (consp (car bit)) ""))
			bit
			(if (eq initially-set :not-specified) nil initially-set))
		       bits))
	     *bits*)
    (let ((ns nil))
      (game-state-bits "Bit Table"
	(dolist (bit (sort bits #'string< :key #'car))
	  (when (not (equal ns (first bit)))
	    (setf ns (first bit))
	    (dc (format nil "Bits for ~a" ns)))
	  (zp-b (second bit) (if (third bit) #xff #x00)))))))

(defun defbits (initially-set &rest bits)
  (dolist (bit bits)
    (defbit bit :initially-set initially-set)))

(defun setbit (bit &optional (set t))
  (defbit bit)
  (if set
      (progn
	(SEC) ;;set bit 7
	(ROR.* bit))
      (LSR.* bit))) ;;clear bit 7

(defun clrbit (bit)
  (setbit bit nil))

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
      (dc (format nil "~a ~a" (if then "IF" "IF NOT") bit) t)
      (BIT.* bit)
      (if then
	(progn
	  (BPL (if else :else :endif))
	  (with-namespace namespace
	    (funcall then))
	  (when else
	    (JMP :endif)
	    (label :else)))
	(progn
	  (assert else nil "At least one clause must be specified")
	  (BMI :endif)))
      (when else
	(with-namespace namespace
	  (funcall else)))
      (label :endif))))

(defmacro if-bit (bit then &optional (else nil else-supplied-p))
  `(if-bit-fn ,bit
	     #'(lambda () ,then)
	     (if ,else-supplied-p #'(lambda () ,else) nil)))

(defmacro if-not-bit (bit then &optional (else nil else-supplied-p))
  `(if-bit-fn ,bit
	     (if ,else-supplied-p #'(lambda () ,else) nil)
	     #'(lambda () ,then)))

;;Note the style of the following if-then-else macro. Decomposing
;;like this seems to make the code easier to read, i.e. minimising
;;the amount of work explicitly inside the macro.

(defun if-in-place-fn (fn object-name place)
  (defplace place)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (LDA (place-id place) (format nil "~a" place))
      (CMP.AB (object-place-address object-name) (format nil "Place of ~a" object-name))
      (BNE :endif)
      (with-namespace namespace
	(funcall fn))
      (label :endif))))

(defun if-else-in-place-fn (fn else-fn object-name place)
  (defplace place)
  (let ((namespace *compiler-label-namespace*))
    (with-local-namespace
      (LDA.AB (object-place-address object-name) (format nil "Place of ~a" object-name))
      (CMP (place-id place) (format nil "~a" place))
      (BNE :else)
      (let ((then-addr *compiler-ptr*))
	(with-namespace namespace
	  (funcall fn))
	(unless (= (peek-last-byte) #x60) ;;RTS
	  ;;Rather than checking for the Z flag (i.e. equal)
	  ;;check for the carry- it will definitely be set before
	  ;;the application of fn, so we can use it to make a short
	  ;;branch. Z flag is also definitely set, but is changed
	  ;;by almost every operation, so the optimisation would likely
	  ;;never be applied
	  (if (code-affects-flag 'C then-addr (1- *compiler-ptr*))
	      (JMP :endif)
	      (BCS :endif))))
      (label :else)
      (with-namespace namespace
	(funcall else-fn))
      (label :endif))))

(defmacro if-in-place (object-name place then &optional (else nil else-supplied-p))
  (if else-supplied-p
      `(if-else-in-place-fn #'(lambda () ,then)
			    #'(lambda () ,else)
			    ,object-name ,place)
      `(if-in-place-fn #'(lambda () ,then) ,object-name ,place)))


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
	    (format nil "Response would have more than 3 lines~%~a"
		    text))
    
    (JSR (cons :print-message lines))
    (dc (format nil "~a" text) t)
    (dw nil (dstr text))))

;;define a action handler for a sentence
(defun words2label (words)
  (string-right-trim "-" (format nil "~{~a-~}" words)))

(defun action-fn (words fn)
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
  (funcall fn)
  (label :rts)
  (RTS))

(defmacro action (words &body body)
  `(action-fn ,words #'(lambda () ,@body)))

(defun test-if-bit-fn (is-set expected test-fn)

  (reset-compiler)

  (reset-bits)
  
  (flet ((src ()
	   (org #x600)

	   (JSR :start)
	   (JMP :finish)
	   
	   (label :start)	   

	   (LDA 42)

	   (funcall test-fn)

	   (label :finish)
	   
	   (STA.AB :output)
	   
	   (BRK)
	   
	   (db :bit (if is-set #xff 0))
	   (db :output 0)
	   
	   (label :end)
	   
	   ))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))

  ;;(disassemble-6502 :start :end)
  
  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (aref buf (resolve :output)) expected))))

(defmacro test-if-bit (is-set expected &body body)
  `(test-if-bit-fn ,is-set ,expected #'(lambda () ,@body)))

(test-if-bit t 5 (if-bit :bit (LDA 5) (LDA 6)))
(test-if-bit nil 6 (if-bit :bit (LDA 5) (LDA 6)))
(test-if-bit t 5	(if-bit :bit (LDA 5)))
(test-if-bit nil 42 (if-bit :bit (LDA 5)))

;;true, has return

(test-if-bit t 5	(if-bit :bit
				(progn (LDA 5)
				       (RTS))
				(progn (LDA 6)
				       (RTS))))

;;false, has return

(test-if-bit nil 6 (if-bit :bit
			   (progn (LDA 5)
				  (RTS))
			   (progn (LDA 6)
				  (RTS))))

;;test zpg true / false, note override of parameter zpgbit

(test-if-bit nil 5
  (zp-b :zpgbit)
  (LDA #xFF)
  (STA.ZP :zpgbit)
  (if-bit :zpgbit
	  (progn
	    (LDA 4)
	    (CLC)
	    (ADC 1))
	  (LDA 55)))

(test-if-bit nil 55
  (zp-b :zpgbit)
  (LDA.ZP #x00)
  (STA.ZP :zpgbit)
  (if-bit :zpgbit
	  (progn
	    (LDA 4)
	    (CLC)
	    (ADC 1))
	  (LDA 55)))

(test-if-bit nil 5 (if-not-bit :bit (LDA 5)))
(test-if-bit t 42 (if-not-bit :bit (LDA 5)))
(test-if-bit nil 5 (if-not-bit :bit (LDA 5) (LDA 6)))
(test-if-bit t 6 (if-not-bit :bit (LDA 5) (LDA 6)))

;; test if-in-place

(defun reset-test-data ()

  (reset-compiler)
  (reset-symbol-table)
  (reset-bits)
  (reset-parser)
  (reset-object-model)

  (defplace :crack)
  (defplace :mountain)
  
  (defobject "KEY" "Some key or other" :initial-place :crack)
  (defobject "HORSE" "Deer" :initial-place :mountain))

(defun test-if-in-place-fn (expected test-fn)

  (reset-test-data)
  
  (flet ((src ()

	   (zeropage)
	   
	   (org #x600)

	   (JSR :start)   
	   (JMP :finish)   ;;clause can terminate with RTS, and
	    ;;will go through same path as if it hadn't
	      
	   (label :start)

	   (LDX 42)

	   (funcall test-fn)

	   (label :finish)
	   (STX.AB :output)
	      
	   (BRK)
	   
	   (db :output 0)

	   (object-table)
	   (string-table)
	   
	   (label :end)))
    (reset-compiler)
    (src)
    (build-symbol-table)
    ;;TODO This is BORING
    (setf *word-table-built* t)
    (src)
    (setf *compiler-final-pass* t)
    (src))

  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (aref buf (resolve :output)) expected))))

(defmacro test-if-in-place (expected &body body)
  `(test-if-in-place-fn ,expected (lambda () ,@body)))

(reset-test-data)

;;Relying on X not being modified by the executed code

;; Single clause

(test-if-in-place 1 (if-in-place "KEY" :crack (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :nowhere (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :inventory (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :mountain (LDX 1)))

(test-if-in-place 42 (if-in-place "HORSE" :crack (LDX 1)))
(test-if-in-place 42 (if-in-place "HORSE" :nowhere (LDX 1)))
(test-if-in-place 42 (if-in-place "HORSE" :inventory (LDX 1)))
(test-if-in-place 1 (if-in-place "HORSE" :mountain (LDX 1)))

(test-if-in-place 1 (if-in-place "KEY" :crack
				 (progn (LDX 1) (label :then-end nil))
				 (LDX 2)))

;; Dual clause

(assert (= #xB0 (peek-byte (resolve :then-end)))) ;;BCS optimization

(test-if-in-place 2 (if-in-place "KEY" :nowhere (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "KEY" :inventory (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "KEY" :mountain (LDX 1) (LDX 2)))

(test-if-in-place 2 (if-in-place "HORSE" :crack (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "HORSE" :nowhere (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "HORSE" :inventory (LDX 1) (LDX 2)))
(test-if-in-place 1 (if-in-place "HORSE" :mountain (LDX 1) (LDX 2)))

(test-if-in-place 2 (if-in-place "HORSE" :crack
		      (progn (CLC) (LDX 1) (label :then-end nil))
		      (LDX 2)))

(assert (= #x4C (peek-byte (resolve :then-end)))) ;;Uses JMP

(test-if-in-place 2 (if-in-place "HORSE" :nowhere (progn (CLC) (LDX 1)) (LDX 2)))
(test-if-in-place 2 (if-in-place "HORSE" :inventory (progn (CLC) (LDX 1)) (LDX 2)))
(test-if-in-place 1 (if-in-place "HORSE" :mountain (progn (CLC) (LDX 1)) (LDX 2)))

;; Return in true clause

(test-if-in-place 2 (if-in-place "HORSE" :crack
		      (progn (LDX 1) (RTS) (label :then-end nil))
		      (LDX 2)))

(assert (= #xA2 (peek-byte (resolve :then-end)))) ;;Goes straight to LDX- no jump

(test-if-in-place 1 (if-in-place "HORSE" :mountain
		      (progn (LDX 1) (RTS) (label :then-end nil))
		      (LDX 2)))

(assert (= #xA2 (peek-byte (resolve :then-end)))) ;;Goes straight to LDX- no jump
