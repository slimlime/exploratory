;; Here are the functions which will be used to declare the game data

(defparameter *bits* nil)

(defun reset-bits ()
  (setf *bits* (make-hash-table :test 'equal)))

(defun defbit (bit initially-set &optional (namespace *current-location*))
  (setf (gethash (cons namespace bit) *bits*) initially-set))

(defun bit-table ()
  (dc "Bit table")
  (let ((bits nil))
  (maphash #'(lambda (bit initially-set)
	       (push (list
		      (format nil "~a" (if (consp (car bit)) ""))
		      bit
		      initially-set)
		     bits))
	   *bits*)
  (let ((ns nil))
    (dolist (bit (sort bits #'string< :key #'car))
      (when (not (equal ns (first bit)))
	(setf ns (first bit))
	(dc (format nil "Bits for ~a" ns)))
      (zp-b (second bit) (if (third bit) #xff #x00))))))
      
      ;;(db (second bit) (if (third bit) #xff #x00))))))

(defun defbits (initially-set &rest bits)
  (dolist (bit bits)
    (defbit bit initially-set)))

(defun setbit (bit &optional (set t))
  (if set
      (progn
	(SEC)
	(ROR.* bit))
      (LSR.* bit)))

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

(defmacro ifbit (bit then &optional (else nil else-supplied-p))
  (let ((bitsym (gensym))
	(then-addr (gensym)))
    `(let ((namespace *compiler-label-namespace*))
       (let ((,bitsym ,bit))
	 (with-local-namespace
	   (dc (format nil "IF ~a" ,bitsym) t)
	   (BIT.* ,bitsym)
	   (BPL ,(if else-supplied-p :else :endif))
	   (let ((,then-addr *compiler-ptr*))
	     (declare (ignorable ,then-addr))
	     (with-namespace namespace
	       ,then)
	     ,(when else-supplied-p
		    `(progn
		       ;;This optimisation omits the branch
		       ;;if there is an RTS immediately preceeding
		       ;;Also, uses a branch rather than a JMP
		       ;;if it can prove the S flag was not affected
		       
		       (unless (= (peek-last-byte) #x60) ;;RTS
			 (if (code-affects-flag 'S ,then-addr (1- *compiler-ptr*))
			     (JMP :endif)
			     (BMI :endif)))
		       (label :else)
		       (with-namespace namespace
			 ,else)))
	     (label :endif)))))))

(defmacro nifbit (bit then &optional (else nil else-supplied-p))
  (if else-supplied-p
      `(ifbit ,bit ,else ,then)
      (let ((bitsym (gensym)))
	`(let ((namespace *compiler-label-namespace*))
	   (let ((,bitsym ,bit))
	     (with-local-namespace
	       (format nil "NIF ~a" ,bitsym)
	       (BIT.* ,bitsym)
	       (BMI :endif)
	       (with-namespace namespace
		 ,then)
	       (label :endif)))))))

;;Note the style of the following if-then-else macro. Decomposing
;;like this seems to make the code easier to read, i.e. minimising
;;the amount of work explicitly inside the macro.

(defun if-in-place-fn (fn object-name place)
  (with-namespace :if-in-place
    (LDA (place-id place) (format nil "~a" place))
    (CMP.AB (object-place-address object-name) (format nil "Place of ~a" object-name))
    (BNE :end-if)
    (funcall fn)
    (label :end-if)))

(defun if-else-in-place-fn (fn else-fn object-name place)
  (with-namespace :if-in-place
    (LDA (place-id place) (format nil "~a" place))
    (CMP.AB (object-place-address object-name) (format nil "Place of ~a" object-name))
    (BNE :else)
    (let ((then-addr *compiler-ptr*))
      (funcall fn)
      (unless (= (peek-last-byte) #x60) ;;RTS
	;;Rather than checking for the Z flag (i.e. equal)
	;;check for the carry- it will definitely be set before
	;;the application of fn, so we can use it to make a short
	;;branch. Z flag is also definitely set, but is changed
	;;by almost every operation, so the optimisation would likely
	;;never be applied
	(if (code-affects-flag 'C then-addr (1- *compiler-ptr*))
	    (JMP :end-if)
	    (BCS :end-if))))
    (label :else)
    (funcall else-fn)
    (label :end-if)))

(defmacro if-in-place (object-name place then &optional (else nil else-supplied-p))
  (if else-supplied-p
      `(if-else-in-place-fn #'(lambda () ,then)
			    #'(lambda () ,else)
			    ,object-name ,place)
      `(if-in-place-fn #'(lambda () ,then) ,object-name ,place)))

(defun ends-with-punctuation (string)
  (let ((char (char string (1- (length string)))))
    (or (char= char #\.)
	(char= char #\!)
	(char= char #\?))))

;;todo make justification work with the - at the beginning without actually
;;puting it into the string table
(defun respond (message &rest messages)
  (when *compiler-final-pass*
    (dolist (s (cons message messages))
      (unless (ends-with-punctuation s)
	(format t "WARNING string '~a' does not end with punctuation~%" s))))
  
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

(defun test-ifbit (is-set expected test-fn)

  (reset-compiler)
     
  (flet ((src ()
	   (org #x600)
	      
	   (label :start)

	   (LDA 42)

	   (funcall test-fn)
	      
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

;;true clause does not affect V

(test-ifbit t 5
	    #'(lambda ()
		(ifbit :bit
		       (LDA 5)
		       (LDA 6))))

;;true clause does affect V

(test-ifbit t 5
	    #'(lambda ()
		(ifbit :bit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1))
		       (LDA 6))))

;;false, true clause does not affect V

(test-ifbit nil 6
	    #'(lambda ()
		(ifbit :bit
		       (LDA 5)
		       (LDA 6))))

;;false, true clause does affect V

(test-ifbit NIL 6
	    #'(lambda ()
		(ifbit :bit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1))
		       (LDA 6))))

;;true, does not affect V, missing else

(test-ifbit t 5
	    #'(lambda ()
		(ifbit :bit
		       (LDA 5))))

;;false, does not affect V, missing else

(test-ifbit nil 42
	    #'(lambda ()
		(ifbit :bit
		       (LDA 5))))

;;true, affects V, missing else

(test-ifbit t 5
	    #'(lambda ()
		(ifbit :bit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1)))))

;;false, affects V, missing else

(test-ifbit nil 42
	    #'(lambda ()
		(ifbit :bit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1)))))

;;true, has return

(test-ifbit t 5
	    #'(lambda ()
		(JSR :test)
		(JMP :test-end)
		(label :test)
		(ifbit :bit
		       (progn (LDA 5)
			      (RTS))
		       (progn (LDA 6)
			      (RTS)))
		(label :test-end)))

;;false, has return

(test-ifbit nil 6
	    #'(lambda ()
		(JSR :test)
		(JMP :test-end)
		(label :test)
		(ifbit :bit
		       (progn (LDA 5)
			      (RTS))
		       (progn (LDA 6)
			      (RTS)))
		(label :test-end)))

;;test zpg true / false, note override of parameter zpgbit

(test-ifbit nil 5
	    #'(lambda ()
		(zp-b :zpgbit)
		(LDA #xFF)
		(STA.ZP :zpgbit)
		(ifbit :zpgbit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1))
		       (LDA 55))))

(test-ifbit nil 55
	    #'(lambda ()
		(zp-b :zpgbit)
		(LDA.ZP #x00)
		(STA.ZP :zpgbit)
		(ifbit :zpgbit
		       (progn
			 (LDA 4)
			 (CLC)
			 (ADC 1))
		       (LDA 55))))

;; test nifbit

(test-ifbit nil 5
	     #'(lambda ()
		 (nifbit :bit
			 (LDA 5))))
(test-ifbit t 42
	     #'(lambda ()
		 (nifbit :bit
			 (LDA 5))))

(test-ifbit nil 5
	     #'(lambda ()
		 (nifbit :bit
			 (LDA 5)
			 (LDA 6))))
(test-ifbit t 6
	     #'(lambda ()
		 (nifbit :bit
			 (LDA 5)
			 (LDA 6))))

;; test if-in-place

(defun test-if-in-place-fn (expected test-fn)

  (reset-compiler)
  (reset-object-model)

  (defplace :crack)
  (defplace :mountain)
  
  (defobject "KEY" "Some key or other" :initial-place :crack)
  (defobject "HORSE" "Deer" :initial-place :mountain)

  (flet ((src ()

	   (zeropage)
	   
	   (org #x600)
	      
	   (label :start)

	   (LDX 42)

	   (funcall test-fn)
	      
	   (STX.AB :output)
	      
	   (BRK)
	   
	   (db :output 0)

	   (object-table)
	      
	   (label :end)

	   ))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))

  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (aref buf (resolve :output)) expected))))

(defmacro test-if-in-place (expected &body body)
  `(test-if-in-place-fn ,expected (lambda () ,@body)))

;;Relying on X not being modified by the executed code

(test-if-in-place 1 (if-in-place "KEY" :crack (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :elsewhere (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :inventory (LDX 1)))
(test-if-in-place 42 (if-in-place "KEY" :mountain (LDX 1)))

(test-if-in-place 42 (if-in-place "HORSE" :crack (LDX 1)))
(test-if-in-place 42 (if-in-place "HORSE" :elsewhere (LDX 1)))
(test-if-in-place 42 (if-in-place "HORSE" :inventory (LDX 1)))
(test-if-in-place 1 (if-in-place "HORSE" :mountain (LDX 1)))

(test-if-in-place 1 (if-in-place "KEY" :crack (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "KEY" :elsewhere (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "KEY" :inventory (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "KEY" :mountain (LDX 1) (LDX 2)))

(test-if-in-place 2 (if-in-place "HORSE" :crack (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "HORSE" :elsewhere (LDX 1) (LDX 2)))
(test-if-in-place 2 (if-in-place "HORSE" :inventory (LDX 1) (LDX 2)))
(test-if-in-place 1 (if-in-place "HORSE" :mountain (LDX 1) (LDX 2)))
