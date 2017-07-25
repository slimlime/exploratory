;; Here are the functions which will be used to declare the game data

(defparameter *current-location* nil)
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
      (db (second bit) (if (third bit) #xff #x00))))))

(defun defbits (initially-set &rest bits)
  (dolist (bit bits)
    (defbit bit initially-set)))

(defun setbit (bit &optional set)
  (if set
      (progn
	(LDA #xFF)
	;;todo this is very susceptible to code analysis...
	;;it is entirely possible it may be already set
	;;to the value we want
	(STA.AB bit))
      (progn
	(LDA #x00)
	(STA.AB bit))))

(defun clrbit (bit)
  (setbit bit nil))

(defmacro with-location (location &body body)
  `(let ((*current-location* ,location))
     (with-namespace *current-location*
       ,@body)))

(defmacro ifbit (bit then &optional (else nil else-supplied-p))
  (let ((bitsym (gensym))
	(then-addr (gensym)))
    `(let ((namespace *compiler-label-namespace*))
       (let ((,bitsym ,bit))
	 (with-local-namespace (format nil "IF ~a" ,bitsym)
	   (if (resolves-to-zpg ,bitsym)
	       (BIT.ZP ,bitsym)
	       (BIT.AB ,bitsym))
	   (BVC ,(if else-supplied-p :else :endif))
	   (let ((,then-addr *compiler-ptr*))
	     (declare (ignorable ,then-addr))
	     (with-namespace namespace
	       ,then)
	     ,(when else-supplied-p
		 `(progn
		    (unless (= (peek-last-byte) #x60) ;;RTS
		      (if (code-affects-flag 'V ,then-addr (1- *compiler-ptr*))
			  (progn
			    (CLV)
			    (BVC :endif))
			  (BVS :endif)))
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
	     (with-local-namespace (format nil "NIF ~a" ,bitsym)
	       (if (resolves-to-zpg ,bitsym)
		   (BIT.ZP ,bitsym)
		   (BIT.AB ,bitsym))
	       (BVS :endif)
	       (with-namespace namespace
		 ,then)
	       (label :endif)))))))

(defun respond-1 (msg)
  ;;todo check for strings that don't fit the screen width
  (JSR :print-message)
  (dc (format nil "-> ~a" msg) t)
  (dw nil (if (stringp msg)
	      (dstr msg)
	      msg)))

;;Display upto 4 lines of message
(defun respond (msg1 &optional msg2 msg3 msg4)
  (respond-1 msg1)
  (when msg2 (respond-1 msg2))
  (when msg3 (respond-1 msg3))
  (when msg4 (respond-1 msg4)))

;;define a handler for the provided sentence
(defmacro action (words &body body)
  (let ((words-sym (gensym)))
    `(progn
       (let ((,words-sym ,words))
	 (dc (format nil "~a -> ~a" ,words-sym ',body))
	 (defsentence ,words-sym (format nil "~a" ,words-sym) *current-location*)
	 ,@body))))

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