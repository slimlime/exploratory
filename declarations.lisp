;; Here are the functions which will be used to declare the game data

(defparameter *current-location* nil)

(defmacro with-location (location &body body)
  `(let ((*current-location* ,location))
     ,@body))

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

  (disassemble-6502 :start :end)
  
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
