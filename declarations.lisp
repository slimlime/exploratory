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
	   (BIT.AB ,bitsym)
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
  
(defun response (input message &key (location *current-location*))
  (assert location)
  (let* ((label (list location input message))
	 (string-label (cons :string label)))
    (dc (format nil "In ~a, ~a -> ~a" location input message))
    (label label)
    (sta16.zp string-label :live-message-addr)
    (JMP :live-message)
    (dcs string-label message)
    (defsentence input label location)))

;;conditional response

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
