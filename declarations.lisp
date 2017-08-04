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

;;todo make justification work with the - at the beginning without actually
;;puting it into the string table
(defun respond (message &rest messages)
  (let* ((text
	  (justify-with-image (format nil "~a~{~%~a~}" message messages)
			      4 200 *act-font*))
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

(defmacro action (words &body body)
  (let ((words-sym (gensym)))
    `(progn
       (let ((,words-sym ,words))
	 (dc (format nil "ON ~{~a ~}" ,words-sym))
	 (defsentence ,words-sym
	     (cons *current-location* (words2label ,words-sym))
	   *current-location*)
	 (label (words2label ,words-sym) *current-location*)
	 ,@body
	 (RTS)))))

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
