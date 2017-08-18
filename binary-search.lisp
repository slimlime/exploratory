(defun binary-search (&key (depth 8))
  (label :binary-search)
  (with-namespace :binary-search
    (alias :word :A0)
    (alias :index :D0)
    (alias :delta :D1)

    (LDA (ash 1 (1- depth)))
    (STA.ZP :delta)

    (label :next)

    (STA.ZP :index)
    (TAX)
    
    (LDY 0)
    (LDA.IZY :word)
    (CMP.ABX (1- (resolve :tbl1)) "tbl1 - 1")
    (BNE :not-match)
    
    (INY)
    (LDA.IZY :word)
    (CMP.ABX (1- (resolve :tbl2)) "tbl2 - 1")
    (BNE :not-match)
    (dc "Might be a one letter word")
    (CMP 0 "Check for space")
    (BEQ :return)
    
    (INY)
    (LDA.IZY :word)
    (CMP.ABX (1- (resolve :tbl3)) "tbl3 - 1")

    (BNE :not-match)
    (dc "Might be a two letter word")
    (CMP 0 "Check for space")
    (BEQ :return)

    (INY)
    (LDA.IZY :word)
    (CMP.ABX (1- (resolve :tbl4)) "tbl4 - 1")
    (BEQ :return)
    
    (label :not-match)
    (BPL :gt)
    (dc "Less than...")
    (LSR.ZP :delta)
    (BEQ :not-found)
    (LDA.ZP :index)
    (SEC)
    (SBC.ZP :delta)
    (dc "Always jump... A != 0 since 128 - 64 ... - 1 = 1")
    (BNE :next)
    (label :gt)
    (dc "Greater than...")
    (LSR.ZP :delta)
    (BEQ :not-found)
    (LDA.ZP :index)
    (CLC)
    (ADC.ZP :delta)
    (BNE :next)

    (label :not-found)
    (LDA 0)
    (STA.ZP :index)
    (label :return)
    (RTS)))

(defun char-or-space (string index)
  (if (>= index (length string))
      0
      (to-alphabet-pos (char string index))))

(defun binary-search-tables (strings)
  (with-namespace :binary-search
    (setf strings (sort (copy-list strings) #'string<))
    ;;TODO ensure that duplicates only appear once
    ;;they will resolve to something so we can check them with the tree parser
    (apply #'db :tbl1 (mapcar #'(lambda (s) (char-or-space s 0)) strings))
    (apply #'db :tbl2 (mapcar #'(lambda (s) (char-or-space s 1)) strings))
    (apply #'db :tbl3 (mapcar #'(lambda (s) (char-or-space s 2)) strings))
    (apply #'db :tbl4 (mapcar #'(lambda (s) (char-or-space s 3)) strings))))
    
(defun test-binary-search (strings input)
  (reset-compiler)
  (flet ((pass ()
	   (zeropage)
	   (org #x600)
	   (CLD)
	   (label :start)
	   
	   (sta16.zp :input '(:binary-search . :word))
	   (JSR :binary-search)
	   
	   (BRK)
	   
	   (db :output 0)
	   
	   (dbs :input 40 0)

	   (binary-search :depth 3)

	   (binary-search-tables strings)
	   	 
	   (label :end)))

    (pass)
    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))
    
    (setf *compiler-final-pass* t)
    (pass)
    
    (format t "Build size ~a~%" (- *compiler-ptr* origin)))

  
  ;; install the string into the input buffer
	 
  (loop for c across input
     for i from 0 to *max-input-length* do       
       (setf (aref *compiler-buffer* (+ i (resolve :input)))
	     (to-alphabet-pos c)))
  
  (monitor-reset #x600)
  (monitor-run :print nil)
  
  ;; return the output byte which will have been set
  ;; if the correct handler is called.
  
  (aref (monitor-buffer) (resolve '(:binary-search . :index))))

(defparameter *test-strings* '("ape" "apple" "banana" "banyan" "cumquat" "gr" "z"))

(loop
   for item in *test-strings*
   for i from 1 to 7 do
     (assert (= (test-binary-search *test-strings* item) i)
	     nil
	     (format nil "Item ~a should be at ~a" item i)))

(assert (= (test-binary-search *test-strings* "dasduhax") 0))

     
