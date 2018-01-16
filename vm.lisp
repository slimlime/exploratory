;;;; VM WIP
;;;; These are the common operations. Once we know everything
;;;; we need, we can implement the game logic using byte-code
;;;; for massive gainz in compactness.

(defparameter *vmops* nil)

(defun vm ()
  (with-namespace :vm
    (zp-w :pc)
    (label :go)
    (JSR :execute-op)
    (label :vm-go nil)
    (LDY 0)
    (LDA.IZY :pc)
    (dc "Get the instruction")
    (BEQ :done "RTS")
    (inc16.zp :pc)    
    (BNE :execute-op)
    (label :op)
    (TAX) 
    (LDA.ABX (1- (resolve :op-hi)))
    (PHA)
    (LDA.ABX (1- (resolve :op-lo)))
    (PHA)    
    (label :done)
    (RTS)

    ;; Let's inline all the opcode functions

    (mapc #'funcall *vmops*)
        
    (label :op-hi)
    (mapc #'(lambda (addr) (db nil (rts-jmp-hi addr))) *vmops*)
    
    (label :op-lo)
    (mapc #'(lambda (addr) (db nil (rts-jmp-lo addr))) *vmops*)))

(defun vm-fetch (&optional (y-blatted nil))
  "Assumes Y is zero"
  (when y-blatted
    (LDY 0))
  (LDA.IZY :pc)
  (vm-skip))

(defparameter *opcode* 1)

(setf *opcode* 0)
(setf *vmops* nil)

(defmacro defvmop (name size hint lambda-list (&body emitter) (&body definition))
  `(progn
     (push #'(lambda ()
	       (label (quote ,name) :vm-op)
	       ,@definition)
	   *vmops*)
     (defun ,name ,lambda-list
       (with-namespace ,name
	 (add-hint ,size ,hint)
	 (db ,nil *opcode*))
       ,@emitter)
     (incf *opcode*)))

(defvmop pr1 3 (format nil "PR1 ~a" str) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.AB (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.AB (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))

(defvmop pr2 3 (format nil "PR2 ~a" str) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.AB (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.AB (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))

(defvmop pr3 3 (format nil "PR3 ~a" str) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.AB (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.AB (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))

(defun forward-branch-offset (label)
  (if *compiler-final-pass*
      (- (resolve label) *compiler-ptr*)
      0))

(defvmop when.zpb 3 (format nil "WHEN ~a" bit) (bit endif)
	 ((push-byte (resolve bit))
	  (push-byte (forward-branch-offset endif)) 
	  ((vm-fetch)
	   (inc16.zp :pc) ;;always need to increment
	   (BIT.ZP bit)
	   (BMI :done)
	   (LDA.IZY :pc)  ;;but only need the offset if we don't execute the clause
	   (add16a.zp :pc)
	   (RTS)
	   (label :done)
	   (inc16.zp :pc)
	   (RTS))))

;; reverse the order of the ops since they were pushed in

(setf *vmops* (nreverse *vmops*))
