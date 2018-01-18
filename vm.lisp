;;;; VM WIP
;;;; These are the common operations. Once we know everything
;;;; we need, we can implement the game logic using byte-code
;;;; for massive gainz in compactness.

(defparameter *vmops* nil)

(defun vm ()
  (zp-w :vm-pc)
  (with-namespace :vm

    (label :vm-enter nil)
    (PLA)
    (TAY)
    (INY)
    (STY.ZP (lo-add :vm-pc))
    (PLA)
    (STA.ZP (hi-add :vm-pc))

    (JSR :vm-go)
    ;;now we resume normal execution after the vm-done
    (LDA.ZP (hi-add :vm-pc))
    (PHA)
    (LDA.ZP (lo-add :vm-pc))
    (PHA)
    (RTS)

    (label :go)
    (JSR :execute-op)
    (label :vm-go nil)
    (LDY 0)
    (LDA.IZY :vm-pc)
    (dc "Get the instruction")
    (BEQ :done "VM-DONE")
    (inc16.zp :vm-pc)    
    (BNE :go)
    (label :execute-op)
    (TAX) 
    (LDA.ABX (resolve :op-hi))
    (PHA)
    (LDA.ABX (resolve :op-lo))
    (PHA)
    (label :done)
    (RTS)

    ;; Branching routine for the opcode to jump
    ;; into.

    (with-namespace :vm-branch
      (label :branch)
      (LDY 0)
      (LDA.IZY :vm-pc)
      (SEC "+1, this parameter byte")
      (adc16a.zp :vm-pc)
      (RTS)
      (label :dont-branch)
      (inc16.zp :vm-pc)
      (RTS))
    
    ;; Let's inline all the opcode functions

    (mapc #'(lambda (vmop) (funcall (second vmop))) *vmops*)
        
    (label :op-hi)
    (mapc #'(lambda (vmop) (db nil (rts-jmp-hi (first vmop)))) *vmops*)
    
    (label :op-lo)
    (mapc #'(lambda (vmop) (db nil (rts-jmp-lo (first vmop)))) *vmops*)

    ;; And some stuff those functions need
    ;; Like this bit mask table

    (zp-b :bit-0 1)
    (zp-b :bit-1 2)
    (zp-b :bit-2 4)
    (zp-b :bit-3 8)
    (zp-b :bit-4 16)
    (zp-b :bit-5 32)
    (zp-b :bit-6 64)
    (zp-b :bit-7 128)


    (label :get-bit-mask)
    (dc "Get the address of the bit and leave it in Y")
    (dc "then get the single-bit mask for that bit and")
    (dc "leave it in A")
    (LDA.IZY :vm-pc)
    (inc16.zp :vm-pc)
    (TAX)
    (dc "Get offset into bit-table")
    (LSR)
    (LSR)
    (LSR)
    (TAY)
    (TXA)
    (dc "Get bit number")
    (AND 7)
    (TAX)
    (LDA.ZPX :bit-0)
    (RTS)))

(defparameter *opcode* 1)

(setf *opcode* 0)
(setf *vmops* nil)

;; Don't forget variadic op-codes are possible. Because we can, we should.

(defmacro defvmop (name hint lambda-list (&body emitter) (&body definition))
  (let ((opcode (gensym))
	(start-ptr (gensym)))
    `(progn
       (push (list ,(intern (symbol-name `,name) :keyword)
		   #'(lambda ()
		       (label ,(intern (symbol-name `,name) :keyword) :vm)
		       ,@definition))
	     *vmops*)
       (let ((,opcode *opcode*))
	 (defun ,name ,lambda-list
	   (with-namespace ,(intern (symbol-name `,name) :keyword)
	     (let ((,start-ptr *compiler-ptr*))
	       (push-byte ,opcode)
	       ,@emitter
	       (add-hint-at ,start-ptr (- *compiler-ptr* ,start-ptr) ,hint)))))
       (incf *opcode*))))

(defun vm-fetch ()
  (LDA.IZY :vm-pc)
  (inc16.zp :vm-pc))

(defun fmt-addr (addr)
  (if (numberp addr)
      (format nil "$~4,'0X" addr)
      (format nil "$~4,'0X (~a)" (resolve addr) addr)))

(defvmop vm-done "VM-DONE" ()
	 ()
	 ())

;; Drop out here to 6502
(defvmop vm-exe "VM-EXE" ()
	 ()
	 ((JMP.IND :vm-pc)))

;; Delegate to generic handler
(defvmop vm-del (format nil "VM-DEL") ()
	 ()
	 ())

(defun forward-branch-offset (label)
  (if *compiler-final-pass*
      (- (resolve label) *compiler-ptr*)
      0))

;;
;;BRA - Branch always
;;
(defvmop vm-bra (format nil "VM-BRA -> ~a" (fmt-addr addr)) (addr)
	 ((push-byte (forward-branch-offset addr)))
	 ((BNE '(:vm-branch . :branch))
	  (BEQ '(:vm-branch . :branch))))

;;
;;BSET - Branch on bit set
;;
(defvmop vm-bset (format nil "VM-BSET ~a -> ~a" bit (fmt-addr addr)) (bit addr)
	 ((push-byte (bit-index bit))
	  (push-byte (forward-branch-offset addr)))
	 ((JSR :get-bit-mask)
	  (AND.ABY :bit-table)
	  (BNE '(:vm-branch . :branch))
	  (BEQ '(:vm-branch . :dont-branch))))

;;
;;BCLR - Branch on bit not set
;;
(defvmop vm-bclr (format nil "VM-BCLR ~a -> ~a" bit (fmt-addr addr)) (bit addr)
	 ((push-byte (bit-index bit))
	  (push-byte (forward-branch-offset addr)))
	 ((JSR :get-bit-mask)
	  (AND.ABY :bit-table)
	  (BEQ '(:vm-branch . :branch))
	  (BNE '(:vm-branch . :dont-branch))))

;;;
;;;BOIP - Branch on object in place
;;;
(defvmop vm-boip (format nil "VM-BOIP ~a ~a -> ~a" object place (fmt-addr addr)) (object place addr)
	 ((push-byte (object-id object))
	  (push-byte (place-id place))
	  (push-byte (forward-branch-offset addr)))
	 ((vm-fetch)
	  (TAX)
	  (vm-fetch)
	  (CMP.ABX (1- (resolve '(:object-table . :places))))
	  (BEQ '(:vm-branch . :branch))
	  (BNE '(:vm-branch . :dont-branch))))

;;;
;;;BOOP - Branch on object not in place
;;;
(defvmop vm-boop (format nil "VM-BOOP ~a ~a -> ~a" object place (fmt-addr addr)) (object place addr)
	 ((push-byte (object-id object))
	  (push-byte (place-id place))
	  (push-byte (forward-branch-offset addr)))
	 ((vm-fetch)
	  (TAX)
	  (vm-fetch)
	  (CMP.ABX (1- (resolve '(:object-table . :places))))
	  (BNE '(:vm-branch . :branch))
	  (BEQ '(:vm-branch . :dont-branch))))

(defvmop vm-nav (format nil "VM-NAV ~a" location) (location)
	 ((dw nil location))
	 ((vm-fetch)
	  (TAX)
	  (vm-fetch)
	  (JMP '(:navigate . :navigate-no-deref))))

(defvmop vm-pr1 (format nil "VM-PR1 ~a" (fmt-addr str)) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.ZP (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.ZP (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))

(defvmop vm-pr2 (format nil "VM-PR2 ~a" (fmt-addr str)) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.ZP (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.ZP (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))

(defvmop vm-pr3 (format nil "VM-PR3 ~a" (fmt-addr str)) (str)
	 ((dw nil str))
	 ((vm-fetch)
	  (STA.ZP (hi-add '(:typeset-cs . :str)))
	  (vm-fetch)
	  (STA.ZP (lo-add '(:typeset-cs . :str)))
	  (LDA 1)
	  (JMP :print-message)))
	 
;;
;;CLR - Clear bit
;;
(defvmop vm-clr (format nil "VM-CLR ~a" bit) (bit)
	 ((push-byte (bit-index bit)))
	 ((JSR :get-bit-mask)
	  (EOR #xff)
	  (AND.ABY :bit-table)
	  (STA.ABY :bit-table)))

;;
;;SET - Set bit
;;
(defvmop vm-set (format nil "VM-SET ~a" bit) (bit)
	 ((push-byte (bit-index bit)))
	 ((JSR :get-bit-mask)
	  (ORA.ABY :bit-table)
	  (STA.ABY :bit-table)))

;;;
;;;MOV - Move object to place
;;;
(defvmop vm-mov (format nil "VM-MOV ~a ~a " object place) (object place)
	 ((push-byte (object-id object))
	  (push-byte (place-id place)))
	 ((vm-fetch)
	  (TAX)
	  (vm-fetch)
	  (STA.ABX (1- (resolve '(:object-table . :places))))))

;; reverse the order of the ops since they were pushed in

(setf *vmops* (nreverse *vmops*))
