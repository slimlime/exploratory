;;; Virtual Machine
;;; Aim, compactness of generated code

;;; TODO do we need VM-RBRA? VM-BRA only goes forward

;;; So there is now a register VM-T which contains the result
;;; of a test, and two instructions for branching on the result
;;; of a test.

;;; TODO LDA.IZY :xxx should show XXX address in the comment automatically

(defparameter *vmops* nil)

(defun vm ()
  (zp-w :vm-pc)
  (zp-b :vm-t) ;;Wow, a register

  (with-namespace :vm

    (dc "Begin executing VM code at the callsite")
    (dc "After VM-DONE, resume 6502 execution")
    (label :vm-enter nil)
    (PLA)
    (TAY)
    (INY)
    (STY.ZP (lo-add :vm-pc))
    (PLA)
    (STA.ZP (hi-add :vm-pc))

    (JSR :vm-go)
    (dc "Resume 6502 execution immediately after the virual code")
    (LDA.ZP (hi-add :vm-pc))
    (PHA)
    (LDA.ZP (lo-add :vm-pc))
    (PHA)
    (RTS)

    (label :go)
    (JSR :execute-op)
    (dc "Execute VM code at :VM-PC. Return when VM-DONE is encountered")
    (dc "This is the normal entry point for an action handler") 
    (label :vm-go nil)
    (LDY 0)
    (dc "Get the instruction")
    (LDA.IZY :vm-pc)
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
    (AND.IMM 7)
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

(defparameter *vm-done-optimizations* (make-hash-table :test 'equal))

(defun reset-vm ()
  (setf *vm-done-optimizations* (make-hash-table :test 'equal)))

;;
;;TODO this is only going to work for branches within the calling namespace
;;hopefully fmt-addr will crap out if so, so we can fix it.
(defun qualify-label-enclosing-namespace (addr)
  (if (or (consp addr) (numberp addr))
      addr
      (cons (second *compiler-namespace-stack*) addr)))

(defun fmt-addr (addr)
  (if (numberp addr)
      (format nil "$~4,'0X" addr)
      (let ((l (qualify-label-enclosing-namespace addr)))
	(format nil "~a:~a" (car l) (cdr l)))))

(defun register-vm-done ()
  (dolist (label (gethash (1- *compiler-ptr*) *compiler-address-labels*))
    (setf (gethash label *vm-done-optimizations*) t)))

;;
;; VM-DONE Finish the VM code and return to where we came from
;;
(defvmop vm-done "VM-DONE" ()
	 ((register-vm-done))
	 ())
;;
;; VM-EXE Execute 6502 here
;;
(defvmop vm-exe "VM-EXE" ()
	 ()
	 ((dc "Pop so we return to caller of VM- we're done after this")
	  (PLA)
	  (PLA)
	  (JMP.IND :vm-pc)))

;;
;; VM-DEL Delegate to generic handler, if one applies
;;
(defvmop vm-del (format nil "VM-DEL") ()
	 ()
	 ((PLA)
	  (PLA)	  
	  (JMP '(:dispatcher . :generic-only))))

(defun forward-branch-offset (label)
  (if *compiler-final-pass*
      (- (resolve label) *compiler-ptr* 1)
      0))

(defun can-omit-branch (addr optimize)
  (and optimize (gethash (qualify-label-enclosing-namespace addr) *vm-done-optimizations*)))

;;
;;VM-BRA - Forward Branch Always
;;
(defvmop vm-bra (if (can-omit-branch addr optimize)
		    "VM-DONE"
		    (format nil "VM-BRA ~a" (fmt-addr addr)))
  (addr &optional (optimize nil))
  ((if (can-omit-branch addr optimize)
       (progn
	 (decf *compiler-ptr*)
	 (push-byte 0)
	 (register-vm-done))
       (push-byte (forward-branch-offset addr))))
  ((BNE '(:vm-branch . :branch))
   (BEQ '(:vm-branch . :branch))))

;;Clearly VM-BOOP, BOIP, BSET, BCLR could be implemented
;;;in terms of VM-BRT/BRF, but they are rather more common
;; so they can stay.

;;
;;VM-BSET - Branch on bit set
;;
(defvmop vm-bset (format nil "VM-BSET ~a ~a"
			 (string-downcase bit)
			 (fmt-addr addr))
  (bit addr)
  ((push-byte (bit-index bit))
   (push-byte (forward-branch-offset addr)))
  ((JSR :get-bit-mask)
   (AND.ABY :bit-table)
   (BNE '(:vm-branch . :branch))
   (BEQ '(:vm-branch . :dont-branch))))

;;
;;VM-BCLR - Branch on bit not set
;;
(defvmop vm-bclr (format nil "VM-BCLR ~a ~a"
			 (string-downcase bit)
			 (fmt-addr addr))
  (bit addr)
  ((push-byte (bit-index bit))
   (push-byte (forward-branch-offset addr)))
  ((JSR :get-bit-mask)
   (AND.ABY :bit-table)
   (BEQ '(:vm-branch . :branch))
   (BNE '(:vm-branch . :dont-branch))))

;;;
;;;VM-BOIP - Branch on object in place
;;;
(defvmop vm-boip (format nil "VM-BOIP ~a IN ~a ~a"
			 (string-downcase object)
			 (string-downcase place) (fmt-addr addr))
  (object place addr)
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
;;;VM-BOOP - Branch on object not in place
;;;
(defvmop vm-boop (format nil "VM-BOOP ~a NOT IN ~a ~a"
			 (string-downcase object)
			 (string-downcase place) (fmt-addr addr))
  (object place addr)
  ((push-byte (object-id object))
   (push-byte (place-id place))
   (push-byte (forward-branch-offset addr)))
  ((vm-fetch)
   (TAX)
   (vm-fetch)
   (CMP.ABX (1- (resolve '(:object-table . :places))))
   (BNE '(:vm-branch . :branch))
   (BEQ '(:vm-branch . :dont-branch))))

;;
;;VM-BRT - Forward Branch When True
;;
(defvmop vm-brt (format nil "VM-BRT ~a" (fmt-addr addr))
  (addr)
  ((push-byte (forward-branch-offset addr)))
  ((BIT.ZP :vm-t)
   (BVS '(:vm-branch . :branch))
   (BVC '(:vm-branch . :dont-branch))))

;;
;;VM-BRF - Forward Branch When False
;;
(defvmop vm-brf (format nil "VM-BRF ~a" (fmt-addr addr))
  (addr)
  ((push-byte (forward-branch-offset addr)))
  ((BIT.ZP :vm-t)
   (BVC '(:vm-branch . :branch))
   (BVS '(:vm-branch . :dont-branch))))

;;
;;VM-JMP - Jump anywhere
;;
(defvmop vm-jmp (format nil "VM-JMP ~a" (fmt-addr addr))
  (addr)
  ((dw nil addr))
  ((LDA.IZY :vm-pc)
   (TAX)
   (INY "Don't inc vm-pc as we throw it away")
   (LDA.IZY :vm-pc)
   (STX.ZP (lo-add :vm-pc))
   (STA.ZP (hi-add :vm-pc))
   (RTS)))

#| SERIOUS INSTABILITY PROBLEM THAT THREATENS THE WHOLE
   FABRIC OF SPACE AND TIME

Using this, which is obviously not completely stable
between passes REALLY screws up the build and introduces
incorrect branch offsets elsewhere

(defun vm-bra-or-jmp (addr)
  (let ((offset (forward-branch-offset addr)))
    (if (and (>= offset 0)
	     (< offset 256))
	(vm-bra addr)
	(vm-jmp addr)))) |#

;;;
;;; VM-NAV - Navigate to a new location
;;;
(defvmop vm-nav (format nil "VM-NAV ~a" location) (location)
	 ((dw nil location))
	 ((vm-fetch)
	  (TAX)
	  (vm-fetch)
	  (JMP '(:navigate . :navigate-no-deref))))

(defun vm-print-inline-fn (lines)
  (dc "The string to print is right after the VM-PR instruction")
  (cpy16.zp :vm-pc '(:typeset-cs . :str))
  (LDA lines)
  (JSR :print-message)
  (dc "The string printer should have left the pointer")
  (dc "on the byte immediately following the string")
  (dc "So we will resume there")
  (cpy16.zp '(:typeset-cs . :str) :vm-pc)
  (RTS))

;;;
;;; VM-PRI1 - Print one line string, inlined after the op
;;;
(defvmop vm-pri1 (format nil "VM-PRI1 '~a'" str) (str)
	 ((dcs nil str))
	 ((vm-print-inline-fn 1)))
;;;
;;; VM-PRI2 - Print two line string, inlined after the op
;;;
(defvmop vm-pri2 (format nil "VM-PRI2 '~a'" str) (str)
	 ((dcs nil str))
	 ((vm-print-inline-fn 2)))

;;;
;;; VM-PRI3 - Print three line string, inlined after the op
;;;
(defvmop vm-pri3 (format nil "VM-PRI3 '~a'" str) (str)
	 ((dcs nil str))
	 ((vm-print-inline-fn 3)))

(defun vm-print-string-fn (lines)
  (dc "The address of the string is after the instruction")
  (vm-fetch)
  (STA.ZP (lo-add '(:typeset-cs . :str)))
  (vm-fetch)
  (STA.ZP (hi-add '(:typeset-cs . :str)))
  (LDA lines)
  (JMP :print-message))

;;;
;;; VM-PR1 - Print one line string
;;;
(defvmop vm-pr1 (format nil "VM-PR1 '~a'" str) (addr str)
	 ((dw nil addr))
	 ((vm-print-string-fn 1)))
;;;
;;; VM-PR2 - Print two line string
;;;
(defvmop vm-pr2 (format nil "VM-PR2 '~a'" str) (addr str)
	 ((dw nil addr))
	 ((vm-print-string-fn 2)))

;;;
;;; VM-PR3 - Print three line string
;;;
(defvmop vm-pr3 (format nil "VM-PR3 '~a'" str) (addr str)
	 ((dw nil addr))
	 ((vm-print-string-fn 3)))

;;
;;VM-CLR - Clear bit
;;
(defvmop vm-clr (format nil "VM-CLR ~a" (string-downcase bit)) (bit)
	 ((push-byte (bit-index bit)))
	 ((JSR :get-bit-mask)
	  (EOR #xff)
	  (AND.ABY :bit-table)
	  (STA.ABY :bit-table)
	  (RTS)))

;;
;;VM-SET - Set bit
;;
(defvmop vm-set (format nil "VM-SET ~a" (string-downcase bit)) (bit)
	 ((push-byte (bit-index bit)))
	 ((JSR :get-bit-mask)
	  (ORA.ABY :bit-table)
	  (STA.ABY :bit-table)
	  (RTS)))

;;;
;;;VM-MOV - Move object to place
;;;
(defvmop vm-mov (format nil "VM-MOV ~a ~a "
			(string-downcase object)
			(string-downcase place))
  (object place)
  ((push-byte (object-id object))
   (push-byte (place-id place)))
  ((vm-fetch)
   (TAX)
   (vm-fetch)
   (STA.ABX (1- (resolve '(:object-table . :places))))
   (RTS)))

;;;
;;;VM-TWORD - Test for a word
;;;
(defvmop vm-tword (format nil "VM-TWORD ~a"
			  (string-downcase word))
  (word)
  ((push-byte (word-id word)))
  ((vm-fetch)
   (LDY (1- *max-input-words*))
   (label :next)
   (CMP.ABY '(:parser . :words))
   (BEQ :found)
   (DEY)
   (BPL :next)
   (INY "Set Y to 0")
   (STY.ZP :vm-t)
   (RTS)
   (label :found)
   (LDA #xff)
   (STA.ZP :vm-t)
   (RTS)))

;; reverse the order of the ops since they were pushed in

(setf *vmops* (nreverse *vmops*))
