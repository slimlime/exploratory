;; 6502 assembler library

(defparameter *opcodes* (make-hash-table :test 'equal))
(defparameter *reverse-opcodes* (make-hash-table))

(defparameter *compiler-ptr* nil)
(defparameter *compiler-disassembler-hints* nil)
(defparameter *compiler-buffer* nil)
(defparameter *compiler-labels* nil)
(defparameter *compiler-aliases* nil)
(defparameter *compiler-final-pass* nil)
(defparameter *compiler-zp-free-slot* nil)
(defparameter *compiler-debug* nil)
(defparameter *compiler-comments* nil)
(defparameter *compiler-postfix-comments* nil)
(defparameter *compiler-label-namespace* nil)

; todo A way of providing a 'spare byte/word' to the compile
; which can then be used later e.g.

; (unused-b addr) - tell the compiler this byte is spare
; (use-b :label)  - tell the compiler to use the next spare byte for a label

(defun reset-compiler (&optional (buffer-size 65536))
  (setf *compiler-ptr* 0)
  (setf *compiler-disassembler-hints* (make-hash-table))
  (setf *compiler-final-pass* nil)
  (setf *compiler-labels* (make-hash-table :test 'equal))
  (setf *compiler-aliases* (make-hash-table :test 'equal))
  (setf *compiler-buffer* (make-array buffer-size
				      :element-type '(unsigned-byte 8)
				      :initial-element 0))
  (setf *compiler-comments* (make-hash-table))
  (setf *compiler-postfix-comments* (make-hash-table))
  (setf *compiler-zp-free-slot* 0)
  (values))

(defmacro defop (byte opcode &optional (mode :imp))
  (let ((todo nil))
    (push `(setf (gethash ,byte *reverse-opcodes*) (cons  ',opcode ,mode))
	  todo)
    (push `(setf (gethash (cons ',opcode ,mode) *opcodes*) ,byte)
	  todo)
    (cond ((eq mode :imp)
	   (push `(defun ,opcode (&optional comment) 
		    (when comment (dc comment t))
		    (imp ',opcode)) todo))
	  ((eq mode :rel)
	   (push `(defun ,opcode (label &optional comment) 
		    (when comment (dc comment t))
		    (rel ',opcode label)) todo))
	  (t (let ((param (case mode 
			    (:aby 'addr) (:abx 'addr) (:ab  'addr)
			    (:zp  'zpg)  (:zpx 'zpg)  (:zpy 'zpg)
			    (:izx 'zpg)  (:izy 'zpg)
			    (:imm 'byte) (:ind 'addr))))
	       (push `(defun ,(intern
			       (if (and (not (eq 'AND opcode))
					(eq :imm mode))
				   (format nil "~a"
					   (string opcode))
				   (format nil "~a.~a"
					   (string opcode)
					   (string mode))))
			  ,(list param '&optional 'comment)
			,(list 'when 'comment '(dc comment t))
			,(list (intern (string mode))
			       `',opcode
			       param)) todo))))
    (push 'progn todo)
    todo))

(defun assert-address (add)
  (assert (>= add 0))
  (assert (< add 65536)))

(defun peek-byte (add)
  (assert-address add)
  (aref *compiler-buffer* add))

(defun hexdump (add len)
  (unless (numberp add)
    (setf add (resolve add)))
  (assert-address add)
  (flet ((peek (add)
	   (if (< add 65536)
	       (peek-byte add)
	       0)))
    (loop 
       for i from add to (+ add (1- (* 16 (ceiling len 16))))
       for j from 1 do
	 (when (= 1 (rem j 16))
	   (format t "~4,'0X  " i))
	 (format t "~2,'0X" (peek i))
	 (when (zerop (rem j 2))
	   (format t " "))
	 (when (zerop (rem j 16))
	   (loop for k from (- i 15) to i do
		(format t "~c" (let ((byte (peek k)))
				 (if (and (> byte 31)
					  (< byte 128))
				     (code-char byte)
				     #\.))))
	   (terpri)))
  (values)))

(defun add-hint (size str)
  (when *compiler-final-pass*
    (setf (gethash *compiler-ptr*
		   *compiler-disassembler-hints*)
	  (cons size str))))

(defun assert-byte (byte)
  (assert (>= byte 0))
  (assert (< byte 256)))

(defun push-byte (byte)
  (when *compiler-final-pass*
    (assert-byte byte))
  (setf (aref *compiler-buffer* *compiler-ptr*) (logand byte #xff))
  (incf *compiler-ptr*))

(defun push-sbyte (byte)
  (when (< byte 0)
    (incf byte 256))
  (push-byte byte))

(defun push-address (add)
  (if *compiler-final-pass*
      (assert-address add)
      (setf add (logand add #xFFFF)))
  (push-byte (logand add #xFF))  ;little end
  (push-byte (ash add -8)))      ;big end

(defun push-op (op mode)
  (when *compiler-debug*
    (format t "OP:~a MODE:~a~%" op mode))
  (let ((byte (gethash (cons op mode) *opcodes*)))
    (assert (numberp byte) nil 
	    (format nil "Opcode:~a Addressing Mode:~a is invalid" op mode))
    (push-byte byte)))

(defun push-zpg-op (op mode zpg)
  (push-op op mode)
  (multiple-value-bind (addr resolved) (resolve zpg)
    (if resolved
	(progn
	  (assert (and (>= addr 0) (< addr 256))
		  nil "Could not use zero-page addressing for ~a as it was not on the zero page (~a)"
		  zpg addr)
	  (push-byte addr))
	(push-byte 0))))

(defun push-addr-op (op mode addr)
  (push-op op mode)
  (push-address (resolve addr)))

(defun resolve (arg)
  (if (numberp arg)
      (values arg t)
      (let ((addr nil))
	;; if there is an alias, then resolve that to a label first
	(let ((aliased-label (gethash 
			      (if (consp arg)
				  arg
				  (cons *compiler-label-namespace* arg))
			      *compiler-aliases*)))
	  (if aliased-label
	      ;; use the aliased label with the namespace
	      ;; it was applied with, ignore the current namespace
	      (setf addr (gethash aliased-label *compiler-labels*))
	      ;; otherwise try the label in the current namespace
	      ;; followed by the global namespace
	      (progn
		(when *compiler-label-namespace*
		  (setf addr (gethash (cons *compiler-label-namespace* arg)
				      *compiler-labels*)))
		;; if no match try again in the global namespace
		(unless addr
		  (setf addr (gethash arg *compiler-labels*)))))
	  ;; on the first pass only, allow
	  ;; labels to be null (resolve to 0 if so)
	  (when (and (null addr) 
		     *compiler-final-pass*)
	    (assert nil nil (format nil "The label ~a was not resolved (aliased-label [~a])" arg aliased-label)))
	  (if addr
	      (values addr t)
	      (values 0 nil))))))

;; assembler instructions

 (defun org (add)
   (assert-address add)
   (setf *compiler-ptr* add))

(defun label (label &optional (namespace nil namespace-p))
  (assert (not (numberp label)))
  (unless namespace-p
    ; use the scoped namespace unless one is supplied
    (setf namespace *compiler-label-namespace*))
  (when namespace
    ; combine the label and namespace if one is present
    (setf label (cons namespace label)))
  (setf (gethash label *compiler-labels*) *compiler-ptr*))

(defun alias (alias label)
  (assert (not (numberp alias)))
  (when *compiler-label-namespace*
    (setf alias (cons *compiler-label-namespace* alias)))
  (setf (gethash alias *compiler-aliases*) label))

(defun db (label &rest bytes)
  (add-hint (length bytes)
	    (format nil "DB ~{$~2,'0X~^, ~}" bytes))
  (when label (label label))
  (dolist (byte bytes)
    (push-byte byte)))

(defun dw (label &rest words)
  (add-hint (* 2 (length words))
	    (format nil "DW ~{$~4,'0X~^, ~}" words))
  (when label (label label))
  (dolist (word words)
    (push-address word)))

(defun ds (label string)
  (add-hint (1+ (length string))
	    (format nil "DS ~s" string))
  (when label (label label))
  (loop for c across string do
       (push-byte (char-code c)))
  (push-byte 0))

(defun dc (comment &optional (postfix nil))
  (when *compiler-final-pass*
    (when *compiler-debug*
      (print comment))
    (let ((*compiler-comments* (if postfix
				   *compiler-postfix-comments*
				   *compiler-comments*)))
      (let ((comments (gethash *compiler-ptr* *compiler-comments*)))
	(if comments
	    (unless (member comment comments :test 'equal)
	      (setf (gethash *compiler-ptr* *compiler-comments*)
		    (cons comment comments)))
	    (setf (gethash *compiler-ptr* *compiler-comments*) (list comment)))))))

(defun fmt-label (label &optional (namespace-p nil))
  (if (consp label)
	(if namespace-p 
	    (format nil "~a:~a" (car label) (cdr label))
	    (format nil "~a" (cdr label)))
	(format nil "~a" label)))

(defun hi (addr)
  "Return the high byte of an address"
  (unless (numberp addr)
    (dc (format nil "HI : ~a" (fmt-label addr t)) t))
  (ash (resolve addr) -8))

(defun lo (addr)
  "Return the lo byte of an address"
  (unless (numberp addr)
    (dc (format nil "LO : ~a" (fmt-label addr t)) t))
  (logand #xFF (resolve addr)))

(defun lo-add (addr)
  "Return the address of the lo byte, aka the address itself"
  (unless (numberp addr)
    (dc (format nil "~a" (fmt-label addr t)) t))
  (resolve addr))

(defun hi-add (addr)
  "Return the address of the hi byte of a label, aka the address + 1"
  (unless (numberp addr)
    (dc (format nil "~a + 1" (fmt-label addr t)) t))
  (1+ (resolve addr)))

;todo remove byte/word as not really setting it and also clashes
;if it appears once in the program, could assemble a zero page
;set up routine if needed.

(defun zp-b (label &optional (byte 0))
  (assert (> #xFF *compiler-zp-free-slot*))
  (unless (gethash label *compiler-labels*)
    (let ((*compiler-ptr* *compiler-zp-free-slot*))
      (db label byte))
    (incf *compiler-zp-free-slot*)))

(defun zp-w (label &optional (word 0))
  (assert (> #xFE *compiler-zp-free-slot*))
  (unless (gethash label *compiler-labels*)
    (let ((*compiler-ptr* *compiler-zp-free-slot*))
      (dw label word))
    (incf *compiler-zp-free-slot* 2)))

(defmacro with-namespace (namespace &body body)
  "In the scope of the macro, define labels in the namespace
   Label resolution will be done preferentially in this namespace
   but will fall back to the global namespace if not found"
  (let ((ns (gensym)))
  `(let* ((,ns ,namespace)
	  (*compiler-label-namespace* ,ns))
     (dc (format nil "+ ~a" ,ns))
     ,@body
     (dc (format nil "- ~a" ,ns)))))

(defun dump-aliases ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" (fmt-label k t) v)) *compiler-aliases*))

(defun dump-labels ()
  (maphash #'(lambda (k v) (format t "~a -> ~4,'0X~%" (fmt-label k t) v)) *compiler-labels*))


;; 6502 instructions by mode

(defun imp (op)      (push-op      op :imp))
(defun zp  (op zpg)  (push-zpg-op op :zp  zpg))
(defun ind (op addr) (push-addr-op op :ind addr))
(defun ab  (op addr) (push-addr-op op :ab  addr))
(defun abx (op addr) (push-addr-op op :abx addr))
(defun aby (op addr) (push-addr-op op :aby addr))
(defun izx (op zpg)  (push-zpg-op op :izx zpg))
(defun izy (op zpg)  (push-zpg-op op :izy zpg))
(defun zpx (op zpg)  (push-zpg-op op :zpx zpg))
(defun zpy (op zpg)  (push-zpg-op op :zpy zpg))

(defun imm (op byte)
  (push-op op :imm)
  (push-byte byte))

(defun rel (op label)
  (push-op op :rel)
  (if (numberp label)
      ;for relative addressing we assume that
      ;if a number is passed then it is relative
      (push-sbyte label)
      (if *compiler-final-pass*
	  (multiple-value-bind (addr resolved) (resolve label)
	    (push-sbyte (if resolved (- addr (1+ *compiler-ptr*))
			    0)))
	  (push-sbyte 0))))


(defun rel-addr (offset bra-addr)
  (+ 2 bra-addr (if (> offset #x7F)
		    (- offset #x100)
		    offset)))

;; opcodes

(defop #x00 BRK)      (defop #x01 ORA :izx) (defop #x05 ORA :zp)  (defop #x06 ASL :zp)
(defop #x08 PHP)      (defop #x09 ORA :imm) (defop #x0A ASL)      (defop #x0D ORA :ab)
(defop #x0E ASL :ab)  (defop #x10 BPL :rel) (defop #x11 ORA :izy) (defop #x15 ORA :zpx)
(defop #x16 ASL :zpx) (defop #x18 CLC)      (defop #x19 ORA :aby) (defop #x1D ORA :abx)
(defop #x1E ASL :abx) (defop #x20 JSR :ab)  (defop #x21 AND :izx) (defop #x24 BIT :zp)
(defop #x25 AND :zp)  (defop #x26 ROL :zp)  (defop #x28 PLP)      (defop #x29 AND :imm)
(defop #x2A ROL)      (defop #x2C BIT :ab)  (defop #x2D AND :ab)  (defop #x2E ROL :ab)
(defop #x30 BMI :rel) (defop #x31 AND :izy) (defop #x35 AND :zpx) (defop #x36 ROL :zpx)
(defop #x38 SEC)      (defop #x39 AND :aby) (defop #x3D AND :abx) (defop #x3E ROL :abx)
(defop #x40 RTI)      (defop #x41 EOR :izx) (defop #x45 EOR :zp)  (defop #x46 LSR :zp)
(defop #x48 PHA)      (defop #x49 EOR :imm) (defop #x4A LSR)      (defop #x4C JMP :ab)
(defop #x4D EOR :ab)  (defop #x4E LSR :ab)  (defop #x50 BVC :rel) (defop #x51 EOR :izy)
(defop #x55 EOR :zpx) (defop #x56 LSR :zpx) (defop #x58 CLI)      (defop #x59 EOR :aby)
(defop #x5D EOR :abx) (defop #x5E LSR :abx) (defop #x60 RTS)      (defop #x61 ADC :izx)
(defop #x65 ADC :zp)  (defop #x66 ROR :zp)  (defop #x68 PLA)      (defop #x69 ADC :imm)
(defop #x6A ROR)      (defop #x6C JMP :ind) (defop #x6D ADC :ab)  (defop #x6E ROR :ab)
(defop #x70 BVS :rel) (defop #x71 ADC :izy) (defop #x75 ADC :zpx) (defop #x76 ROR :zpx)
(defop #x78 SEI)      (defop #x79 ADC :aby) (defop #x7D ADC :abx) (defop #x7E ROR :abx)
(defop #x81 STA :izx) (defop #x84 STY :zp)  (defop #x85 STA :zp)  (defop #x86 STX :zp)
(defop #x88 DEY)      (defop #x8A TXA)      (defop #x8C STY :ab)  (defop #x8D STA :ab)
(defop #x8E STX :ab)  (defop #x90 BCC :rel) (defop #x91 STA :izy) (defop #x94 STY :zpx)
(defop #x95 STA :zpx) (defop #x96 STX :zpy) (defop #x98 TYA)      (defop #x99 STA :aby)
(defop #x9A TXS)      (defop #x9D STA :abx) (defop #xA0 LDY :imm) (defop #xA1 LDA :izx)
(defop #xA2 LDX :imm) (defop #xA4 LDY :zp)  (defop #xA5 LDA :zp)  (defop #xA6 LDX :zp)
(defop #xA8 TAY)      (defop #xA9 LDA :imm) (defop #xAA TAX)      (defop #xAC LDY :ab)
(defop #xAD LDA :ab)  (defop #xAE LDX :ab)  (defop #xB0 BCS :rel) (defop #xB1 LDA :izy)
(defop #xB4 LDY :zpx) (defop #xB5 LDA :zpx) (defop #xB6 LDX :zpy) (defop #xB8 CLV)
(defop #xB9 LDA :aby) (defop #xBA TSX)      (defop #xBC LDY :abx) (defop #xBD LDA :abx)
(defop #xBE LDX :aby) (defop #xC0 CPY :imm) (defop #xC1 CMP :izx) (defop #xC4 CPY :zp)
(defop #xC5 CMP :zp)  (defop #xC6 DEC :zp)  (defop #xC8 INY)      (defop #xC9 CMP :imm)
(defop #xCA DEX)      (defop #xCC CPY :ab)  (defop #xCD CMP :ab)  (defop #xCE DEC :ab)
(defop #xD0 BNE :rel) (defop #xD1 CMP :izy) (defop #xD5 CMP :zpx) (defop #xD6 DEC :zpx)
(defop #xD8 CLD)      (defop #xD9 CMP :aby) (defop #xDD CMP :abx) (defop #xDE DEC :abx)
(defop #xE0 CPX :imm) (defop #xE1 SBC :izx) (defop #xE4 CPX :zp)  (defop #xE5 SBC :zp)
(defop #xE6 INC :zp)  (defop #xE8 INX)      (defop #xE9 SBC :imm) (defop #xEA NOP)
(defop #xEC CPX :ab)  (defop #xED SBC :ab)  (defop #xEE INC :ab)  (defop #xF0 BEQ :rel)
(defop #xF1 SBC :izy) (defop #xF5 SBC :zpx) (defop #xF6 INC :zpx) (defop #xF8 SED)
(defop #xF9 SBC :aby) (defop #xFD SBC :abx) (defop #xFE INC :abx)

; default addressing modes

(defun JMP (addr) (JMP.AB addr))
(defun JSR (addr) (JSR.AB addr))

(reset-compiler)

;;;; Disassembler

(defun right-justify (s)
  (subseq (format nil "            ~a" s)
	  (length s)))

(defun disassemble-6502 (start end &key (buffer nil))
  (when (null buffer) (setf buffer *compiler-buffer*))
  (let ((*compiler-final-pass* t))
    (setf start (resolve start))
    (setf end (resolve end)))
  (let ((lab (make-hash-table)))		
    ;;invert the label table i.e. lookup label by address
    ;;todo, additional labels at the same address are currently hidden
    (maphash #'(lambda (k v) (setf (gethash v lab) k))
	     *compiler-labels*)
    (loop for i from start to end do
	 (let* ((opcode (gethash (aref buffer i) *reverse-opcodes*))
		(mode (cdr opcode))
		(op (car opcode))
		(arg1 (aref buffer (+ 1 i)))
		(arg2 (aref buffer (+ 2 i))))
	   (let* ((label (gethash i lab))
		  (str (if label
			   (right-justify (fmt-label label))
			   "            "))
		  (hint (gethash i *compiler-disassembler-hints*))
		  (comments (gethash i *compiler-comments*))
		  (postfix-comments (gethash i *compiler-postfix-comments*)))
	     (when comments
	       (dolist (comment (reverse comments))
		 (format t "             ;~a~%" comment)))
	     (format t "~a ~4,'0X ~2,'0X" str i (aref buffer i))
	     (if (and hint (numberp (car hint)))
		 (progn
					;render hint
		   (incf i (1- (car hint)))
		   (format t "      ~a" (cdr hint)))
					;else render opcodes
		 (labels ((format-label (addr)
			    (let ((label (gethash addr lab)))
			      (when label
				(format t "~45,1T;~a" (fmt-label label)))))
			  (format-cur-label ()
			    (format-label (logior arg1 (ash arg2 8))))
			  (format-1 (fmt) (format t fmt arg1 op arg1)
				    (incf i))
			  (format-2 (fmt) (format t fmt arg1 arg2 op arg2 arg1)
				    (format-cur-label)
				    (incf i 2)))
		   (case mode
		     (:imm (format-1 "~2,'0X    ~a #$~2,'0X"))
		     (:rel (let ((addr (rel-addr arg1 i)))
			     (format t "~2,'0X    ~a $~4,'0X" arg1 op addr)
			     (format-label addr))
			   (incf i))
		     (:imp (format t "      ~a" op))
		     (:ab (format-2 "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X"))
		     (:zp (format-1 "~2,'0X    ~a $~2,'0X"))
		     (:ind (format-2 "~2,'0X~2,'0X  ~a ($~2,'0X~2,'0X)"))
		     (:izx (format-1 "~2,'0X    ~a ($~2,'0X,X)"))
		     (:izy (format-1 "~2,'0X    ~a ($~2,'0X),Y"))
		     (:zpx (format-1 "~2,'0X    ~a $~2,'0X,X"))
		     (:zpy (format-1 "~2,'0X    ~a $~2,'0X,Y"))
		     (:aby (format-2 "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X,Y"))
		     (:abx (format-2 "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X,X")))))
	     (dolist (comment (reverse postfix-comments))
	       (format t "~45,1T;~a" comment))
	     (terpri))
	   (values)))))
