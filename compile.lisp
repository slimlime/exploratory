;; 6502 assembler library

(defparameter *opcodes* (make-hash-table :test 'equal))
(defparameter *reverse-opcodes* (make-hash-table))

(defparameter *compiler-ptr* nil)
(defparameter *compiler-disassembler-hints* nil)
(defparameter *compiler-buffer* nil)
(defparameter *compiler-labels* nil)
(defparameter *compiler-ensure-labels-resolve* nil)


(defun reset-compiler (&optional (buffer-size 65536))
  (setf *compiler-ptr* 0)
  (setf *compiler-disassembler-hints* (make-hash-table))
  (setf *compiler-ensure-labels-resolve* nil)
  (setf *compiler-labels* (make-hash-table :test 'equal))
  (setf *compiler-buffer* (make-array buffer-size
				      :element-type '(unsigned-byte 8)
				      :initial-element 0))
  (values))

(defmacro defop (byte opcode &optional (mode :imp))
  (let ((todo nil))
    (push `(setf (gethash ,byte *reverse-opcodes*) (cons  ',opcode ,mode))
	  todo)
    (push `(setf (gethash (cons ',opcode ,mode) *opcodes*) ,byte)
	  todo)
    (cond ((eq mode :imp)
	   (push `(defun ,opcode () (imp ',opcode)) todo))
	  ((eq mode :rel)
	   (push `(defun ,opcode (label) (rel ',opcode label)) todo))
	  (t (let ((param (case mode 
			    (:aby 'addr) (:abx 'addr) (:ab  'addr)
			    (:zp  'zpg)  (:zpx 'zpg)  (:zpy 'zpg)
			    (:izx 'zpg)  (:izy 'zpg)
			    (:imm 'byte) (:ind 'addr))))
	       (push `(defun ,(intern
			       (format nil "~a.~a"
				       (string opcode)
				       (string mode)))
			  ,(list param)
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
  (assert-address add)
  (flet ((peek (add)
	   (if (< add 65536)
	       (peek-byte add)
	       0)))
    (loop 
       for i from add to (+ add (1- (* 16 (ceiling len 16))))
       for j from 1 do
	 (when (= 1 (rem j 16))
	   (format t "~4,'0X " i))
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
  (setf (gethash *compiler-ptr*
		 *compiler-disassembler-hints*)
	(cons size str)))

(defun assert-byte (byte)
  (assert (>= byte 0))
  (assert (< byte 256)))

(defun push-byte (byte)
  (assert-byte byte)
  (setf (aref *compiler-buffer* *compiler-ptr*) byte)
  (incf *compiler-ptr*))

(defun push-sbyte (byte)
  (when (< byte 0)
    (incf byte 256))
  (push-byte byte))

(defun push-address (add)
  (assert-address add)
  (push-byte (logand add #xFF))  ;little end
  (push-byte (ash add -8)))      ;big end

(defun push-op (op mode)
  (let ((byte (gethash (cons op mode) *opcodes*)))
    (assert (numberp byte) nil 
	    (format nil "Opcode:~a Addressing Mode:~a is invalid" op mode))
    (push-byte byte)))

(defun push-zpg-op (op mode zpg)
  (push-op op mode)
  (push-byte (resolve zpg)))

(defun push-addr-op (op mode addr)
  (push-op op mode)
  (push-address (resolve addr)))

(defun resolve (arg)
  (if (numberp arg)
      arg
      (let ((addr (gethash arg *compiler-labels*)))
; on the first pass only, allow labels to be 0
	(if (and (not *compiler-ensure-labels-resolve*)
		 (null addr))
	     (values 0 nil)
	     (values addr t)))))

;; assembler instructions

(defun org (add)
  (assert-address add)
  (setf *compiler-ptr* add))

(defun label (label)
  (assert (not (numberp label)))
  (let ((add (gethash label *compiler-labels*)))
    (if add
	(assert (= add *compiler-ptr*)) ;changing the value of a label is wrong
	(setf (gethash label *compiler-labels*) *compiler-ptr*))))

(defun db (label &rest bytes)
  (add-hint (length bytes)
	    (format nil "DB ~{$~2,'0X~^, ~}" bytes))
  (when label (label label))
  (dolist (byte bytes)
    (push-byte byte)))

;; TODO LOOKUP DEFINITION OF DW FROM COMMON ASSEMBLERS-
;; I TAKE IT THEY DO IT LO-HI

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

(defun hi (addr)
  (ash (resolve addr) -8))

(defun lo (addr)
  (logand #xFF (resolve addr)))

(defun dc (comment)
  (add-hint :comment comment))

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
  (multiple-value-bind (addr resolved) (resolve label)
    (push-sbyte (if resolved (- addr (1+ *compiler-ptr*))
		    0))))

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

(reset-compiler)

;;;; Disassembler

(defun disassemble-6502 (start end &key (buffer nil))
  (when (null buffer) (setf buffer *compiler-buffer*))
  (setf start (resolve start))
  (setf end (resolve end))
  (let ((lab (make-hash-table)))		
					;invert the label table
    (maphash #'(lambda (k v) (setf (gethash v lab) k))
	     *compiler-labels*)
    (loop for i from start to end do
	 (let* ((opcode (gethash (aref buffer i) *reverse-opcodes*))
		(mode (cdr opcode))
		(op (car opcode)))
	   (let* ((label (gethash i lab))
		  (str (if label 
			   (subseq (format nil "~a~a" label "         ") 0 9)
			   "         "))
		  (hint (gethash i *compiler-disassembler-hints*)))
	     (if (and hint (eq :comment (car hint)))
		 (format t "          ;~a~%" (cdr hint))
		 (format t "~a ~4,'0X ~2,'0X" str i (aref buffer i)))
	     (if (and hint (numberp (car hint)))
		 (progn
					;render hint
		   (incf i (1- (car hint)))
		   (format t "      ~a" (cdr hint)))
					;else render opcodes
		 (case mode
		   (:imm (format t "~2,'0X    ~a #$~2,'0X"
				 (aref buffer (incf i))
				 op 
				 (aref buffer i)))
		   (:rel (format t "~2,'0X    ~a $~4,'0X"
				 (aref buffer (incf i))
				 op 
				 (+ i 1 (aref buffer i))))
		   (:imp (format t "      ~a" op))
		   (:ab (format t "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X"
				(aref buffer (incf i))
				(aref buffer (incf i))
				op
				(aref buffer i)
				(aref buffer (1- i))))
		   (:zp (format t "~2,'0X    ~a $~2,'0X"
				(aref buffer (incf i))
				op
				(aref buffer i)))
		   (:ind (format t "~2,'0X~2,'0X  ~a ($~2,'0X~2,'0X)"
				 (aref buffer (incf i))
				 (aref buffer (incf i))
				 op
				 (aref buffer i)
				 (aref buffer (1- i))))
		   (:izx (format t "~2,'0X    ~a ($~2,'0X,X)"
				 (aref buffer (incf i))
				 op
				 (aref buffer i)))
		   (:izy (format t "~2,'0X    ~a ($~2,'0X),Y"
				 (aref buffer (incf i))
				 op
				 (aref buffer i)))
		   (:zpx (format t "~2,'0X    ~a $~2,'0X,X"
				 (aref buffer (incf i))
				 op
				 (aref buffer i)))
		   (:zpy (format t "~2,'0X    ~a $~2,'0X,Y"
				 (aref buffer (incf i))
				 op
				 (aref buffer i)))
		   (:aby (format t "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X,X"
				 (aref buffer (incf i))
				 (aref buffer (incf i))
				 op
				 (aref buffer i)
				 (aref buffer (1- i))))
		   (:abx (format t "~2,'0X~2,'0X  ~a $~2,'0X~2,'0X,Y"
				 (aref buffer (incf i))
				 (aref buffer (incf i))
				 op
				 (aref buffer i)
				 (aref buffer (1- i))))))))
	 (terpri))
    (values)))
	       
(defun test ()
  (reset-compiler)

  (dotimes (pass 2)
    (setf *compiler-ensure-labels-resolve* (= pass 1))

;; the code

    (org #x0000)

    (db :variable 0)
    (db :another-variable 1)
    (db :lbl1)
    
    (org #x0600)

    (label :start)
    
    (ORA.IZX :lbl1)
    (ORA.IZY :lbl1)
    (ROR)
    (LSR)
    (dc "This is a comment")
    (ROL)
    (ASL)
    (BCC :the-future)
    (JMP :over-some-text)
    (DS nil "Scrozzbot")
    (label :over-some-text)
    (CLD)
    (CLD)
    (label :the-future)
    (CLD)
    (STA.ZP :another-variable)
    (STA.AB :a-non-zpg-variable)
    (JMP :start)
    (RTS)
    (dw :words #x1234 #x5678 #xABCD)
    (db :bytes #x01 #x02 #x03)
    (BRK)
    (LDA.IMM (lo :start))
    (PHA)
    (LDA.IMM (hi :start))
    (PHA)
    (RTS)
    
    (ds nil "Tetradic Chronisms")
    (db :a-non-zpg-variable #x55)
    (NOP)
    (label :end)
 ) 
  ;;
  
  (hexdump #x0000 #x100)
  (hexdump #x0600 #x100))
