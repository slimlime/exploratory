;; 6502 assembler library

(defparameter *opcodes* (make-hash-table :test 'equal))

(defparameter *compiler-ptr* nil)
(defparameter *compiler-buffer* nil)
(defparameter *compiler-labels* nil)

(defmacro defop (byte opcode &optional (mode :imp))
  `(setf (gethash (cons ,opcode ,mode) *opcodes*)
	  ,byte))

(defop #x00 'BRK)      (defop #x01 'ORA :izx) (defop #x05 'ORA :zp)  (defop #x06 'ASL :zp)
(defop #x08 'PHP)      (defop #x09 'ORA :imm) (defop #x0A 'ASL)      (defop #x0D 'ORA :abs)
(defop #x0E 'ASL :abs) (defop #x10 'BPL :rel) (defop #x11 'ORA :izy) (defop #x15 'ORA :zpx)
(defop #x16 'ASL :zpx) (defop #x18 'CLC)      (defop #x19 'ORA :aby) (defop #x1D 'ORA :abx)
(defop #x1E 'ASL :abx) (defop #x20 'JSR :abs) (defop #x21 'AND :izx) (defop #x24 'BIT :zp)
(defop #x25 'AND :zp)  (defop #x26 'ROL :zp)  (defop #x28 'PLP)      (defop #x29 'AND :imm)
(defop #x2A 'ROL)      (defop #x2C 'BIT :abs) (defop #x2D 'AND :abs) (defop #x2E 'ROL :abs)
(defop #x30 'BMI :rel) (defop #x31 'AND :izy) (defop #x35 'AND :zpx) (defop #x36 'ROL :zpx)
(defop #x38 'SEC)      (defop #x39 'AND :aby) (defop #x3D 'AND :abx) (defop #x3E 'ROL :abx)
(defop #x40 'RTI)      (defop #x41 'EOR :izx) (defop #x45 'EOR :zp)  (defop #x46 'LSR :zp)
(defop #x48 'PHA)      (defop #x49 'EOR :imm) (defop #x4A 'LSR)      (defop #x4C 'JMP :abs)
(defop #x4D 'EOR :abs) (defop #x4E 'LSR :abs) (defop #x50 'BVC :rel) (defop #x51 'EOR :izy)
(defop #x55 'EOR :zpx) (defop #x56 'LSR :zpx) (defop #x58 'CLI)      (defop #x59 'EOR :aby)
(defop #x5D 'EOR :abx) (defop #x5E 'LSR :abx) (defop #x60 'RTS)      (defop #x61 'ADC :izx)
(defop #x65 'ADC :zp)  (defop #x66 'ROR :zp)  (defop #x68 'PLA)      (defop #x69 'ADC :imm)
(defop #x6A 'ROR)      (defop #x6C 'JMP :ind) (defop #x6D 'ADC :abs) (defop #x6E 'ROR :abs)
(defop #x70 'BVS :rel) (defop #x71 'ADC :izy) (defop #x75 'ADC :zpx) (defop #x76 'ROR :zpx)
(defop #x78 'SEI)      (defop #x79 'ADC :aby) (defop #x7D 'ADC :abx) (defop #x7E 'ROR :abx)
(defop #x81 'STA :izx) (defop #x84 'STY :zp)  (defop #x85 'STA :zp)  (defop #x86 'STX :zp)
(defop #x88 'DEY)      (defop #x8A 'TXA)      (defop #x8C 'STY :abs) (defop #x8D 'STA :abs)
(defop #x8E 'STX :abs) (defop #x90 'BCC :rel) (defop #x91 'STA :izy) (defop #x94 'STY :zpx)
(defop #x95 'STA :zpx) (defop #x96 'STX :zpy) (defop #x98 'TYA)      (defop #x99 'STA :aby)
(defop #x9A 'TXS)      (defop #x9D 'STA :abx) (defop #xA0 'LDY :imm) (defop #xA1 'LDA :izx)
(defop #xA2 'LDX :imm) (defop #xA4 'LDY :zp)  (defop #xA5 'LDA :zp)  (defop #xA6 'LDX :zp)
(defop #xA8 'TAY)      (defop #xA9 'LDA :imm) (defop #xAA 'TAX)      (defop #xAC 'LDY :abs)
(defop #xAD 'LDA :abs) (defop #xAE 'LDX :abs) (defop #xB0 'BCS :rel) (defop #xB1 'LDA :izy)
(defop #xB4 'LDY :zpx) (defop #xB5 'LDA :zpx) (defop #xB6 'LDX :zpy) (defop #xB8 'CLV)
(defop #xB9 'LDA :aby) (defop #xBA 'TSX)      (defop #xBC 'LDY :abx) (defop #xBD 'LDA :abx)
(defop #xBE 'LDX :aby) (defop #xC0 'CPY :imm) (defop #xC1 'CMP :izx) (defop #xC4 'CPY :zp)
(defop #xC5 'CMP :zp)  (defop #xC6 'DEC :zp)  (defop #xC8 'INY)      (defop #xC9 'CMP :imm)
(defop #xCA 'DEX)      (defop #xCC 'CPY :abs) (defop #xCD 'CMP :abs) (defop #xCE 'DEC :abs)
(defop #xD0 'BNE :rel) (defop #xD1 'CMP :izy) (defop #xD5 'CMP :zpx) (defop #xD6 'DEC :zpx)
(defop #xD8 'CLD)      (defop #xD9 'CMP :aby) (defop #xDD 'CMP :abx) (defop #xDE 'DEC :abx)
(defop #xE0 'CPX :imm) (defop #xE1 'SBC :izx) (defop #xE4 'CPX :zp)  (defop #xE5 'SBC :zp)
(defop #xE6 'INC :zp)  (defop #xE8 'INX)      (defop #xE9 'SBC :imm) (defop #xEA 'NOP)
(defop #xEC 'CPX :abs) (defop #xED 'SBC :abs) (defop #xEE 'INC :abs) (defop #xF0 'BEQ :rel)
(defop #xF1 'SBC :izy) (defop #xF5 'SBC :zpx) (defop #xF6 'INC :zpx) (defop #xF8 'SED)
(defop #xF9 'SBC :aby) (defop #xFD 'SBC :abx) (defop #xFE 'INC :abx)

(defun assert-address (add)
  (assert (>= add 0))
  (assert (< add 65536)))

(defun peek-byte (add)
  (assert-address add)
  (aref *compiler-buffer* add))

(defun hexdump (add len)
  (assert-address add)
  (loop 
     for i from add to (min (+ add len) 65535)
     for j from 1 do
       (when (= 1 (rem j 16))
	 (format t "~4,'0X " i))
       (format t "~2,'0X" (peek-byte i))
       (when (zerop (rem j 2))
	 (format t " "))
       (when (zerop (rem j 16))
	 (loop for k from i downto (- i 16) do
	      (format t "~c" (let ((c (peek-byte i)))
			       (if (and (> c 31)
					(< c 128))
				   (code-char c)
				   #\.))))
	 (terpri)))
  (values))

(defun assert-byte (byte)
  (assert (>= byte 0))
  (assert (< byte 256)))

(defun org (add)
  (assert-address add)
  (setf *compiler-ptr* add))

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
    (assert (numberp byte) nil (format nil "Opcode:~a Addressing Mode:~a is invalid" op mode))
    (push-byte byte)))

(defun label-address (obj)
  (gethash obj *compiler-labels*))

;; assembler instructions

(defun label (obj)
  (let ((add (label-address obj)))
    (if add
	(assert (= add *compiler-ptr*)) ;labels must be the same
	(setf (gethash obj *compiler-labels*) *compiler-ptr*))))

;; 6502 instructions by mode

(defun imp (op)
  (push-op op :imp))

(defun rel (op label)
  (push-op op :rel)
  (let ((add (label-address label)))
    (push-sbyte (if add (- add
			   (1+ *compiler-ptr*))
		    0))))

(defun reset-compiler (&optional (buffer-size 65536))
  (setf *compiler-ptr* 0)
  (setf *compiler-labels* (make-hash-table :test 'equal))
  (setf *compiler-buffer* (make-array buffer-size
				     :element-type '(unsigned-byte 8)
				     :initial-element 0))
  (values))

(reset-compiler)

(defun test ()
  (reset-compiler)
  (dotimes (x 2)
    (org #x600)
    (rel 'BCC :the-future)
    (imp 'CLD)
    (imp 'CLD)
    (label :the-future)
    (imp 'CLD)
    *compiler-buffer*)
  (hexdump #x600 100))
