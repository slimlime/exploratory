;;very simple code analysis

(defparameter *opcode-effects* (make-hash-table))

(defun add-effects (op flag-effects)
  (setf (gethash op *opcode-effects*) flag-effects))

(defparameter *valid-flags* '(S V Z C I D))

(add-effects 'ADC '(S V Z C))
(add-effects 'AND '(S Z))
(add-effects 'ASL '(S Z C))
(add-effects 'BIT '(S V Z))
(add-effects 'BRK '(*))
(add-effects 'CMP '(S Z C))
(add-effects 'CPX '(S Z C))
(add-effects 'CPY '(S Z C))
(add-effects 'DEC '(S Z))
(add-effects 'EOR '(S Z))
(add-effects 'CLC '(C))
(add-effects 'SEC '(C))
(add-effects 'CLI '(I))
(add-effects 'SEI '(I))
(add-effects 'CLV '(V))
(add-effects 'CLD '(D))
(add-effects 'SED '(D))
(add-effects 'INC '(S Z))
(add-effects 'JMP '(*))
(add-effects 'JSR '(*))
(add-effects 'LDA '(S Z))
(add-effects 'LDX '(S Z))
(add-effects 'LDY '(S Z))
(add-effects 'LSR '(S Z C))
(add-effects 'ORA '(S Z))
(add-effects 'TAX '(S Z))
(add-effects 'TXA '(S Z))
(add-effects 'DEX '(S Z))
(add-effects 'INX '(S Z))
(add-effects 'TAY '(S Z))
(add-effects 'TYA '(S Z))
(add-effects 'DEY '(S Z))
(add-effects 'INY '(S Z))
(add-effects 'ROL '(S Z C))
(add-effects 'ROR '(S Z C))
(add-effects 'RTI '(*))
(add-effects 'RTS '(*))
(add-effects 'SBC '(S V Z C))

(defun opcodes-flag-effects (opcodes)
  "Get all flag effects for a list of opcodes"
  (let ((effects nil))
    (dolist (op opcodes)
      (setf effects (union effects (gethash op *opcode-effects*))))
    effects))

(defun opcodes-affect-flag (flag opcodes)
  "Return if a single flag is affected by a list of opcodes"
  (dolist (op opcodes)
    (when (or
	   (find flag (gethash op *opcode-effects*))
	   (find '* (gethash op *opcode-effects*)))
      (return-from opcodes-affect-flag flag)))
  nil)

(defun code-affects-flag (flag start end)
  (assert (find flag *valid-flags*) nil (format nil "Unknown ~a flag" flag))
  (when *compiler-debug*
    (format t "Code affects flag ~a~%" flag)
    (disassemble-6502 start end))
  (let ((val
	 (opcodes-affect-flag flag (mapcar #'car (disassemble-6502-to-list start end)))))
    (when *compiler-debug*
      (format t "Code affects flag ~a returns ~a~%" flag val))
    val))
