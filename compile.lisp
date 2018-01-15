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
(defparameter *compiler-namespace-depth* 0)
(defparameter *compiler-namespace-stack* nil)
(defparameter *compiler-namespace-sizes* nil)

; todo A way of providing a 'spare byte/word' to the compile
; which can then be used later e.g.

; (unused-b addr) - tell the compiler this byte is spare
; (use-b :label)  - tell the compiler to use the next spare byte for a label
; YAGGERS until we are out of memory!

; WIBNI
					; Assert if final page jump bug

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
  (setf *compiler-namespace-depth* 0)
  (setf *compiler-namespace-stack* nil)
  (setf *compiler-namespace-sizes* (make-hash-table :test 'equal))

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

(defun peek-last-byte ()
  (peek-byte (1- *compiler-ptr*)))

(defun resolve (arg &key (no-assert nil))
  (if (numberp arg)
      (values arg t)
      (let ((addr nil)
	    (alias nil))
	;;look for an alias
	(if (consp arg)
	    ;;for a qualified alias we just resolve
	    (setf alias (gethash arg *compiler-aliases*))
	    ;;otherwise, go up the nested stack of namespaces
	    (dolist (ns *compiler-namespace-stack*)
	      (setf alias (gethash (cons ns arg) *compiler-aliases*))
	      (when alias (return))))
	;;we have found an alias, which should give a fully qualified
	;;label we can immediately resolve
	;;TODO check for circular aliases..
	(when alias
	  (setf addr (resolve alias)))
	  ;;(setf addr (gethash alias *compiler-labels*)))
	(unless addr
	  (if (consp arg)
	      ;;for a qualified label we just resolve
	      (setf addr (gethash arg *compiler-labels*))
	      ;;otherwise go up the nested stack of namespaces
	      (progn
		(dolist (ns *compiler-namespace-stack*)
		  ;;(format t "Checking ~a:~a~%" ns arg)
		  (setf addr (gethash (cons ns arg) *compiler-labels*))
		  (when addr (return)))
		(unless addr
		  ;;finally, check in the global namespace
		  (setf addr (gethash arg *compiler-labels*))))))
	;; on the first pass only, allow
	;; labels to be null (resolve to 0 if so)
	(when (and (null addr)
		   (null no-assert)
		   *compiler-final-pass*)
	  (assert nil nil (format nil "The label ~a was not resolved (aliased to ~a, in namespace ~a)" arg alias *compiler-label-namespace*)))
	(if addr
	    (values addr t)
	    (values 0 nil)))))

(defun resolves (label)
  (multiple-value-bind (addr resolves)
      (resolve label :no-assert t)
    (declare (ignorable addr))
    resolves))

(defun resolves-to-zpg (label)
  (and
   (resolves label)
   (< (resolve label :no-assert t) 256)))

(defun hexdump (add &optional (len 32))
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

(defun to-ubyte (sbyte)
  (if (< sbyte 0)
      (+ sbyte 256)
      sbyte))

(defun push-sbyte (byte)
  (push-byte (to-ubyte byte)))

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
  (unless (consp alias)
    (when *compiler-label-namespace*
      (setf alias (cons *compiler-label-namespace* alias))))
  (unless (consp label)
    ;; now try to find which label we are actually talking about
    (dolist (ns *compiler-namespace-stack*)
      (when (gethash (cons ns label) *compiler-labels*)
	(setf label (cons ns label)))))
  (setf (gethash alias *compiler-aliases*) label))

;;this function will ensure that a set of aliases have no members
;;in another set of aliases. Used in conjunction with get-aliases
;;this can tell us whether there will be a clash between 'functions'
(defun ensure-aliases-different (these those &key (no-assert nil))
  (when *compiler-final-pass*
    (unless no-assert
      ;;probably a typo in the namespace
      (assert these)
      (assert those))
    (let ((these-addresses (make-hash-table)))
      (dolist (this these)
	(setf this (resolve this))
	(setf (gethash this these-addresses) this))
      (dolist (that those)
	(assert (null (gethash (resolve that) these-addresses)) nil
		(format nil "Alias clash '~a'. These aliases ~a. Those aliases ~a"
			that these those))))))

	
(defun db (label byte &rest bytes)
  (setf bytes (cons byte bytes))
  (add-hint (length bytes)
	    (format nil "DB ~{$~2,'0X~^, ~}" bytes))
  (when label (label label))
  (dolist (byte bytes)
    (push-byte byte)))

(defun dbs (label count &optional (byte 0))
  (add-hint count (format nil "DBS (~a)" count))
  (when label (label label))
  (dotimes (i count)
    (push-byte byte)))

(defun dw (label word &rest words)
  (setf words (mapcar #'resolve (cons word words)))
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

(defun dc-1 (comment ptr postfix)
  (when *compiler-final-pass*
    (when *compiler-debug*
      (print comment))
    (let ((*compiler-comments* (if postfix
				   *compiler-postfix-comments*
				   *compiler-comments*)))
      (let ((comments (gethash ptr *compiler-comments*)))
	(if comments
	    (unless (member comment comments :test 'equal)
	      (setf (gethash ptr *compiler-comments*)
		    (cons comment comments)))
	    (setf (gethash ptr *compiler-comments*) (list comment)))))))

(defun dc (comment &optional (postfix nil))
  (dc-1 comment *compiler-ptr* postfix))

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

(defun rts-jmp-hi (addr)
  "Return the high byte of an address"
  (unless (numberp addr)
    (dc (format nil "RTS-JMP-HI : ~a" (fmt-label addr t)) t))
  (ash (1- (resolve addr)) -8))

(defun rts-jmp-lo (addr)
  "Return the lo byte of an address"
  (unless (numberp addr)
    (dc (format nil "RTS-JMP-LO : ~a" (fmt-label addr t)) t))
  (logand #xFF (1- (resolve addr))))

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

(defun inc-namespace-bytes (namespace bytes)
  (unless namespace
    (setf namespace 'global))
  (let ((size (gethash namespace *compiler-namespace-sizes*)))
    (setf (gethash namespace *compiler-namespace-sizes*)
	  (if size (+ size bytes) bytes))))

(defun dump-namespaces ()
  (let ((entries nil))
  (maphash #'(lambda (namespace bytes)
	       (push (cons namespace bytes) entries))
	   *compiler-namespace-sizes*)
  (dolist (entry (sort entries #'> :key #'cdr))
    (format t "~a (~a)~%" (car entry) (cdr entry)))))

;;todo this way of defining a macro is a bit troublesome, I think
;;this can be gleaned from the fact that the parameters are immediately
;;evaluated and that the body could just be funcalled. The immediate advantage
;;would be recompilation.
(defmacro with-namespace (namespace &body body)
  "In the scope of the macro, define labels in the namespace
   Label resolution will be done preferentially in this namespace
   but will fall back to the global namespace if not found"
  (let ((ns (gensym))
	(ptrsym (gensym)))
    `(let* ((,ns ,namespace)
	    (,ptrsym *compiler-ptr*)
	    ;;could get rid of this namespace as it is just the car of the stack
	    (*compiler-label-namespace* ,ns))
       (push ,ns *compiler-namespace-stack*)
       ,@body
       (pop *compiler-namespace-stack*)
       (when *compiler-final-pass*
	 (inc-namespace-bytes ,ns (- *compiler-ptr* ,ptrsym))))))

(defmacro with-local-namespace (&body body)
  "All labels in this scope will resolve to the instantation
   at the current location"
  `(with-namespace (format nil "$~4,'0X" *compiler-ptr*)
     ,@body))

(defmacro measure-size (name &body body)
  (let ((ptrsym (gensym)))
    `(let ((,ptrsym *compiler-ptr*))
       ,@body
       (when *compiler-final-pass*
	 (format t "~a size ~a~%"
		 ,name
		 (- *compiler-ptr* ,ptrsym))))))

(defun label-namespace (label)
  (if (consp label)
      (car label)
      nil))

;;return the aliases labels for a namespace
;;might be useful in seeing which are used in a 'function'
(defun get-aliased-labels (namespace)
  (let ((labels nil))
    (maphash #'(lambda (k v)
		 (when (equal (label-namespace k)
			      namespace)
		   (push v labels)))
	     *compiler-aliases*)
    labels))

(defun dump-aliases ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" (fmt-label k t) v)) *compiler-aliases*))

(defun dump-labels (&optional (namespace nil namespace-p))  
  (maphash #'(lambda (k v)
	       (when (or (not namespace-p)
			 (equal (label-namespace k)
				namespace))
		 (format t "~a -> ~4,'0X~%" (fmt-label k t) v)))
	       *compiler-labels*))


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
	  (let ((offset (- (resolve label) (1+ *compiler-ptr*))))
	    (assert (>= offset #x-80) nil "Relative offset is < -#x80")
	    (assert (<= offset #x7f) nil "Relative offset is > #x7f")
	    (push-sbyte offset))
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

;; page agnostic addressing, provide an instruction of the form OP.* which
;; resolves to either zero page or absoulte depending on how the address resolves

(defun concat-symbol (symbol str)
  (intern (concatenate 'string (symbol-name symbol) str)))

(defmacro page-agnostic (op)
  `(defun ,(concat-symbol op ".*") (addr &optional comment)
     (if (resolves-to-zpg addr)
	 (,(concat-symbol op ".ZP") addr comment)
	 (,(concat-symbol op ".AB") addr comment))))

(page-agnostic ORA) (page-agnostic ASL) (page-agnostic AND) (page-agnostic BIT)
(page-agnostic ROL) (page-agnostic EOR) (page-agnostic LSR) (page-agnostic ADC)
(page-agnostic ROR) (page-agnostic STA) (page-agnostic STY) (page-agnostic STX)
(page-agnostic LDA) (page-agnostic LDX) (page-agnostic LDY) (page-agnostic CPY)
(page-agnostic CMP) (page-agnostic SBC) (page-agnostic CPX) (page-agnostic INC)
(page-agnostic DEC)

(reset-compiler)

;;;; Disassembler

(defun right-justify (s)
  (subseq (format nil "                    ~a" s)
	  (length s)))

(defun disassemble-6502 (start end &key (buffer nil))
  (when (null buffer) (setf buffer *compiler-buffer*))
  (let ((*compiler-final-pass* t))
    (setf start (resolve start))
    (setf end (resolve end)))
  (let ((lab (make-hash-table)))		
    ;;invert the label table i.e. lookup label by address
    ;;todo, additional labels at the same address are currently hidden
    (maphash #'(lambda (label addr) (push label (gethash addr lab)))
	     *compiler-labels*)
    (loop for i from start to end do
	 (let* ((opcode (gethash (aref buffer i) *reverse-opcodes*))
		(mode (cdr opcode))
		(op (car opcode))
		(arg1 (aref buffer (+ 1 i)))
		(arg2 (aref buffer (+ 2 i))))
	   (let* ((label (gethash i lab))
		  (hint (gethash i *compiler-disassembler-hints*))
		  (comments (gethash i *compiler-comments*))
		  (postfix-comments (gethash i *compiler-postfix-comments*)))
	     (when comments
	       (dolist (comment (reverse comments))
		 (format t "                     ;~a~%" comment)))
	     (when label
	       (dolist (l (cdr label))
		 (format t "~a~%" (right-justify (fmt-label l t)))))
	     (format t "~a ~4,'0X ~2,'0X"
		     (if label
			 (right-justify (fmt-label (car label) t))
			 "                    ")
		     i (aref buffer i))
	     (if (and hint (numberp (car hint)))
		 (progn
					;render hint
		   (incf i (1- (car hint)))
		   ;;todo for smaller hints, whynot just dump the bytes rather
		   ;;than just the first one
		   (format t "..    ~a" (cdr hint)))
					;else render opcodes
		 (labels ((format-label (addr)
			    (let ((label (gethash addr lab)))
			      (when label
				(format t "~45,1T;~{~a ~}"
					(mapcar #'(lambda (l) (fmt-label l t)) label)))))
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

;;e.g. CL-USER> (disassemble-6502-to-list #x600 #x610)
;; ((LDA :IMM 1) (STA :ZP 0) (LDA :IMM 0) (STA :ZP 1) (INC :ZP 0) (BNE :REL 2)
;; (INC :ZP 1) (BRK :IMP) (BRK :IMP) (BRK :IMP))

(defun disassemble-6502-to-list (start end &key (buffer nil))
  (when (null buffer) (setf buffer *compiler-buffer*))
  (let ((output nil)
	(*compiler-final-pass* t))
    (setf start (resolve start))
    (setf end (resolve end))
    (loop for i from start to end do
	 (let* ((opcode (gethash (aref buffer i) *reverse-opcodes*))
		(mode (cdr opcode))
		(op (car opcode))
		(arg1 (aref buffer (+ 1 i)))
		(arg2 (aref buffer (+ 2 i))))
	   (let* ((hint (gethash i *compiler-disassembler-hints*)))
	     (if (and hint (numberp (car hint)))
		 (incf i (1- (car hint)))
		 (labels ((format-1 () (push (list op mode arg1) output) (incf i))
			  (format-2 () (push (list op mode arg1 arg2) output) (incf i 2)))
		   (case mode
		     (:imm (format-1))
		     (:rel (format-1))
		     (:imp (push (list op mode) output))
		     (:ab (format-2))
		     (:zp (format-1))
		     (:ind (format-2))
		     (:izx (format-1))
		     (:izy (format-1))
		     (:zpx (format-1))
		     (:zpy (format-1))
		     (:aby (format-2))
		     (:abx (format-2))))))))
    (nreverse output)))
