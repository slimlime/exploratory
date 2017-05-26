;monitor

;keys q     - quit
;keys space - step

;step function
(defparameter *monitor-step* nil)
;function to return values containing pc, sp, sr, a, x, y
(defparameter *monitor-get-state* nil)
;todo change to lines
(defparameter *monitor-context-bytes* 8)
(defparameter *monitor-watches* nil)

(defun monitor-print ()
  (multiple-value-bind (buffer pc sp sr a x y)
      (funcall *monitor-get-state*)
    (let ((*compiler-buffer* buffer))
      (format t "-- Stack ----------------------------------------------------~%")
      (hexdump (+ sp #x101) 16)
      (when *monitor-watches*
	(format t "-- Watches --------------------------------------------------~%"))
      (dolist (a *monitor-watches*)
	(hexdump (car a) (cdr a)))
      (format t "-- PC -------------------------------------------------------~%")
      (hexdump pc 32)
      (format t "-------------------------------------------------------------~%")
      (format t "                          SV BDIZC~%")
      (format t "       PC:~4,'0X SP:1~2,'0X  SR:~8,'0b" pc sp sr)
      (format t " A:~2,'0X X:~2,'0X Y:~2,'0X~%" a x y)
      (format t "-------------------------------------------------------------~%")
      (disassemble-6502 pc (+ pc *monitor-context-bytes*)))))

(defun monitor-watch (addr len)
    (push (cons (resolve addr) len) *monitor-watches*))

(defun monitor-step ()
  (funcall *monitor-step*)
  (monitor-print))

(defun monitor-setup-for-cl-6502 ()
  ;temporary binding to cl-6502
  (setf (cl-6502:get-range 0) *compiler-buffer*)
  (setf (6502:cpu-pc cl-6502:*cpu*) #x600)
  (setf *monitor-step* 
	#'(lambda () (cl-6502:step-cpu 
		      cl-6502:*cpu* 
		      (cl-6502:get-byte (6502:cpu-pc cl-6502:*cpu*)))))
  (setf *monitor-get-state* 
	#'(lambda () (values
		      (cl-6502:get-range 0)
		      (6502:cpu-pc cl-6502:*cpu*)
		      (6502:cpu-sp cl-6502:*cpu*)
		      (6502:cpu-sr cl-6502:*cpu*)
		      (6502:cpu-ar cl-6502:*cpu*)
		      (6502:cpu-xr cl-6502:*cpu*)
		      (6502:cpu-yr cl-6502:*cpu*)))))

(defun monitor-test ()
  (reset-compiler)

  (dotimes (pass 2)
    (setf *compiler-ensure-labels-resolve* (= pass 1))
    
    (org #x0600)

    (label :start)
    (CLD)
    (LDY.IMM #x10)
    (label :next)
    (LDA.ABY :string)
    (JSR :print)
    (dc "Decrement the index")
    (DEY)
    (BNE :next)
    (ds :string "Hello World!")
    (NOP)
    (NOP)
    (BRK)
    (label :print)
    (TAX)
    (RTS)
    (label :end)
    (monitor-setup-for-cl-6502))
  (values))
