;monitor

;step function
(defparameter *monitor-step* nil)
;function to return values containing pc, sp, sr, a, x, y
(defparameter *monitor-get-state* nil)
(defparameter *monitor-context-bytes* 8)
(defparameter *monitor-watches* nil)
(defparameter *monitor-buffer* nil)

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

(defun monitor-unwatch (addr)
  ;todo monitor-watch forgets the original label, so if you
  ;unwatch it later, it won't work if the address has changed
  (setf *monitor-watches* 
	(remove (resolve addr) *monitor-watches* :key #'car)))

(defun monitor-watch (addr &optional (len 16))
  (monitor-unwatch addr)
  (push (cons (resolve addr) len) *monitor-watches*))

(defun monitor-step ()
  (funcall *monitor-step*)
  (monitor-print))

(defun monitor-run (&key (break-on 'BRK) (max-cycles 1000000) (print t))
  (multiple-value-bind (buffer pc sp sr a x y)
      (funcall *monitor-get-state*)
    (declare (ignore sp sr a x y))
    (let ((*compiler-buffer* buffer))
      (loop for i from 1 to max-cycles do
	   (funcall *monitor-step*)
	   (let ((op (gethash (aref *compiler-buffer* pc)
			      *reverse-opcodes*)))
	     (when (and op (eq break-on (car op)))
	       (return))))
      (when print
	(monitor-print)))))

(defun monitor-buffer ()
  (funcall *monitor-buffer*))

(defun monitor-setup-for-cl-6502 (buffer org)
  ;temporary binding to cl-6502
  (setf *monitor-buffer* #'(lambda () (cl-6502:get-range 0)))
  (setf (cl-6502:get-range 0) buffer)
  (setf (6502:cpu-pc cl-6502:*cpu*) org)
  (setf *monitor-step* 
	#'(lambda ()
	    (let ((byte (cl-6502:get-byte (6502:cpu-pc cl-6502:*cpu*))))
	      ; BRK seems to fubar cl-6502 next time it is called
	      ; can't be bothered to fix it
	      (unless (zerop byte)
		(cl-6502:step-cpu cl-6502:*cpu* byte)))))
  (setf *monitor-get-state* 
	#'(lambda () (values
		      (cl-6502:get-range 0)
		      (6502:cpu-pc cl-6502:*cpu*)
		      (6502:cpu-sp cl-6502:*cpu*)
		      (6502:cpu-sr cl-6502:*cpu*)
		      (6502:cpu-ar cl-6502:*cpu*)
		      (6502:cpu-xr cl-6502:*cpu*)
		      (6502:cpu-yr cl-6502:*cpu*)))))

(defparameter *monitor-reset* #'monitor-setup-for-cl-6502)

(defun monitor-reset (org &key (buffer *compiler-buffer*))
  (funcall *monitor-reset* buffer org))
