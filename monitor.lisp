;monitor

;; ideas
;; a watch variable function
;; a local variables dump, which basically sees which namespace we
;; are in and dumps all aliased zero-page variables


;;step function
(defparameter *monitor-step* nil)
;;function to return values containing pc, sp, sr, a, x, y
(defparameter *monitor-get-state* nil)
(defparameter *monitor-context-bytes* 8)
(defparameter *monitor-watches* nil)
(defparameter *monitor-buffer* nil)
(defparameter *monitor-peek-fn* nil)
(defparameter *monitor-poke-fn* nil)
(defparameter *monitor-label-watches* nil)

(defun hex (number)
  (format t "~4,'0X" number))

;; TODO Add locals, which gets all the symbols in the namespace where
;; we currently are

(defun monitor-print ()
  (multiple-value-bind (buffer pc sp sr a x y)
      (funcall *monitor-get-state*)
    (let ((*compiler-buffer* buffer))
      (when *monitor-label-watches*
	(format t "-- Labels ---------------------------------------------------~%")
	(dolist (watch *monitor-label-watches*)
	  (let ((label (first watch))
		(len (second watch)))
	    (if (numberp label)
		(format t "~4,'0X                    " label)
		(format t "~24a" (fmt-label label t)))
	    (if (resolves label)
		(let ((addr (resolve label)))
		  (hexdump-simple
		   addr
		   (if len
		       len
		       (aif (gethash addr *compiler-disassembler-hints*)
			    (car it) ;;hint length
			    ;;no hint? Assume 2 bytes
			    2))))
		(format t "????~%")))))
      (format t "-- Stack ----------------------------------------------------~%")
      (hexdump (+ sp #x101) 16)
      (when *monitor-watches*
	(format t "-- Watches --------------------------------------------------~%")
	(dolist (a *monitor-watches*)
	  (hexdump (car a) (cdr a))))
      (format t "-- PC -------------------------------------------------------~%")
      (hexdump pc 32)
      (format t "-------------------------------------------------------------~%")
      (format t "                          SV BDIZC~%")
      (format t "       PC:~4,'0X SP:1~2,'0X  SR:~8,'0b" pc sp sr)
      (format t " A:~2,'0X X:~2,'0X Y:~2,'0X~%" a x y)
      (format t "-------------------------------------------------------------~%")
      (disassemble-6502 pc (+ pc *monitor-context-bytes*)))))

(defun monitor-unwatch (addr)
  (if (numberp addr)
      (setf *monitor-watches* 
	    (remove (resolve addr) *monitor-watches* :key #'car))
      (setf *monitor-label-watches*
	    (remove addr *monitor-label-watches* :key #'car))))

(defun monitor-unwatch-all ()
  (setf *monitor-watches* nil))

(defun monitor-watch (addr &optional (len 16 len-supplied-p))
  (monitor-unwatch addr)
  (if (numberp addr)
      (push (cons addr len) *monitor-watches*)
      (push (list addr
		  (if len-supplied-p
		      len
		      nil)) ;;we'll get the hint at the time we need it
	    *monitor-label-watches*)))

(defun monitor-watch-namespace (namespace)
  (maphash #'(lambda (k v) (declare (ignore v))
	       (when (equal (label-namespace k) namespace)
		 (monitor-watch k)
		 (format t "Watched ~a~%" (fmt-label k t))))
	       *compiler-labels*))

(defun monitor-step ()
  (funcall *monitor-step*)
  (monitor-print))

(defun monitor-go ()
  (monitor-print)
  (format t "Press RETURN to step, anthing else followed by RETURN to quit~%")
  (loop while (char= (read-char) #\Newline) do
       (monitor-step)))

;;TODO break-on should be able to match an unqualified label
(defun monitor-run (&key (break-on 'BRK) (max-cycles 1000000) (print t))
  (multiple-value-bind (buffer pc sp sr a x y)
      (funcall *monitor-get-state*)
    (declare (ignore sp pc sr a x y))
    (let ((*compiler-buffer* buffer))
      (loop for i from 1 to max-cycles do
	   (multiple-value-bind (buffer pc sp sr a x y)
	       (funcall *monitor-get-state* nil)
	     (declare (ignore buffer sp sr a x y))
	     ;;THIS IS USING THE COMPILER BUFFER IT SHOULD PROBABLY
	     ;;LOOK INTO THE 6502 BUFFER...
	     (let ((op (gethash (aref *compiler-buffer* pc)
				*reverse-opcodes*)))
	       ;(format t "~2,'0X ~a~%" pc op)
	       (when (or (= pc (resolve break-on :no-assert t))
			 (and op (eq break-on (car op))))
		 (return)))
	     (funcall *monitor-step*)))
	   (when print
	     (monitor-print)))))

(defun monitor-buffer ()
  (funcall *monitor-buffer*))

(defun monitor-peek (address)
  (funcall *monitor-peek-fn* address))

(defun monitor-poke (address byte)
  (funcall *monitor-poke-fn* address byte))

(defun monitor-setup-for-cl-6502 (buffer org)
  ;temporary binding to cl-6502
  (setf *monitor-buffer* #'(lambda () (cl-6502:get-range 0)))
  (setf (cl-6502:get-range 0) buffer)
  (setf (6502:cpu-pc cl-6502:*cpu*) org)
  (setf *monitor-peek-fn* #'(lambda (address)
			      (cl-6502:get-byte address)))
  (setf *monitor-poke-fn* #'(lambda (address byte)
			      (setf (cl-6502:get-byte address) byte)))
  (setf *monitor-step*
	#'(lambda ()
	    (let ((byte (cl-6502:get-byte (6502:cpu-pc cl-6502:*cpu*))))
	      ; BRK seems to fubar cl-6502 next time it is called
	      ; can't be bothered to fix it
	      (unless (zerop byte)
		(cl-6502:step-cpu cl-6502:*cpu* byte)))))
  (setf *monitor-get-state* 
	#'(lambda (&optional (slow-get-buffer t)) (values
					(if slow-get-buffer (cl-6502:get-range 0) nil)
					(6502:cpu-pc cl-6502:*cpu*)
					(6502:cpu-sp cl-6502:*cpu*)
					(6502:cpu-sr cl-6502:*cpu*)
					(6502:cpu-ar cl-6502:*cpu*)
					(6502:cpu-xr cl-6502:*cpu*)
					(6502:cpu-yr cl-6502:*cpu*)))))

(defparameter *monitor-reset* #'monitor-setup-for-cl-6502)

(defun monitor-reset (org &key (buffer *compiler-buffer*))
  (funcall *monitor-reset* buffer (resolve org))
  (values))

(defun monitor-setpc (addr)
  (setf (6502:cpu-pc cl-6502:*cpu*) (resolve addr)))
