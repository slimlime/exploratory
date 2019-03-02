;monitor

;; ideas
;; a watch variable function
;; a local variables dump, which basically sees which namespace we
;; are in and dumps all aliased zero-page variables
;; TODO monitor-reset-debug-hooks would not be necessary if the compiler were
;; modular and each module could be registered. Having said that, all that will
;; happen is that a call to monitor-reset will be replaced with a call to
;; monitor-register with compiler ;-

;;step function
(defparameter *monitor-step* nil)
;;function to return values containing pc, sp, sr, a, x, y
(defparameter *monitor-get-state* nil)
(defparameter *monitor-context-bytes* 16)
(defparameter *monitor-watches* nil)
(defparameter *monitor-buffer* nil)
(defparameter *monitor-peek-fn* nil)
(defparameter *monitor-poke-fn* nil)
(defparameter *monitor-label-watches* nil)
(defparameter *monitor-profile* nil)
(defparameter *monitor-debug-hooks* (make-hash-table))

(defun monitor-reset-debug-hooks ()
  (clrhash *monitor-debug-hooks*))

(defun monitor-add-debug-hook-fn (addr fn)
  "Add a hook which will be executed when the label is reached"
  (when *compiler-final-pass*
    (push fn (gethash addr *monitor-debug-hooks*))))

(defmacro dbg (&body body)
  "Execute the body of the hook when the address is reached
   The following variables are bound buffer, pc, sp, sr, a, x, y and cc"
  `(monitor-add-debug-hook-fn *compiler-ptr*
     (λ (buffer pc sp sr a x y cc)
	 (declare (ignorable buffer pc sp sr a x y cc))
	 ,@body)))

(defun monitor-execute-hooks (buffer pc sp sr a x y cc)
  "Execute all hooks at the address"
  (aif (gethash pc *monitor-debug-hooks*)
       (dolist (hook it)
	   (funcall hook buffer pc sp sr a x y cc))))
  
(defun monitor-reset-profile ()
  (setf *monitor-profile* (make-hash-table)))

(monitor-reset-profile)

(defun monitor-profile-address (addr cycles)
  (if (gethash addr *monitor-profile*)
      (incf (gethash addr *monitor-profile*))
      (setf (gethash addr *monitor-profile*) cycles)))

(defun monitor-dump-profile (&optional (top 4) (context 5))
  (let ((profile nil))
    (maphash (λ (addr cycles) (push (cons addr cycles) profile))
	     *monitor-profile*)
    (loop for e in (sort profile #'> :key #'cdr)
	 for _ from 1 to top do
	 (format t "Address:~4,'0X Total Cycles:~6d~%" (car e) (cdr e))
	 (format t "|------------------------------~%")
	 (disassemble-6502 (car e) (+ (car e) context 5))
	 (terpri))))

(defun hex (number)
  (format t "~4,'0X" number))

;; TODO Add locals, which gets all the symbols in the namespace where
;; we currently are

;; TODO using multiple-value-bind to get the properties is horrid

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
  (maphash (λ (k v) (declare (ignore v))
	       (when (equal (label-namespace k) namespace)
		 (monitor-watch k)
		 (format t "Watched ~a~%" (fmt-label k t))))
	       *compiler-labels*))

(defun monitor-step ()
  (funcall *monitor-step*)
  (monitor-print))

(defun monitor-cc ()
  (multiple-value-bind (buffer pc sp sr a x y cc)
      (funcall *monitor-get-state* nil)
    (declare (ignore buffer pc sp sr a x y))
    cc))

(defun monitor-pc ()
  (multiple-value-bind (buffer pc sp sr a x y cc)
      (funcall *monitor-get-state* nil)
    (declare (ignore buffer cc sp sr a x y))
    pc))

;;TODO break-on should be able to match an unqualified label
(defun monitor-run (&key (break-on 'BRK) (max-cycles 2000000) (print t))
  (monitor-reset-profile)
  (multiple-value-bind (buffer pc sp sr a x y cc)
      (funcall *monitor-get-state*)
    (declare (ignore sp pc sr a x y))
    (let ((*compiler-buffer* buffer)
	  (start-cycles cc)
	  (total-cycles 0))
      (do ()
	  ((> total-cycles max-cycles))
	(multiple-value-bind (buffer pc sp sr a x y cc)
	    (funcall *monitor-get-state* nil)
	  (monitor-execute-hooks buffer pc sp sr a x y cc)
	  (setf total-cycles (- cc start-cycles))
	  ;;THIS IS USING THE COMPILER BUFFER IT SHOULD PROBABLY
	  ;;LOOK INTO THE 6502 BUFFER AS IT WON'T WORK FOR SELF MODDING
	  (let ((op (gethash (aref *compiler-buffer* pc)
			     *reverse-opcodes*)))
	    (when (or (= pc (resolve break-on :no-assert t))
		      (and op (eq break-on (car op))))
	      (return)))
	  (let ((step-cycles (monitor-cc)))
	    (funcall *monitor-step*)
	    (monitor-profile-address pc (- (monitor-cc) step-cycles)))))
      (when print
	(format t "Cycles:~a~%" total-cycles)
	(monitor-print)))))

(defun monitor-go ()
  (monitor-print)
  (tagbody
   :prompt
     (format t "Ret=Step q=Quit label/address=Run 3=Skip 3 5=Skip 5~%")
     (let ((line (read-line)))
       (if (equalp "3" line)
	   ;;TODO, can we detect dereferencing functions?
	   ;;TODO use case
	   (monitor-run :break-on (+ 3 (monitor-pc)))
	   (if (equalp "5" line)
	       (monitor-run :break-on (+ 5 (monitor-pc)))
	       (if (equalp "q" line)
		   (return-from monitor-go (values))
		   (if (equal "" line)
		       (monitor-step)
		       (let ((exp (with-input-from-string (s line) (read s))))
			 (if (resolves exp)
			     (monitor-run :break-on exp)
			     (format t "~a is not a valid address.~%" exp))))))))
       (go :prompt)))

(defun monitor-buffer ()
  (funcall *monitor-buffer*))

(defun monitor-peek (address)
  (funcall *monitor-peek-fn* address))

(defun monitor-peek-addr (addr)
  (logior (monitor-peek addr)
	  (ash (monitor-peek (1+ addr)) 8)))

(defun monitor-poke (address byte)
  (funcall *monitor-poke-fn* address byte))

(defun monitor-time-fn (fn)
  (let ((cc (monitor-cc)))
    (funcall fn)
    (format t "Cycles:~d~%" (- (monitor-cc) cc))))

(defmacro monitor-time (&body body)
  `(monitor-time-fn (λ () ,@body)))

(defun monitor-setup-for-cl-6502 (buffer org)
  ;temporary binding to cl-6502
  (setf *monitor-buffer* (λ () (cl-6502:get-range 0)))
  (setf (cl-6502:get-range 0) buffer)
  (setf (6502:cpu-pc cl-6502:*cpu*) org)
  (setf *monitor-peek-fn* (λ (address)
			      (cl-6502:get-byte address)))
  (setf *monitor-poke-fn* (λ (address byte)
			      (setf (cl-6502:get-byte address) byte)))
  (setf *monitor-step*
	(λ ()
	    (let ((byte (cl-6502:get-byte (6502:cpu-pc cl-6502:*cpu*))))
	      ; BRK seems to fubar cl-6502 next time it is called
	      ; can't be bothered to fix it
	      (unless (zerop byte)
		(cl-6502:step-cpu cl-6502:*cpu* byte)))))
  (setf *monitor-get-state* 
	(λ (&optional (slow-get-buffer t)) (values
					(if slow-get-buffer (cl-6502:get-range 0) nil)
					(6502:cpu-pc cl-6502:*cpu*)
					(6502:cpu-sp cl-6502:*cpu*)
					(6502:cpu-sr cl-6502:*cpu*)
					(6502:cpu-ar cl-6502:*cpu*)
					(6502:cpu-xr cl-6502:*cpu*)
					(6502:cpu-yr cl-6502:*cpu*)
					(6502:cpu-cc cl-6502:*cpu*)))))

(defparameter *monitor-reset* #'monitor-setup-for-cl-6502)

(defun monitor-reset (org &key (buffer *compiler-buffer*))
  (funcall *monitor-reset* buffer (resolve org))
  (values))

(defun monitor-setpc (addr)
  (setf (6502:cpu-pc cl-6502:*cpu*) (resolve addr)))

