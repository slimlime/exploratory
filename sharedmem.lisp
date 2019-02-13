;; map the entire memory space for VICKY
;; todo make a headless mode

(defparameter *buffer-fd* nil)
(defparameter *buffer-sap* nil)
(defparameter *buffer-length* #x10000)
(defvar *vicky-instance* "6502-instance-0")

(defun setmem (addr byte)
  (assert (>= addr 0))
  (assert (< addr 65536))
  (setf (sb-sys:sap-ref-8 *buffer-sap* addr) byte))

(defun getmem (addr)
  (assert (>= addr 0))
  (assert (< addr 65536))
  (sb-sys:sap-ref-8 *buffer-sap* addr))

(defun unmap-memory ()
  (when *buffer-sap*
    (sb-posix:munmap *buffer-sap* *buffer-length*)
    (setf *buffer-sap* nil))
  (when *buffer-fd*
    (close *buffer-fd*)
    (setf *buffer-fd* nil)))

(defun map-memory ()
  (unmap-memory)
  (setf *buffer-fd* (open (concatenate 'string
				       "/dev/shm/"
				       *vicky-instance*)
			  :direction :io :element-type '(unsigned-byte 8)
			  :if-exists :overwrite
			  :if-does-not-exist :create))
  (file-position *buffer-fd* *buffer-length*)
  (write-byte 0 *buffer-fd*)
  (file-position *buffer-fd* 0)
  (setf *buffer-sap*
	(sb-posix:mmap nil *buffer-length* (logior sb-posix:prot-read
							  sb-posix:prot-write)
		       sb-posix:map-shared *buffer-fd* 0))
  (close *buffer-fd*))

(defun setmem-copy (src)
  (unless *buffer-sap*
    (map-memory))
;todo - use pinned buffer and sb-sys copy function. Also, make memory
;       or make memory driver in emulator access this directly
  (loop for i from 0 to (1- *buffer-length*) do
       (setmem i (aref src i))))

(defun clrmem ()
  (unless *buffer-sap*
    (map-memory))
  (loop for i from 0 to (1- *buffer-length*) do
       (setmem i 0)))
