;; shared VIC-II parameters

;; 0 - black   1 - white       2 - red        3 - cyan
;; 4 - purple  5 - green       6 - blue       7 - yellow
;; 8 - orange  9 - brown       A - light red  B - dark grey
;; C - grey    D - light green E - light blue F - light grey

(defparameter *c64-colours* #(#x000000 #xFFFFFF #x68372B #x70A4B2
			      #x6F3D86 #x588D43 #x352879 #xB8C76F
			      #x6F4F25 #x433900 #x9A6759 #x444444
			      #x6C6C6C #x9AD284 #x6C5EB5 #x959595))

(defparameter *screen-memory-address* #x8000)
(defparameter *char-memory-address* #x7000)

(defconstant +char-memory-length+ 1000)
(defconstant +screen-memory-length+ 8000)
(defconstant +screen-width+ 320)
(defconstant +screen-width-bytes+ 40)
(defconstant +screen-height+ 200)

(defun scradd (row col)
  (+ *screen-memory-address* (* +screen-width-bytes+ row) col))

;; now the console shared memory stuff

;; map the entire memory space for VICKY
;; todo make a headless mode

(defparameter *buffer-fd* nil)
(defparameter *buffer-sap* nil)
(defparameter *buffer-length* #x10000)
(defvar *vicky-instance* "6502-instance-0")

(defun setmem (addr byte)
  (assert (>= addr 0))
  (assert (< addr 65536))
  (when *buffer-sap*
    (setf (sb-sys:sap-ref-8 *buffer-sap* addr) byte)))

(defun unenable-vicky ()
  (when *buffer-sap*
    (sb-posix:munmap *buffer-sap* *buffer-length*)
    (setf *buffer-sap* nil))
  (when *buffer-fd*
    (close *buffer-fd*)
    (setf *buffer-fd* nil)))

(defun enable-vicky ()
  (unenable-vicky)
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
  (when *buffer-sap*
    (loop for i from 0 to (1- *buffer-length*) do
	 (setmem i (aref src i)))))

(defun clrmem ()
  (when *buffer-sap*
    (loop for i from 0 to (1- *buffer-length*) do
	 (setmem i 0))))
