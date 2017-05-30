(defparameter *screen-buffer-fd* nil)
(defparameter *screen-buffer-sap* nil)
(defparameter *screen-buffer-length* 8192)

(defun unmap-screen ()
  (when *screen-buffer-sap*
    (sb-posix:munmap *screen-buffer-sap* *screen-buffer-length*)
    (setf *screen-buffer-sap* nil))
  (when *screen-buffer-fd*
    (close *screen-buffer-fd*)
    (setf *screen-buffer-fd* nil)))

(defun map-screen ()
  (unmap-screen)
  (setf *screen-buffer-fd* (open "screen-buffer" :direction :io :element-type '(unsigned-byte 8)
				 :if-exists :overwrite
				 :if-does-not-exist :create))
  (file-position *screen-buffer-fd* *screen-buffer-length*)
  (write-byte 0 *screen-buffer-fd*)
  (file-position *screen-buffer-fd* 0)
  (setf *screen-buffer-sap*
	(sb-posix:mmap nil *screen-buffer-length* (logior sb-posix:prot-read
							  sb-posix:prot-write)
		       sb-posix:map-shared *screen-buffer-fd* 0))
  (close *screen-buffer-fd*))

(defun copy-to-screen-buffer (src offset)
  (unless *screen-buffer-sap*
    (map-screen))
;todo - use pinned buffer and sb-sys copy function. Also, make memory
;       or make memory driver in emulator access this directly
  (loop for i from 0 to (1- *screen-buffer-length*) do
       (setf (sb-sys:sap-ref-8 *screen-buffer-sap* i)
	     (aref src (+ i offset)))))
