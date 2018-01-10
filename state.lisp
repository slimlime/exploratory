;; a list of named address ranges containing state we want to
;; save in a game

(defparameter *game-state-addresses* nil)

;; TODO Monitor Poke! Which is missing..

(defun add-to-state-addresses (name is-bits start end)
  (when (> end start)
    (setf (gethash name *game-state-addresses*)
	  (list start (1- end) is-bits))))

;;This macro marks all the bytes emitted inside it as part of the
;;game state. Bytes may be emitted into main memory or into the
;;zero page, hence the two calls to add-to-state-addresses

(defun game-state-fn (name is-bits body-fn)
  (let ((start *compiler-ptr*)
	(zp-start *compiler-zp-free-slot*))
    (dc (format nil "+ ~a" name))
    (funcall body-fn)
    (dc (format nil "- ~a" name))
    (add-to-state-addresses name is-bits zp-start *compiler-zp-free-slot*)
    (add-to-state-addresses name is-bits start *compiler-ptr*)))

(defmacro game-state-bytes (name &body body)
  `(game-state-fn ,name nil #'(lambda () ,@body)))

(defmacro game-state-bits (name &body body)
  `(game-state-fn ,name t #'(lambda () ,@body)))

(defun reset-game-state-ranges ()
  (setf *game-state-addresses* (make-hash-table :test 'equal)))

(reset-game-state-ranges)

(defun dump-state-ranges ()
  (maphash #'(lambda (name range)
	       (format t "$~4,'0X-$~4,'0X ~a~%"
		       (first range)
		       (second range)
		       name))
	   *game-state-addresses*))

(defun dump-state-base64 (&optional (pack-bits t))
  (let ((data (make-array 0
			  :adjustable t
			  :fill-pointer 0
			  :element-type '(unsigned-byte 8))))
    (maphash #'(lambda (name range)
		 (declare (ignorable name))
		 (if (and pack-bits (third range))
		     (let ((byte 0)
			   (bit 0))
		       ;;pack the bits
		       (loop for address from (first range) to (second range) do
			    (setf byte (logior (ash byte 1)
					       (if (= 0 (peek-byte address))
						   1
						   0)))
			    (incf bit)
			    (when (= 8 bit)
			      (vector-push-extend byte data)
			      (setf bit 0)))
		       (when (/= 0 bit)
			 ;;push in the remaining bits
			 (vector-push-extend byte data)))
		     (loop for address from (first range) to (second range) do
			  (vector-push-extend (peek-byte address) data))))
		 *game-state-addresses*)
	     (base64encode data)))

(defun test-poke-fn (address byte)
  (format t "Setting ~a to ~a~%" address byte))

(defun restore-state-base64 (state &optional (packed-bits t) (poke-fn #'test-poke-fn))
  (let ((data (base64decode state))
	(i 0))
    (maphash #'(lambda (name range)
		 (declare (ignorable name))
		 (if (and packed-bits (third range))
		     (let ((byte 0)
			   (bit 8))
		       ;;unpack the bits and poke them in reverse order
		       (loop for address from (second range) downto (first range) do
			    (when (= 8 bit)
			      (setf byte (elt data i)))
			    (funcall poke-fn address (if (logand 1 byte) #x80 #x00))
			    (incf bit)
			    (setf byte (ash byte -1))))
		     (loop for address from (first range) to (second range) do
			  (funcall poke-fn address address (elt data i))
			  (incf i))))
	     *game-state-addresses*)))

;; TODO Here we will generate some javascript to apply the save state
;;      from the uuencoded game state

