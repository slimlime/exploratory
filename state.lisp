;; a list of named address ranges containing state we want to
;; save in a game

(defparameter *game-state-addresses* nil)

;; TODO TODO "Bit Table" can be compressed down to 1/8th...
;; TODO Monitor Poke! Which is missing..

(defun add-to-state-addresses (name start end)
  (when (> end start)
    (setf (gethash name *game-state-addresses*)
	  (list start (1- end)))))

;;This macro marks all the bytes emitted inside it as part of the
;;game state.

(defmacro game-state (name &body body)
  (let ((start-sym (gensym))
	(name-sym (gensym))
	(zp-start-sym (gensym)))
    `(let ((,start-sym *compiler-ptr*)
	   (,name-sym ,name)
	   (,zp-start-sym *compiler-zp-free-slot*))
       (dc (format nil "+ ~a" ,name-sym))
       ,@body
       (dc (format nil "- ~a" ,name-sym))
       (add-to-state-addresses ,name-sym ,zp-start-sym *compiler-zp-free-slot*)
       (add-to-state-addresses ,name-sym ,start-sym *compiler-ptr*))))

(defun reset-game-state-ranges ()
  (setf *game-state-addresses* (make-hash-table :test 'equal)))

(defun dump-state-ranges ()
  (maphash #'(lambda (name range)
	       (format t "$~4,'0X-$~4,'0X ~a~%"
		       (first range)
		       (second range)
		       name)) *game-state-addresses*))

(defun dump-state-base64 ()
  (let ((data (make-array 0
			  :adjustable t
			  :fill-pointer 0
			  :element-type '(unsigned-byte 8))))
    (maphash #'(lambda (name range)
		 (declare (ignorable name))
		 (loop for address from (first range) to (second range) do
		      (vector-push-extend (peek-byte address) data)))
	     *game-state-addresses*)
    (base64encode data)))

(defun restore-state-base64 (state)
  (let ((data (base64decode state))
	(i 0))
    (maphash #'(lambda (name range)
		 (declare (ignorable name))
		 (loop for address from (first range) to (second range) do
		      (format t "Setting ~a to ~a~%" address (elt data i))
		      (incf i)))
	     *game-state-addresses*)))
  

;; TODO Here we will generate some javascript to apply the save state
;;      from the uuencoded game state

