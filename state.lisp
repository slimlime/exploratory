;; a list of named address ranges containing state we want to
;; save in a game

(defparameter *game-state-addresses* nil)

(defun add-to-state-addresses (name start end)
  (when *compiler-final-pass*
    (push (list name start (1- end)) *game-state-addresses*)))

;;This macro marks all the bytes emitted inside it as part of the
;;game state.

;;DOESNT WORK FOR ZP RES
  
(defmacro game-state (name &body body)
  (let ((start-sym (gensym))
	(name-sym (gensym)))
    `(let ((,start-sym *compiler-ptr*)
	   (,name-sym ,name))
       (dc (format nil "+ ~a" ,name-sym))
       ,@body
       (dc (format nil "- ~a" ,name-sym))
       (add-to-state-addresses ,name-sym ,start-sym *compiler-ptr*))))

(defun reset-game-state ()
  (setf *game-state-addresses* nil))

(defun dump-state-ranges ()
  (dolist (range *game-state-addresses*)
    (format t "$~4,'0X-$~4,'0X ~a~%"
	    (second range)
	    (third range)
	    (first range))))

(defun dump-state-base64 ()
  (let ((data (make-array 0
			  :adjustable t
			  :fill-pointer 0
			  :element-type '(unsigned-byte 8))))
    (dolist (range *game-state-addresses*)
      (loop for address from (second range) to (third range) do
	   (vector-push-extend (peek-byte address) data)))
    data))

;; TODO Here we will generate some javascript to apply the save state
;;      from the uuencoded game state

