;; a list of named address ranges containing state we want to
;; save in a game

;; NOTE the hashset does not have a defined order. Here is does
;; not matter as the iteration will be the same for the same build
;; but we need to take care that the javascript code makes it come
;; out the same way too- which it will as the javascript will be
;; generated here.

;; TODO Use parenscript to generate the js encoder and decoder-
;; it will need to be present on the target.

;; IDEA If we use difference encoding from the initial state the
;; state string can be shorter if we use another symbol to indicate
;; RLE, e.g. $D for 4 bytes that do not differ from their initial
;; state. However, this does not improve the worst case scenario

;; TODO when we restore the state we need to trigger the game to
;; navigate to the right location. It would be fun to do this with
;; an interrupt, but not essential.

(defparameter *game-state-addresses* nil)

;; TODO monitor poke and peek

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
    (dc (format nil "+ State  '~a'" name))
    (funcall body-fn)
    (dc (format nil "- State '~a'" name))
    (add-to-state-addresses name is-bits zp-start *compiler-zp-free-slot*)
    (add-to-state-addresses name is-bits start *compiler-ptr*)))

(defmacro game-state-bytes (name &body body)
  `(game-state-fn ,name nil #'(lambda () ,@body)))

(defmacro game-state-bits (name &body body)
  `(game-state-fn ,name t #'(lambda () ,@body)))

(defun reset-game-state-ranges ()
  (setf *game-state-addresses* (make-hash-table :test 'equal)))

(reset-game-state-ranges)

(defun game-state-ranges ()
  "Return the state ranges ordered by address"
  (let ((ranges nil))
    (maphash #'(lambda (name range)
		 (declare (ignorable name))
		 (push range ranges))
	     *game-state-addresses*)
    (sort ranges #'< :key #'car)))

(defun dump-state-ranges ()
  (let ((ranges nil))
    (maphash #'(lambda (name range)
		 (push (cons range name) ranges))
	     *game-state-addresses*)
    (dolist (range (sort ranges #'< :key #'caar))
      (format t "$~4,'0X-$~4,'0X ~a ~a~%"
	      (first (car range))
	      (second (car range))
	      (cdr range)
	      (if (third (car range)) "As bits" "As bytes")))
    *game-state-addresses*))

(defun dump-state-base64 (&key (pack-bits t) (peek-fn #'monitor-peek))
  (let ((data (make-array 0
			  :adjustable t
			  :fill-pointer 0
			  :element-type '(unsigned-byte 8))))
    (dolist (range (game-state-ranges))
      (if (and pack-bits (third range))
	  (let ((byte 0)
		(bit 0))
	    ;;pack the bits
	    (loop for address from (first range) to (second range) do
		 (setf byte (logior (ash byte 1)
				    (if (/= 0 (funcall peek-fn address))
					1
					0)))
		 (incf bit)
		 (when (= 8 bit)
		   (vector-push-extend byte data)
		   (setf byte 0)
		   (setf bit 0)))
	    (when (/= 0 bit)
	      (setf byte (ash byte (- 8 bit)))
	      ;;push in the remaining bits
	      (vector-push-extend byte data)))
	  (loop for address from (first range) to (second range) do
	       (vector-push-extend (funcall peek-fn address) data))))
    (base64encode data)))

(defun restore-state-base64 (state &key (packed-bits t) (poke-fn #'monitor-poke))
  (let ((data (base64decode state))
	(src 0))
    (dolist (range (game-state-ranges))
      (if (and packed-bits (third range))
	  (let ((byte 0)
		(bit 8))
	    ;;unpack the bits and poke them in reverse order
	    (loop for address from (first range) to (second range) do
		 (when (= 8 bit)
		   (setf byte (elt data src))
		   (incf src)
		   (setf bit 0))
		 (funcall poke-fn address (if (= 0 (logand 128 byte)) #x00 #x80))
		 (incf bit)
		 (setf byte (ash byte 1))))
	  (loop for address from (first range) to (second range) do
	       (funcall poke-fn address (elt data src))
	       (incf src))))))

;; test the saving and loading code

(reset-game-state-ranges)

(add-to-state-addresses "a" nil 0 3)
(add-to-state-addresses "b" nil 4 6)

(let* ((src #(97 98 99 0 97 98))
       (dst (make-array 6))
       (enc (dump-state-base64 :peek-fn #'(lambda (address)
					    (elt src address)))))
  (assert (string= enc "YWJjYWI="))
  (restore-state-base64 enc :poke-fn #'(lambda (address byte)
					 (setf (elt dst address) byte)))
  (assert (equalp dst src)))

;; couple of bit ranges, note that bit ranges aren't coalesced.

(reset-game-state-ranges)

(add-to-state-addresses "a" t 0 3)
(add-to-state-addresses "b" t 4 6)

(let* ((src #(128 0 128 0 128 128))
       (dst (make-array 6))
       (enc (dump-state-base64 :peek-fn #'(lambda (address)
					    (elt src address)))))
  (assert (string= enc "oMA="))
  (restore-state-base64 enc :poke-fn #'(lambda (address byte)
					 (setf (elt dst address) byte)))
  (assert (equalp dst src)))

;; 8 bits

(reset-game-state-ranges)

(add-to-state-addresses "b" t 0 8)

(let* ((src #(128 128 0 128 0 0 128 128))
       (dst (make-array (length src)))
       (enc (dump-state-base64 :peek-fn #'(lambda (address)
					    (elt src address)))))
  (restore-state-base64 enc :poke-fn #'(lambda (address byte)
					 (setf (elt dst address) byte)))
  (assert (equalp dst src)))

;; more than 8 bits

(reset-game-state-ranges)

(add-to-state-addresses "b" t 0 10)

(let* ((src #(128 128 0 128 0 0 128 128 0 128))
       (dst (make-array (length src)))
       (enc (dump-state-base64 :peek-fn #'(lambda (address)
					    (elt src address)))))
  (restore-state-base64 enc :poke-fn #'(lambda (address byte)
					 (setf (elt dst address) byte)))
  (assert (equalp dst src)))

;; TODO Here we will generate some javascript to apply the save state
;;      from the uuencoded game state

