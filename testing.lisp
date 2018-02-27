(defun test-input (&rest commands)
  (mapc #'(lambda (s) (enter-input s :print nil)) commands))

(defun assert-location (location)
   (assert (= (resolve location)
	     (monitor-peek-addr (resolve :current-location)))
	  nil
	  "Was not in location ~a" location))

(defun assert-set (&rest bits)
  (assert-bits bits t))

(defun assert-clr (&rest bits)
  (assert-bits bits nil))

(defun assert-object-in (object location)
  (let ((expected-place (place-id location))
	(actual-place (monitor-peek (object-place-address object))))
    (assert expected-place nil "Expected place ~a was not defined" location)
    (assert (eq expected-place actual-place)
	    nil
	    "Expected the ~a in ~a (~a) was in place ~a"
	    object location expected-place actual-place)))
