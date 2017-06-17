;; Read a string, ignoring newlines and return a new string
;; with newlines inserted so it fits into a line

;; test width function for debugging
(defun test-width (w)
  (* 8 (length w)))

(defun test-sw (y)
  (declare (ignorable y))
  +screen-width+)

(defun font-width (font)
  ;todo handle kerning
  #'(lambda (word)
      (let ((w 0))
	(loop for c across word do
	     (incf w (gethash (cons font c) *font-widths*))
	     (incf w))
	w)))

;;sw function gets the width at the y specified y position,
;;this is so we can draw up to the edge of an image
(defun justify (s &key (dy 11) (sw #'test-sw) (width #'test-width))
  (let ((len (length s))
	(i 0)
	(x 0)
	(y 0))
    (labels ((is-space (c) (or
			    (char= c #\Newline)
			    (char= c #\ )))
	     (next-word ()
	       (let ((w (make-array 16 
				    :element-type 'standard-char 
				    :fill-pointer 0
				    :adjustable t)))
		 (loop while (and (< i len)
				  (not (is-space (aref s i))))
		    do
		      (vector-push-extend (aref s i) w)
		      (incf i))
		 ;gobble up spaces
		 (loop while (and (< i len)
				  (is-space (aref s i)))
		    do
		      (incf i))
		 (if (> (length w) 0) w nil))))
      (let ((spc (funcall width " "))
	    (js (make-array 16 :element-type 'standard-char
			    :fill-pointer 0
			    :adjustable t)))
	(loop while t do
	     (let ((w (next-word)))
	       (unless w
		 (return))
	       (let* ((dx (funcall width w))
		      (fits (<= (+ dx x) (funcall sw y))))
		 (if fits
		     (progn
		       (unless (= x 0)
			 (vector-push-extend #\  js))
		       (incf x dx))
		     (progn
		       (vector-push-extend #\Newline js)
		       (setf x dx)
		       (incf y dy)))
	       (incf x spc)
	       (loop for c across w do
		    (vector-push-extend c js)))))
	js))))

(defun justify-with-image (s imgw imgh font)
  (justify s :sw #'(lambda (y)
		     (- +screen-width+ (if (<= y (1+ imgh)) imgw 0)))
	   :width (font-width font)))



(defparameter *odyssey*
"Tell me, O muse, of that ingenious hero who travelled far and wide after he had sacked
the famous town of Troy. Many cities did he visit, and many were the nations with whose 
manners and customs he was acquainted, moreover he suffered much by sea while trying to
save his own life and bring his men safely home, but do what he might he could not save
his men, for they perished through their own sheer folly in eating the cattle of the
Sun-god Hyperion, so the god prevented them from ever reaching home. Tell me, too,
about all these things, O daughter of Jove, from whatsoever source you may know them.")

