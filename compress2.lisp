(defun lfb (buf)
  (let ((arr (make-array 256 :element-type 'integer :initial-element 0)))
    (loop for b across buf do
	 (incf (aref arr b)))
    (let ((lf most-positive-fixnum)
	  (lfb 0))
      (loop for i from 0 to 255 do
	   (when (< (aref arr i) lf)
	     (setf lf (aref arr i))
	     (setf lfb i)))
      lfb)))

(defparameter *max-length* 18) ; 0-15 -> 3-18
(defparameter *max-offset* 15) ; 1-15 -> 1-15 (0 encodes the row above)

(defun is-match (x y)
  ;(and (= y (logior x y))
  ;     (< (logcount (logxor x y)) 3)))

  (= x y))

;; where q>p, look for a match at p
(defun match (buf eob p q)
  (let ((len 0))
    (loop while (and (<= q eob)
		     (< len *max-length*)
		     (is-match (aref buf p)
			       (aref buf q))) do
 	 (incf len)
	 (incf p)
	 (incf q))
    len))

(defun compress (buf width)
  (let ((eob (1- (length buf)))
	(lfb (lfb buf))
	(lfb-dummy 0) ;meh- could just adjust lfb by one bit...
	(out (make-array 0
			 :adjustable t
			 :fill-pointer 0
			 :element-type '(unsigned-byte 8))))
    (vector-push-extend lfb out)
    (vector-push-extend (if (= (aref buf 0) lfb)
			    lfb-dummy
			    (aref buf 0)) out)
    (loop for i from 1 to eob do
	 (let ((best-len 0)
	       (best-offset 0))
	   ;; look for a match on the row above
	   (when (>= i width)
	     (setf best-len (match buf eob (- i width) i)))
	   ;; look for matches from the beginning of the row
	   ;; or from the maximum offset back on the row
	   (loop for p from (max
			     (- i *max-offset*)
			     (* width (floor i width))) to (1- i) do
		(let ((len (match buf eob p i)))
		  (when (> len best-len)
		    (setf best-len len)
		    (setf best-offset (- i p)))))
	   (if (> best-len 2)
	       (progn
		 (format t "Found ~a @ ~a ~%" best-len best-offset)
		 (incf i (1- best-len))
		 (vector-push-extend lfb out)
		 (vector-push-extend (logior (ash (- best-len 3) 4)
					     best-offset) out))
	       (vector-push-extend (if (= (aref buf i) lfb)
				       lfb-dummy
				       (aref buf i)) out))))
    out))

(defun decompress (buf width)
  (let ((lfb (aref buf 0))
	(out (make-array 0
			 :adjustable t
			 :fill-pointer 0
			 :element-type '(unsigned-byte 8)))
	(q 0))
    (loop for i from 1 to (1- (length buf)) do
	 (let ((byte (aref buf i)))
	   (if (= byte lfb)
	       (progn
		 (setf byte (aref buf (incf i)))
		 (let ((len-1 (+ 2 (ash byte -4)))
		       (off (logand #xf byte)))
		   (format t "Unpacking ~a @ ~a ~%" (1+ len-1) off)
		   (when (= 0 off)
		     (setf off width))
		   (setf off (- q off))
		   (format t "Going from ~a to ~a ~%" off (+ len-1 off))
		   (loop for p from off to (+ len-1 off) do
			(vector-push-extend (aref out p) out)
			(incf q))))
	       (progn
		 (vector-push-extend byte out)
		 (incf q)))))
    out))

(defun blit (buf width)
  ; for testing purposes. This is a blit rubbish.
  (loop for i from 0 to 7999 do
       (setmem (+ i #x8000) 0))
  ;(loop for i from 0 to 1000 do
  ;     (setmem (+ i #x7000) 0))

  ;(let ((ptr #x7000)
;	(x 0))
;    (loop for att in (second result) do
;	 (setmem ptr att)
;	 (incf ptr)
;	 (incf x)
;	 (when (= x (floor sx 8))
;	   (incf ptr (- 40 x))
;	   (setf x 0))))

  
  (let ((ptr #x8000)
	(x 0))
    (loop for b across buf do
	 
	 (setmem ptr b)
	 (incf ptr)
	 (incf x)
	 (when (= x width)
	   
	   (incf ptr (- 40 x))
	   (setf x 0)))))

