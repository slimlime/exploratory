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

;; where q>p, look for a match at p
(defun match (buf width eob p q)
  (let ((len 0))
    (loop while (and (<= q (min eob (1- (* width (ceiling q width)))))
		     (< len *max-length*)
		     (=
		      (aref buf p)
		      (aref buf q)))
       do
 	 (incf len)
	 (incf p)
	 (incf q))
    len))

;todo, don't allow match width to wrap at end of line
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
			    (aref buf 0))
			out)
    (loop for i from 1 to eob do
	 (let ((best-len 0)
	       (best-offset 0))
	   ;; look for a match on the row above
	   (when (>= i width)
	     (setf best-len (match buf width eob (- i width) i)))
	   ;; look for matches from the beginning of the row
	   ;; or from the maximum offset back on the row
	   (loop for p from (max
			     (- i *max-offset*)
			     (* width (floor i width)))
	      to (1- i) do
		(let ((len (match buf width eob p i)))
		  (when (> len best-len)
		    (setf best-len len)
		    (setf best-offset (- i p)))))
	   (if (> best-len 2)
	       (progn
		 (incf i (1- best-len))
		 (vector-push-extend lfb out)
		 (vector-push-extend (logior (ash (- best-len 3) 4)
					     best-offset)
				     out))
	       (vector-push-extend (if (= (aref buf i) lfb)
				       lfb-dummy
				       (aref buf i))
				   out))))
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
		   (when (= 0 off)
		     (setf off width))
		   (setf off (- q off))
		   (loop for p from off to (+ len-1 off) do
			(vector-push-extend (aref out p) out)
			(incf q))))
	       (progn
		 (vector-push-extend byte out)
		 (incf q)))))
    out))

(defun lentest (f)
  (let ((img (posterize-image 104 104 (load-image f 104 104) :reduce-popcount t)))
    (format t "file ~a ~a ~a~%"
	    f
	    (length (compress (first img) (/ 104 8)))
	    (length (compress (coerce (second img) 'vector) 13))
	    )))

(defun test-images ()
  (lentest "/home/dan/Downloads/cellardoor.bmp")
  (lentest "/home/dan/Downloads/porsche.bmp")
  (lentest "/home/dan/Downloads/face.bmp"))

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


(defun test (arr exp &key (width 100))
  (let ((c (compress arr width)))
    (assert (equalp (subseq (compress arr width) 1) exp))
    (assert (equalp arr (decompress c width)))))

;; no matches
(test #(1) #(1))
(test #(1 2) #(1 2))
(test #(1 2 3) #(1 2 3))
(test #(1 2 3 4) #(1 2 3 4))
;; simple matches
(test #(1 2 3 4 1 2 3) #(1 2 3 4 0 #x04))
(test #(1 2 3 4 1 2 3 4) #(1 2 3 4 0 #x14))
(test #(1 2 3 4 1 2 3 4 1) #(1 2 3 4 0 #x24))
;; run-on
(test #(1 1 1 1 1) #(1 0 #x11))
(test #(1 1 1 1 1 1) #(1 0 #x21))
(test #(1 1 1 1 1 1 1) #(1 0 #x31))
;; run-on pattern
(test #(1 2 3 1 2 3 1 2 3 1 2 3)
      #(1 2 3 0 #x63))
;; back to back
(test #(1 2 3 4 9 9 9 1 2 3 4  1 2 3 4  )
      #(1 2 3 4 9 9 9 0 #x17 0 #x1B))
;; best match
(test #(1 2 3 4 5 9 9 9 1 2 3 4  8 8 8 1 2 3 4 5)
      #(1 2 3 4 5 9 9 9 0 #x18   8 8 8 0 #x2F))
;; best match second position
(test #(1 2 3 4 9 9 9 1 2 3 4  5 8 8 8 1 2 3 4 5)
      #(1 2 3 4 9 9 9 0 #x17   5 8 8 8 0 #x28))
;; match in compressed section
(test #(1 2 3 4 5 6 1 2 3 4 5 6 7 8 5 6 7 8)
      #(1 2 3 4 5 6 0 #x36      7 8 0 #x14))
;; match row above

(test #(1 2 3 4 5 6 7 8 9 10
        2 3 4 4 5 6 7 9 9 10)
      #(1 2 3 4 5 6 7 8 9 10
	2 3 4 0 #x10 9 9 10) :width 10)

(test #(1 2 3 4 5 6 7 8 9 9 10
        2 3 4 4 5 6 7 1 9 9 10)
      #(1 2 3 4 5 6 7 8 9 9 10
	2 3 4 0 #x10  1 0 #x00) :width 11)

;; match row above doesn't run on

(test #(1 2 3 4 5 6 7 8 9 10
        2 3 4 5 6 7 7 8 9 10
        2 3 4)
      #(1 2 3 4 5 6 7 8 9 10
	2 3 4 5 6 7 0 #x10
	2 3 4) :width 10)
