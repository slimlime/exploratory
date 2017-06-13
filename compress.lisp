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

(defparameter *max-pattern-length* 19) ;Fits in a nybble, minimum length 4
(defparameter *max-offset* 4095) ;Fits in 12 bits

;;todo modulo offset
;;todo don't span line

(defparameter *width-freq* nil)
(defparameter *offset-freq* nil)

(defun compress (buf)
  (setf *width-freq* (make-array *max-pattern-length* :initial-element 0))
  (setf *offset-freq* (make-array *max-offset* :initial-element 0))
  (let ((eob (1- (length buf)))
	(lfb (lfb buf))
	(lfb-dummy 0)	     ;meh- could just adjust lfb by one bit...
	(out (make-array 0
			 :adjustable t
			 :fill-pointer 0
			 :element-type '(unsigned-byte 8)))
	(word 0)
	(lookup (make-hash-table)))
    (format t "Compressing. LFB=#x~2,'0X~%" lfb)
    (loop for i from 0 to eob do
					;this is inefficient, should build lookup as we go.. but, yaggers
	 (setf word (logior (ash (logand #xffffff word) 8)
			    (aref buf i)))
	 (when (> i 2)
	   (push (- i 3) (gethash word lookup))))
    (loop for i from 0 to eob do
	 (let ((best-offset 0)
	       (best-len 0))
	   (when (<= i (- eob 3))
	     (setf word (logior (ash (aref buf i) 24)
				(ash (aref buf (+ 1 i)) 16)
				(ash (aref buf (+ 2 i)) 8)
				(aref buf (+ 3 i)))) 
	     (dolist (p (gethash word lookup))
	       (let ((offset (- i p 1))
		     (len 0)
		     (q i))
		 (when (and (>= offset 0)
			    (<= offset *max-offset*))
		   (loop while (and (<= q eob)
				    (= (aref buf p)
				       (aref buf q))
				    (< len *max-pattern-length*)) do
			(incf p)
			(incf q)
			(incf len))
		   (when (> len best-len)
		     ;;todo break out early if best-len=19
		     (setf best-len len)
		     (setf best-offset offset))))))
	   (if (> best-len 3)
	       (progn
		 (incf (aref *width-freq* best-len))
		 (incf (aref *offset-freq* best-offset))
		 (vector-push-extend lfb out)
		 (vector-push-extend (logior (ash (- best-len 4) 4)
					     (ash best-offset -8)) out)
		 (vector-push-extend (logand #xff best-offset) out)
		 (incf i (1- best-len)))
	       (vector-push-extend (if (= lfb (aref buf i))
				       lfb-dummy
				       (aref buf i)) out))))
    (format t "~a->~a~%" (length buf) (length out))
    out))

(defun test (arr exp)
  (assert (equalp (compress arr) exp)))

;; no matches
(test #(1) #(1))
(test #(1 2) #(1 2))
(test #(1 2 3) #(1 2 3))
(test #(1 2 3 4) #(1 2 3 4))
;; simple matches
(test #(1 2 3 4 1 2 3) #(1 2 3 4 1 2 3))
(test #(1 2 3 4 1 2 3 4) #(1 2 3 4 0 #x00 3))
(test #(1 2 3 4 1 2 3 4 1) #(1 2 3 4 0 #x10 3))
;; run-on
(test #(1 1 1 1 1) #(1 0 #x00 0))
(test #(1 1 1 1 1 1) #(1 0 #x10 0))
(test #(1 1 1 1 1 1 1) #(1 0 #x20 0))
;; run-on pattern
(test #(1 2 3 1 2 3 1 2 3 1 2 3)
      #(1 2 3 0 #x50 2))
;; back to back
(test #(1 2 3 4 9 9 9 9 1 2 3 4  1 2 3 4  )
      #(1 2 3 4 9 9 9 9 0 #x00 7 0 #x00 3))
;; back to back
(test #(1 2 3 4 5 9 9 9 9 1 2 3 4 5 1 2 3 4 5 )
      #(1 2 3 4 5 9 9 9 9 0 #x10 8 0 #x10 4))
;; best match
(test #(1 2 3 4 5 9 9 9 1 2 3 4  8 8 8 1 2 3 4 5)
      #(1 2 3 4 5 9 9 9 0 #x00 7 8 8 8 0 #x10 14))
;; best match second position
(test #(1 2 3 4 9 9 9 1 2 3 4  5 8 8 8 1 2 3 4 5)
      #(1 2 3 4 9 9 9 0 #x00 6 5 8 8 8 0 #x10 7))
;; match in compressed section
(test #(1 2 3 4 5 6 1 2 3 4 5 6 7 8 5 6 7 8)
      #(1 2 3 4 5 6 0 #x20 5    7 8 0 #x00 3))
;; match 19 ok
(test #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
	1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
      #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
	0 #xF0 18))
;; match 20 ok
(test #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
	1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
      #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
	0 #xF0 19                                       20))
;; simple matches with extra
(test #(1 2 3 4 1 2 3 9) #(1 2 3 4 1 2 3 9))
(test #(1 2 3 4 1 2 3 4 9) #(1 2 3 4 0 #x00 3 9))
(test #(1 2 3 4 1 2 3 4 1 9) #(1 2 3 4 0 #x10 3 9))
;; run-on with extra
(test #(1 1 1 1 1 9) #(1 0 #x00 0 9))
(test #(1 1 1 1 1 1 9) #(1 0 #x10 0 9))
(test #(1 1 1 1 1 1 1 9) #(1 0 #x20 0 9))
;; run-on pattern with extra
(test #(1 2 3 1 2 3 1 2 3 1 2 3 9)
      #(1 2 3 0 #x50 2 9))
;; back to back with extra
(test #(1 2 3 4 9 9 9 9 1 2 3 4  1 2 3 4 11)
      #(1 2 3 4 9 9 9 9 0 #x00 7 0 #x00 3 11))
;; best match with extra
(test #(1 2 3 4 5 9 9 9 1 2 3 4  8 8 8 1 2 3 4 5 11)
      #(1 2 3 4 5 9 9 9 0 #x00 7 8 8 8 0 #x10 14 11))
;; best match second position with extra
(test #(1 2 3 4 9 9 9 1 2 3 4  5 8 8 8 1 2 3 4 5 11)
      #(1 2 3 4 9 9 9 0 #x00 6 5 8 8 8 0 #x10 7 11))
;; match in compressed section with extra
(test #(1 2 3 4 5 6 1 2 3 4 5 6 7 8 5 6 7 8 11)
      #(1 2 3 4 5 6 0 #x20 5    7 8 0 #x00 3 11))
;; match 19 ok with extra
(test #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
	1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
      #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
	0 #xF0 18 20))


