(defun load-image (img sx sy)
  (sdl:with-init (sdl:sdl-init-video)
    (let* ((img (sdl:load-image img))
	   (sfp (sdl:fp img)))
      (lispbuilder-sdl-base::with-locked-surface (p sfp)
	(let ((out (make-array (* sx sy))))
	  (loop for x from 0 to (1- sx) do
	       (loop for y from 0 to (1- sy) do
		    (lispbuilder-sdl:with-color
			(c (lispbuilder-sdl-base::read-pixel
			    (sdl:point :x x :y y) :surface img)
			   (setf (aref out (+ x (* sx y)))
				 (logior (ash c.r 16)
					 (ash c.g 8)
					 c.b))))))
	  out)))))

;; load the image. There are 256 possible combinations of fg and bg (well not really..)
;; for each 8x8 block. For each block, for each color combination, calculate the 'error'
;; and pick the one which minimises it- each pixel having been assigned either fg or bg

(defun square (x)
  (* x x))

(defun colour-diff (c1 c2)
  (let ((r1 (ash c1 -16))
	(g1 (logand #xff (ash c1 -8)))
	(b1 (logand #xff c1))
	(r2 (ash c2 -16))
	(g2 (logand #xff (ash c2 -8)))
	(b2 (logand #xff c2)))
    ;; Now we could convert to YUV or CIELAB. I prefer the little known
    ;; YAGNILABs colour space:
    ;; CLASSIFIED
    ;; In 1976, YAGNILABs was set up to investigate quantum bogo dynamics. Its
    ;; findings were confiscated by the military a year later and have never been
    ;; released to the public, only leaking onto conspiracy and occult websites.
    ;; Initial documents seemed to indicate that YAGNILAB was working on a
    ;; Ytrrium-Arsenide-Gallium space laser, a finding that was debunked in a
    ;; senatorial hearing when the scientists couldn't answer any questions about
    ;; the laser as they kept saying the word 'Arsenide' repeatedly and giggling.
    ;; It is now widely believed that YAGNILABs spent the entire defence budget on
    ;; sumptuous lunches and trips to various secretive MK-ULTRA linked strip clubs
    ;; located under the Antarctic permafrost.
    (+ (abs (- r1 r2))
       (abs (- g1 g2))
       (abs (- b1 b2)))))

;; YAGNILABs may have to be reactivated...

;; TODO if fg==bg then byte =0, this will aid the compression




(defun posterize-block (x y sx img out)
  (let ((best-diff 13000000)
	(best-c 0))
    (loop for c from 0 to 255 do
	 (let ((diff 0)
	       (fg (aref *c64-colours* (ash c -4)))
	       (bg (aref *c64-colours* (logand c #xf))))
	   (loop for i from x to (+ 7 x) do
		(loop for j from y to (+ 7 y) do
		     (let ((img-col (aref img (+ i (* sx j)))))
		       (incf diff (min (colour-diff fg img-col)
				       (colour-diff bg img-col))))))
	   (when (< diff best-diff)
	     (setf best-diff diff)
	     (setf best-c c))))
    (loop for j from y to (+ 7 y) do
	 (let ((bit #x80)
	       (byte 0))
	   (loop for i from x to (+ 7 x) do
		(let ((img-col (aref img (+ i (* sx j))))
		      (fg (aref *c64-colours* (ash best-c -4)))
		      (bg (aref *c64-colours* (logand best-c #xf))))
		  (when (> (colour-diff fg img-col)
			   (colour-diff bg img-col))
		    (incf byte bit))
		  (setf bit (ash bit -1))))
	   (setf (aref out (+ (* j (floor sx 8)) (floor x 8))) byte)))
    best-c))

(defun posterize-image (sx sy img)
  (let ((attributes nil)
	(bitmap (make-array (* (floor sx 8) sy))))
    (loop for j from 0 to (1- sy) by 8 do
	 (loop for i from 0 to (1- sx) by 8 do
	      (push (posterize-block i j sx img bitmap) attributes)))
    (list bitmap (nreverse attributes))))
	    
(defun copy2screen (result sx)
  (loop for i from 0 to 7999 do
       (setmem (+ i #x8000) 0))
  (loop for i from 0 to 1000 do
       (setmem (+ i #x7000) 0))

  (let ((ptr #x7000)
	(x 0))
    (loop for att in (second result) do
	 (setmem ptr att)
	 (incf ptr)
	 (incf x)
	 (when (= x (floor sx 8))
	   (incf ptr (- 40 x))
	   (setf x 0))))

  
  (let ((ptr #x8000)
	(x 0))
    (loop for b across (first result) do
	 
	 (setmem ptr b)
	 (incf ptr)
	 (incf x)
	 (when (= x (floor sx 8))
	   
	   (incf ptr (- 40 x))
	   (setf x 0))

	   )))



