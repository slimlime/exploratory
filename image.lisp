;; TODO popcount reduction is broken as it looks rubbish if we
;; just swap the bytes round. Break the posterization into 2- if we want popcount
;; reduction we have to go round choosing the bytes again, but with a fixed colour
;; Secondly, we need to measure how badly the images are affected by sharing the
;; huffman table
;; Third, how many empty squares are there? Do they have FG=BG in addition to popcount 0
;; might improve the statistics
;; Fourth, If we share one Huffman table over the whole attributes, does it improve at all?

;; Note popcount reversal saved about 200 bytes over three images
;; 590 542 645

(defparameter *images* nil)
(defparameter *image-huffman-lookup* nil)
(defparameter *colour-huffman-lookup* nil)

;; A cache of converted images, since it takes so long to posterize them
;; Unless the posterization changes, we can save this between builds

(defparameter *posterized-image-cache* (make-hash-table :test 'equal))

;; This must be called before each build

(defun reset-images ()
  (setf *images* (make-hash-table :test 'equal))
  (setf *image-huffman-lookup* nil))

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

;; Experimental results RGB - Sum of squares. Rubbish
;;                      RGB - Sum of abs      OK. No green.
;;                      YUV - Sum of abs      Rubbish
;;                      YUV - Squares         Better, still no green in the Porsche
;;                                            but Lena has lost her ghastly pallour
;; face looks good with abs-diff
;; For the porsche, dithered + adjusted yuv is best
;; For Lena, dthering + RGB abs. So it's just preference
;; We must choose and move on

;; Outline for a better method?

;; Pick every other pixel, alternating between lines, picking bg and fg to
;; minimise the error of the pixel
;; Go back and pick the odd pixels picking bg and fg such that the sum of the
;; errors of the pixel and its neighbours is minimised


(defun abs-diff (x1 y1 z1 x2 y2 z2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))
     (abs (- z1 z2))))

(defun square (x)
  (* x x))

(defun square-diff (x1 y1 z1 x2 y2 z2)
  (sqrt (+ (square (- x1 x2))
	   (square (- y1 y2))
	   (square (- z1 z2)))))

(defun rgb2yuv (r g b)
  (let ((y (+ (*  0.257 r) (*  0.504 g) (*  0.098 b)))
	(u (+ (* -0.148 r) (* -0.291 g) (*  0.439 b)))
	(v (+ (*  0.439 r) (* -0.368 g) (* -0.071 b))))
;    (values y u v)))
;    (values (* 2.5 y) (* 1.75 u) (* 1.25 v))))
    (values y (* 1.75 u) (* 1.25 v))))
    
;; Good values for lena 2.5 1.75 1.25 - this gives nice value for porsche too
;; cellardoor 2.5 1.75 1.25
;; porsche 1 1.75 1.25
;; maxine 1 1 1 / 1 1.75 1.25

(defun yuv (r1 g1 b1 r2 g2 b2)
  (multiple-value-bind (y1 u1 v1)
      (rgb2yuv r1 g1 b1)
    (multiple-value-bind (y2 u2 v2)
	(rgb2yuv r2 g2 b2)
      (square-diff y1 u1 v1 y2 u2 v2))))

(defun colour-diff (c1 c2 &optional (f #'yuv))
  (let ((r1 (ash c1 -16))
	(g1 (logand #xff (ash c1 -8)))
	(b1 (logand #xff c1))
	(r2 (ash c2 -16))
	(g2 (logand #xff (ash c2 -8)))
	(b2 (logand #xff c2)))
    (funcall f r1 g1 b1 r2 g2 b2)))

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

(defparameter *even-bytes*
  #(#b00000000 #b00000011 #b00000001 #b00001100 #b00001111 #b00001101 #b00000100 #b00000111 
    #b00000101 #b00110000 #b00110011 #b00110001 #b00111100 #b00111111 #b00111101 #b00110100 
    #b00110111 #b00110101 #b00010000 #b00010011 #b00010001 #b00011100 #b00011111 #b00011101 
    #b00010100 #b00010111 #b00010101 #b11000000 #b11000011 #b11000001 #b11001100 #b11001111 
    #b11001101 #b11000100 #b11000111 #b11000101 #b11110000 #b11110011 #b11110001 #b11111100 
    #b11111111 #b11111101 #b11110100 #b11110111 #b11110101 #b11010000 #b11010011 #b11010001 
    #b11011100 #b11011111 #b11011101 #b11010100 #b11010111 #b11010101 #b01000000 #b01000011 
    #b01000001 #b01001100 #b01001111 #b01001101 #b01000100 #b01000111 #b01000101 #b01110000 
    #b01110011 #b01110001 #b01111100 #b01111111 #b01111101 #b01110100 #b01110111 #b01110101 
    #b01010000 #b01010011 #b01010001 #b01011100 #b01011111 #b01011101 #b01010100 #b01010111 
    #b01010101))

(defparameter *odd-bytes*
  #(#b00000000 #b00000011 #b00000010 #b00001100 #b00001111 #b00001110 #b00001000 #b00001011 
    #b00001010 #b00110000 #b00110011 #b00110010 #b00111100 #b00111111 #b00111110 #b00111000 
    #b00111011 #b00111010 #b00100000 #b00100011 #b00100010 #b00101100 #b00101111 #b00101110 
    #b00101000 #b00101011 #b00101010 #b11000000 #b11000011 #b11000010 #b11001100 #b11001111 
    #b11001110 #b11001000 #b11001011 #b11001010 #b11110000 #b11110011 #b11110010 #b11111100 
    #b11111111 #b11111110 #b11111000 #b11111011 #b11111010 #b11100000 #b11100011 #b11100010 
    #b11101100 #b11101111 #b11101110 #b11101000 #b11101011 #b11101010 #b10000000 #b10000011 
    #b10000010 #b10001100 #b10001111 #b10001110 #b10001000 #b10001011 #b10001010 #b10110000 
    #b10110011 #b10110010 #b10111100 #b10111111 #b10111110 #b10111000 #b10111011 #b10111010 
    #b10100000 #b10100011 #b10100010 #b10101100 #b10101111 #b10101110 #b10101000 #b10101011 
    #b10101010))

(defun tristatebits (s ht)
  (case s (0 0) (1 3) (2 ht)))

(defun tristatebytes (ht) ;;1 or 2
  (let ((tab 0))
    (loop for a from 0 to 2 do
	 (loop for b from 0 to 2 do
	      (loop for c from 0 to 2 do
		   (loop for d from 0 to 2 do
			  (format t "#b~8,'0b " (logior (ash (tristatebits a ht) 6)
							(ash (tristatebits b ht) 4)
							(ash (tristatebits c ht) 2)
							(tristatebits d ht)))
			  (when (= (incf tab) 8)
			    (terpri)
			    (setf tab 0)))
		      )))))

(defun posterize-block (x y sx img out reduce-popcount)
  (let ((best-diff most-positive-fixnum)
	(best-c 0)
	(popcount 0))
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
    (flet ((apply-best-match (fg bg)
	     (loop for j from y to (+ 7 y) do
		  (let ((best-byte 0))
		    (unless (= fg bg)
		      (let ((best-diff most-positive-fixnum))
			(loop for byte across (if (= (mod j 2) 0)
						  *even-bytes*
						  *odd-bytes*)
			   do (let ((diff 0)
				    (bit #x80))
				(loop for i from x to (+ 7 x) do
				     (let ((img-col (aref img (+ i (* sx j))))
					   (is-bg (= 0 (logand bit byte))))
				       (incf diff (colour-diff (if is-bg bg fg) img-col)))
				     (setf bit (ash bit -1)))
				(when (< diff best-diff)
				  (setf best-diff diff)
				  (setf best-byte byte))))))
		    (setf (aref out (+ (* j (floor sx 8)) (floor x 8))) best-byte)
		    (let ((bit #x80))
		      (loop for i from 0 to 7 do
			   (let ((is-bg (= 0 (logand bit best-byte))))
			     (unless is-bg (incf popcount)))
			   (setf bit (ash bit -1))))))))
      (let ((fg (aref *c64-colours* (ash best-c -4)))
	    (bg (aref *c64-colours* (logand best-c #xf))))
	(apply-best-match fg bg)
	(when (and reduce-popcount
		   (> popcount 32))
	  (setf popcount 0)
	  (apply-best-match bg fg)
	  ;;experiments show doesn't really make much difference
	  ;;if we swap back when popcount > 32 here
	  (setf best-c (logior (ash (logand best-c #xf) 4)
			       (ash best-c -4)))))
      best-c)))

(defun posterize-image (sx sy img &key (reduce-popcount t))
  (let ((attributes nil)
	(bitmap (make-array (* (floor sx 8) sy))))
    (loop for j from 0 to (1- sy) by 8 do
	 (loop for i from 0 to (1- sx) by 8 do
	      (push (posterize-block i j sx img bitmap reduce-popcount)
		    attributes)))
    (list bitmap (coerce (nreverse attributes) 'vector))))
	    
(defun copy2screen (result sx)
  ; for testing purposes. This is a blit rubbish.
  (loop for i from 0 to 7999 do
       (setmem (+ i #x8000) 0))
  (loop for i from 0 to 1000 do
       (setmem (+ i #x7000) 0))
  (let ((ptr #x7000)
	(x 0))
    (loop for att across (second result) do
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
	   (setf x 0)))))

(defun pre-encode-image (img sx sy)
  (let ((out (make-array (length img) :element-type 'integer))
	(k 0))
    (loop for i from 0 to (1- sy) do
	 (loop for j from 0 to (1- (/ sx 8)) do
	      (let ((index (position (aref img k)
				     (if (= (mod i 2) 0)
					 *even-bytes*
					 *odd-bytes*))))
		(assert index nil "Byte ~a was not in the image byte list" (aref img k))
		(setf (aref out k) index))
	      (incf k)))
    out))

(defun pre-encode-colours (colours)
  (let ((out (make-array (length colours) :element-type 'integer))
	(p 0))
	(loop for i from 0 to (1- (length colours)) do
	     (setf (aref out i) (logxor (aref colours i) p))
	     (setf p (aref colours i)))
	out))

(defun huffman-encode-vector (vec lookup)
  (let ((out (make-array (length vec)
			     :fill-pointer 0
			     :adjustable t
			     :element-type '(unsigned-byte 8))))
	(let ((word 0) ;;24 bits
	      (bits 0))
	  (flet ((emit ()
		   (vector-push-extend (ash word -16) out)
		   (setf word (logand #xffff00 (ash word 8)))
		   (decf bits 8)))
	    (loop for b across vec
	       for j from 0 do
		 (let ((e (gethash b lookup)))
		   (assert e nil "byte ~a not in lookup" b)
		   ;;shift the bit pattern to the right so it would be at the
		   ;;24 bit position if the word buffer were empty
		   (setf word (logior word (ash (third e) (- 8 bits))))
		   ;;now add on the number of bits
		   (incf bits (second e))
		   (do ()
		       ((< bits 8))
		     (emit))))
	    (when (> bits 0)
	      (emit))
	    (when (> bits 0)
	      (emit)))
	  out)))

(defun get-posterized-image (file sx sy)
  (aif (gethash file *posterized-image-cache*)
       it
       (setf (gethash file *posterized-image-cache*)
	     (posterize-image sx sy (load-image file sx sy) :reduce-popcount t))))

(defun dimg (name file sx sy)
  (assert (= 0 (mod sx 8)))
  (assert (= 0 (mod sy 8)))
  (if *image-huffman-lookup*
      ;;stick the image data here
      (destructuring-bind (pixels colours) (get-posterized-image file sx sy)
	(dc (format nil "Image ~a ~ax~a (~a)" name sx sy file))
	(let ((data (huffman-encode-vector (pre-encode-image pixels sx sy)
					   *image-huffman-lookup*)))
	  (label :pixels name)
	  (add-hint (length data) (format nil "~a pixel data ~a bytes" name (length data)))
	  (loop for c across data do (push-byte c)))
	(let ((data (huffman-encode-vector (pre-encode-colours colours)
					   *colour-huffman-lookup*)))
	  (label :colours name)
	  (add-hint (length data) (format nil "~a colour data ~a bytes" name (length data)))
	  (loop for c across data do (push-byte c))))
      ;;otherwise just add it to the list to be processed
      (setf (gethash name *images*) (list file sx sy))))

(defun to-freq-table (freq)
  (let ((tbl nil))
    (loop for f across freq
       for i from 0 do
	 (when (> f 0)
	   (push (list i f) tbl)))
    (huffman (sort tbl #'> :key #'second))))

(defun image-table ()
  (let ((pixel-freq (make-array 256 :element-type 'integer :initial-element 0))
	(colour-freq (make-array 256 :element-type 'integer :initial-element 0)))
    (do-hashtable (name image *images*)
      (declare (ignore name))
      (destructuring-bind (file sx sy) image
	(destructuring-bind (pixels colours) (get-posterized-image file sx sy)
	  (loop for byte across (pre-encode-colours colours) do
	       (incf (aref colour-freq byte)))
	  ;;get byte frequencies across all images
	  (loop for byte across (pre-encode-image pixels sx sy) do
	       (incf (aref pixel-freq byte))))))
    (let ((tbl (to-freq-table colour-freq)))
      (setf *colour-huffman-lookup*
	    (build-huffman-bit-pattern-lookup tbl))
      (huffman-pop-table :colour-population tbl "Colour Huffman Table")
      (apply #'db :colours (mapcar #'first tbl)))
    ;;filter out the 0 occurances and sort
    (let ((tbl (to-freq-table pixel-freq)))
      ;;(when *compiler-final-pass* (print tbl))
      (setf *image-huffman-lookup*
	    (build-huffman-bit-pattern-lookup tbl))
      (huffman-pop-table :pixel-population tbl "Pixel Huffman Table")
      ;;now we have to be able to translate from indices to bytes
      ;;using the amazing tables
      (apply #'db :even-bytes (mapcar #'(lambda (e) (aref *even-bytes* (first e))) tbl))
      (apply #'db :odd-bytes (mapcar #'(lambda (e) (aref *odd-bytes* (first e))) tbl)))))

(defun set-colours (colour)
  (call-memset colour *char-memory-address* +char-memory-length+))

;; Render an image to the screen
(defun image-decompressor ()
  (label :decompress-image)
  (with-namespace :decompress-image
    ;;these parameters must be set
    (alias :src :huffman-ptr)
    (alias :dst :A1)
    (alias :width/8 :D1)
    (alias :height :D2) ;;pixels
    (alias :prev-colour :D4)
    ;;temps
    (alias :column :D3)
    (dc "Initialize huffman decoder")
    (LDA 1)
    (STA.ZP :huffman-bits)
    (sta16.zp :pixel-population :huffman-pop-table)
    (dc "Start with the even lookup table")
    (sta16.ab :even-bytes :bytes)
    (LDY 0)
    (label :next)
    (STY.ZP :column)
    (JSR :huffman-next)
    (label+1 :bytes)
    (LDA.ABX 0)
    (LDY.ZP :column)
    (STA.IZY :dst)
    (INY)
    (CPY.ZP :width/8)
    (BNE :next)
    (DEC.ZP :height)
    (BEQ :done)
    (dc "Swap the lookup table from even to odd")
    (dc "and vice-versa, using the XOR trick.")
    (LDA.AB  (hi-add :bytes))
    (EOR (logxor (hi :even-bytes) (hi :odd-bytes)))
    (STA.AB (hi-add :bytes))
    (LDA.AB (lo-add :bytes))
    (EOR (logxor (lo :even-bytes) (lo :odd-bytes)))
    (STA.AB (lo-add :bytes))
    (add16.zp (/ +screen-width+ 8) :dst)
    (LDY 0)
    (BEQ :next)
    (label :done)
    (RTS)
    (label :decompress-colours nil)
    (LDA 0)
    (STA.ZP :prev-colour)
    (dc "Initialize huffman decoder")
    (LDA 1)
    (STA.ZP :huffman-bits)
    (sta16.zp :colour-population :huffman-pop-table)
    (LDY 0)
    (label :next)
    (STY.ZP :column)
    (JSR :huffman-next)
    (LDA.ABX :colours)
    (dc "Each byte is XORed with the previous one")
    (EOR.ZP :prev-colour)
    (STA.ZP :prev-colour)
    (LDY.ZP :column)
    (STA.IZY :dst)
    (INY)
    (CPY.ZP :width/8)
    (BNE :next)
    (DEC.ZP :height)
    (BEQ :done)
    (add16.zp (/ +screen-width+ 8) :dst)
    (LDY 0)
    (BEQ :next)
    (label :done)
    (RTS)))

(defun draw-test () 
  (reset-compiler)
  (reset-images)
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (label :start)
	   (CLD)
	   (set-colours #xf0)
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :width/8))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-image . :height))
	   (sta16.zp (cons :img1 :pixels) '(:decompress-image . :src))
	   (sta16.zp *screen-memory-address* '(:decompress-image . :dst))
	   (JSR :decompress-image)
	   (sta16.ab (cons :img1 :colours) '(:decompress-image . :src))
	   (sta16.ab *char-memory-address* '(:decompress-image . :dst))
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :height))
	   (JSR :decompress-colours)
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :width/8))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-image . :height))
	   (sta16.zp (cons :img2 :pixels) '(:decompress-image . :src))
	   (sta16.zp (+ 13 *screen-memory-address*) '(:decompress-image . :dst))
	   (JSR :decompress-image)
	   (sta16.ab (cons :img2 :colours) '(:decompress-image . :src))
	   (sta16.ab (+ 13 *char-memory-address*) '(:decompress-image . :dst))
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :height))
	   (JSR :decompress-colours)
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :width/8))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-image . :height))
	   (sta16.zp (cons :img3 :pixels) '(:decompress-image . :src))
	   (sta16.zp (+ 26 *screen-memory-address*) '(:decompress-image . :dst))
	   (JSR :decompress-image)
	   (sta16.ab (cons :img3 :colours) '(:decompress-image . :src))
	   (sta16.ab (+ 26 *char-memory-address*)'(:decompress-image . :dst))
	   (LDA 13)
	   (STA.ZP '(:decompress-image . :height))
	   (JSR :decompress-colours)
	   (BRK)
	   (memset)
	   (huffman-decoder)
	   (image-decompressor)
	   (dimg :img1 "~/exploratory/images/porsche.bmp" 104 104)
	   (dimg :img2 "~/exploratory/images/cheetos.bmp" 104 104)
	   (dimg :img3 "~/exploratory/images/garage.bmp" 104 104)
	   (image-table)
	   (label :end)))
    (pass)
    (pass)
    (pass)
    (setf *compiler-final-pass* t)
    (pass))
  (monitor-reset #x600)
  (monitor-run)
  (setmem-copy (monitor-buffer)))


