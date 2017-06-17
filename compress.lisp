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
    (copy2screen img 104)
    ;;(vicky)   
    (format t "file ~a ~a ~a~%"
	    f
	    (length (compress (first img) (/ 104 8)))
	    (length (compress (coerce (second img) 'vector) 13)))
    (copy2screen (list (decompress (compress (first img) 13) 13)
		       (decompress (compress (second img) 13) 13))
		 104)))
    ;;(vicky)))

(defun test-images ()
  (lentest "/home/dan/Downloads/garage2.bmp")
  (lentest "/home/dan/Downloads/cellardoor.bmp")
  (lentest "/home/dan/Downloads/porsche.bmp")
  (lentest "/home/dan/Downloads/face.bmp"))

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

;; A cache of converted images, since it takes so long to posterize them
(defparameter *image-cache* nil)

(defun reset-image-cache ()
  (setf *image-cache* (make-hash-table :test 'equal)))

(reset-image-cache)

;; Define an image
(defun dimg (label file sx sy)
  (assert (= 0 (mod sx 8)))
  (assert (= 0 (mod sy 8)))
  (let ((img (gethash file *image-cache*)))
    (when (null img)
      (setf img (posterize-image sx sy (load-image file sx sy)))
      (setf (gethash file *image-cache*) img))
    (let ((data (compress (first img) (/ sx 8)))
	  (att  (compress (second img) (/ sx 8))))
      (label label :image-pixels)
      (add-hint (length data) (format nil "~a pixels (~a)" file (length data)))
      (loop for c across data do
	   (push-byte c))
      (label label :image-colours)
      (add-hint (length att) (format nil "~a colours (~a)" file (length att)))
      (loop for c across att do
	   (push-byte c)))))

(defun draw-image (image w h)
  (assert (= 0 (mod w 8)))
  (assert (= 0 (mod h 8)))
  (with-namespace :decompress
  ;;we don't really need to emit this much assembly for the call
    (LDA (/ w 8))
    (STA.ZP :imgw)
    (sta16.zp (cons :image-pixels image) :src)
    (sta16.zp (+ (/ +screen-width+ 8)
		 (- (/ w 8))
		 *screen-memory-address*)
	      :dest)
    (LDA h)
    (JSR :decompress)
    (sta16.zp (cons :image-colours image) :src)
    (sta16.zp (+ (/ +screen-width+ 8)
		 (- (/ w 8))
		 *char-memory-address*)
	      :dest)
    (LDA (/ h 8))
    (JSR :decompress)))

(defun cls ()
  "Function to clear the char memory with A"
  (label :cls)
  (with-namespace :cls
    (sta16.zp *char-memory-address* :A0)
    (LDY 0)
    (label :page1)
    (STA.IZY :A0)
    (INY)
    (BNE :page1)
    (INC.ZP (hi-add :A0))
    (label :page2)
    (STA.IZY :A0)
    (INY)
    (BNE :page2)
    (INC.ZP (hi-add :A0))
    (label :page3)
    (STA.IZY :A0)
    (INY)
    (BNE :page3)
    (INC.ZP (hi-add :A0))
    (LDY (- 1000 1 (* 3 256)))
    (label :page4)
    (STA.IZY :A0)
    (DEY)
    (BNE :page4)
    (STA.IZY :A0)))
    
;; Render an image to the screen
(defun image-decompressor ()
  (label :decompress)
  (with-namespace :decompress
    (alias :src :A0)
    (alias :dest :A1)
    (alias :prev :A2)
    (alias :lfb :D0)
    (alias :w :D1)
    (alias :h :D2)
    (alias :tmp :D3)
    (alias :imgw :D4)
    (dc "Expect the height in A")
    (STA.ZP :h)
    (LDY 0)
    (LDA.IZY :src)
    (STA.ZP :lfb)
    (inc16.zp :src)
    (label :copy-row)
    (LDX 0)
    (LDY 0)
    (LDA.ZP :imgw)
    (STA.ZP :w)
    (label :copy-byte)
    (LDA.IZX :src "X should be zero")
    (inc16.zp :src)
    (CMP.ZP :lfb)
    (BEQ :pattern)
    (STA.IZY :dest)
    (INY)
    (DEC.ZP :w)
    (BNE :copy-byte)
    (label :row-end)
    (add16.zp (/ +screen-width+ 8) :dest)
    (DEC.ZP :h)
    (BNE :copy-row)
    (RTS)
    (label :pattern)
    (LDA.IZX :src "X should be zero")
    (dc "Get the pattern offset from the lo-nybble")
    (AND.IMM #x0F)
    (BNE :same-row)
    (dc "Pattern is on the row above")
    (CLC)
    (ADC (/ +screen-width+ 8))
    (label :same-row)
    (STA.ZP :tmp)  ;could we negate and add? rather than tmp
    (LDA.ZP (lo-add :dest))
    (SEC)
    (SBC.ZP :tmp)
    (STA.ZP (lo-add :prev))
    (LDA.ZP (hi-add :dest))
    (SBC 0)
    (STA.ZP (hi-add :prev))
    (dc "Get the length from high nybble")
    (LDA.IZX :src)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (CLC)
    (ADC 3)
    (TAX)
    (dc "Advance past the match byte")
    (inc16.zp :src)
    (dc "Now we have offset in PREV and length in X")
    (label :pattern-next)
    (LDA.IZY :prev)
    (STA.IZY :dest)
    (INY)
    (DEC.ZP :w)
    (BEQ :row-end "End of pattern and row")
    (DEX)
    (BEQ :copy-byte "End of pattern")
    (BNE :pattern-next)))

(defun draw-test ()
  (reset-compiler)
  (reset-symbol-table)
  (let ((font :past))
    (flet ((pass ()
	     (zeropage)	     
	     (org #x600)
	     (CLD)
	     (label :render-test2)
	     
	     (sta16.zp :str '(:typeset-cs . :str))
	     (sta16.zp (cons :font font) :font)
	     (sta16.zp #x8000 '(:typeset . :raster))

	     (JSR :typeset-cs)

	     (LDA #x79)
	     (JSR :cls)

	     (draw-image :odd 104 104)
	     
	     (BRK)

	     (cls)
	     (typeset)
	     (image-decompressor)

	     (dimg :odd "/home/dan/Downloads/odd.bmp" 104 104)
	     
	     (dcs :str (justify-with-image *odyssey*
					   104 104 font))

	     (font-data)
	     (label :end)


	     
	     ))
      
      
      (build #'pass))) 

  (monitor-reset #x600)
  (monitor-run)

  (let ((buf (monitor-buffer))) ;need to abstract out the memory, ditch cl-6502
    (setmem-copy buf)))   

  


      
    
      
  
	     
