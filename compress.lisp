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
(defun match (buf eob p q)
  (let ((len 0))
    (loop while (and (<= q eob)
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
	     (setf best-len (match buf eob (- i width) i)))
	   ;; look for matches from the beginning of the row
	   ;; or from the maximum offset back on the row
	   (loop for p from (max
			     (- i *max-offset*)
			     (* width (floor i width)))
	      to (1- i) do
		(let ((len (match buf eob p i)))
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
    ;(vicky)   
    (format t "file ~a ~a ~a~%"
	    f
	    (length (compress (first img) (/ 104 8)))
	    (length (compress (coerce (second img) 'vector) 13)))
    (copy2screen (list (decompress (compress (first img) 13) 13)
		       (decompress (compress (second img) 13) 13))
		 104)
    ;(vicky)
    ))

(defun test-images ()
  (lentest "/home/dan/Downloads/odd.bmp")
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

;; match row above does run on

(test #(1 2 3 4 5 6 7 8 9 10
        2 3 4 5 6 7 7 8 9 10
        2 3 4)
      #(1 2 3 4 5 6 7 8 9 10
	2 3 4 5 6 7 0 #x40) :width 10)

;; A cache of converted images, since it takes so long to posterize them
(defparameter *image-cache* nil)

(defun reset-image-cache ()
  (setf *image-cache* (make-hash-table :test 'equal)))

(reset-image-cache)

;; Define an image
(defun dimg (name file sx sy)
  (assert (= 0 (mod sx 8)))
  (assert (= 0 (mod sy 8)))
  (dc (format nil "Image ~a ~ax~a (~a)" name sx sy file))
  (let ((img (gethash file *image-cache*)))
    (when (null img)
      (setf img (posterize-image sx sy (load-image file sx sy)))
      (setf (gethash file *image-cache*) img))
    (let ((data (compress (first img) (/ sx 8)))
	  (att  (compress (second img) (/ sx 8))))
      (label :pixels name)
      (add-hint (length data) (format nil "~a pixels (~a)" file (length data)))
      (loop for c across data do
	   (push-byte c))
      (label :colours name)
      (add-hint (length att) (format nil "~a colours (~a)" file (length att)))
      (loop for c across att do
	   (push-byte c)))))

(defun cls (colour)
  (call-memset colour *char-memory-address* +char-memory-length+))

(defun draw-image (image w h &optional (img-align :right))
  (assert (= 0 (mod w 8)))
  (assert (= 0 (mod h 8)))
  (with-namespace :decompress
    ;;we don't really need to emit this much assembly for the call
    (let ((offset (if (eq img-align :right)
		      (+ (/ +screen-width+ 8) (- (/ w 8)))
		      0)))

      (LDA (/ w 8))
      (STA.ZP :imgw)
      (sta16.zp (cons image :pixels) :data)
      (sta16.zp (+ offset *screen-memory-address*) :dest)
      (LDA h)
      (JSR :decompress)
      (sta16.zp (cons image :colours) :data)
      (sta16.zp (+ offset *char-memory-address*) :dest)
      (LDA (/ h 8))
      (JSR :decompress))))

;; Render an image to the screen
(defun image-decompressor ()
  ;;expecting the image height in A
  (label :decompress)
  (with-namespace :decompress
    (alias :data :A0) ;; source image
    (alias :dest :A1) ;; current screen position
    (alias :src :A2)  ;; pattern address
    (alias :lfb :D0)  ;; special least frequent byte
    (alias :imgw :D1) ;; image width
    (alias :imgh :D2) ;; image height
    (alias :src-y :D3)    ;; source pattern column
    (alias :dest-y :D4)   ;; destination column
    (alias :tmp :D5)  ;; temp for subtraction
    
    (STA.ZP :imgh "Store the height")

    (dc "Get the LFB")
    (LDX 0)
    (STX.ZP :dest-y)
    (LDA.IZX :data)
    (STA.ZP :lfb)

    (dc "Get a byte from the data and either emit")
    (dc "it to the screen or check for the special")
    (dc "LFB which signals a pattern")
    (label :next)
    (inc16.zp :data)
    (LDA.IZX :data)
    (CMP.ZP :lfb)
    (BEQ :pattern)
    (JSR :emit)
    (JMP :next)

    (dc "Found a pattern. Extract the offset and")
    (dc "set up a src pointer and a column to copy from")
    
    (label :pattern)
    (inc16.zp :data)
    (LDA.IZX :data)
    (AND.IMM #xF)
    (BEQ :row-above)
    (dc "Pattern is on the same row, so we use the same")
    (dc "scanline pointer for the source, but offset the")
    (dc "column index by the amount in the lo-nybble")
    (EOR #xFF)
    (SEC)
    (ADC.ZP :dest-y)
    (STA.ZP :src-y)
    (cpy16.zp :dest :src)
    (JMP :copy)
    (label :row-above)
    (LDA.ZP (lo-add :dest))
    (SEC)
    (SBC (/ +screen-width+ 8))
    (STA.ZP (lo-add :src))
    (LDA.ZP (hi-add :dest))
    (SBC 0)
    (STA.ZP (hi-add :src))
    (LDA.ZP :dest-y)
    (STA.ZP :src-y)
    (label :copy)
    (dc "Get the pattern width from the hi-nybble")
    (LDA.IZX :data)
    (LSR)
    (LSR)
    (LSR)
    (LSR)
    (CLC)
    (ADC 3)
    (TAX)
    (label :next-pattern)
    (LDA.ZP :src-y)
    (CMP.ZP :imgw)
    (BNE :not-wrapped)
    (add16.zp (/ +screen-width+ 8) :src)
    (LDA 0)
    (STA.ZP :src-y)
    (label :not-wrapped)
    (TAY)
    (LDA.IZY :src)
    (LDY.ZP :dest-y)
    (STA.IZY :dest)
    (JSR :emit)
    (INC.ZP :src-y)
    (DEX)
    (BNE :next-pattern)
    (BEQ :next)

    ;; This bit emits a byte and takes care that the output
    ;; pointer dest and its column dest-y wrap at the end
    ;; of the scanline.
    
    (label :emit)
    (dc "Emit a byte to the screen")
    (LDY.ZP :dest-y)
    (STA.IZY :dest)
    (INY)
    (TYA)
    (CMP.ZP :imgw)
    (BNE :emit-end)
    (dc "Scanline wrap")
    (add16.zp (/ +screen-width+ 8) :dest)
    (LDY 0)
    (DEC.ZP :imgh)
    (BNE :emit-end)
    (dc "We are done, pop the stack return directly to caller")
    (PLA)
    (PLA)
    (RTS)
    (label :emit-end)
    (STY.ZP :dest-y)
    (RTS)))
    
(defun draw-test ()
  (reset-compiler)
  (reset-strings)
  (let ((font :present))
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

	     (cls #x0F)
	     (typeset)
	     (image-decompressor)

	     (font-data)
	     
	     (dimg :odd "/home/dan/Downloads/odd.bmp" 104 104)
	     
	     (dcs :str (justify-with-image *odyssey*
					   104 104 font))

	     (label :end)))
      
      
      (build #'pass)))

  (monitor-reset #x600)
  (monitor-run)

  (setmem-copy (monitor-buffer)))

(defun image-byte-frequency (buf)
  (let ((arr (make-array 257 :element-type 'integer :initial-element 0)))
    (loop for b across buf do
	 (incf (aref arr b)))
    (let ((list nil))
      (loop for b across arr
	 for i from 0 do
	   (when (> b 0)
	     (push (list i b) list)))
      (sort list #'> :key #'second))))

(defun pre-encode-image (img)
  (let ((out (make-array (length img) :element-type 'integer)))
    (loop for i from 0 to (1- (length img)) do
	 (let ((index nil))
	   (setf index (position (aref img i) *even-bytes*))
	   (unless index
	     (setf index (position (aref img i) *odd-bytes*)))
	   (assert index nil "Byte ~a was not in the image byte list" (aref img i))
	   (setf (aref out i) index)))
    out))

(defun huffman-encode-image (file &key (sx 104) (sy 104))
  (let ((img (gethash file *image-cache*)))
    (when (null img)
      (setf img (posterize-image sx sy (load-image file sx sy)))
      (setf (gethash file *image-cache*) img))
    (setf img (pre-encode-image (first img)))
    (let* ((freq (image-byte-frequency img))
	   (tbl (huffman freq))
	   (lookup (build-huffman-bit-pattern-lookup tbl)))
      (print freq)
      (dump-huffman tbl nil)
      (let ((vec (make-array (length img)
			     :fill-pointer 0
			     :adjustable t
			     :element-type '(unsigned-byte 8))))
	(let ((word 0) ;;24 bits
	      (bits 0))
	  (flet ((emit ()
		   (vector-push-extend (ash word -16) vec)
		   (setf word (logand #xffff00 (ash word 8)))
		   (decf bits 8)))
	    (loop for b across img
	       for j from 0 do
		 (let ((e (gethash b lookup)))
		   (assert e nil "byte ~a not in lookup" b)
		   ;;shift the bit patter to the right so it would be at the
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
	  (values vec (/ (+ (* 8 (length vec)) bits) 8.0)))))))
      
      
      
  
	     
