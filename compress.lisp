;; TODO popcount reduction is broken as it looks rubbish if we
;; just swap the bytes round. Break the posterization into 2- if we want popcount
;; reduction we have to go round choosing the bytes again, but with a fixed colour
;; Secondly, we need to measure how badly the images are affected by sharing the
;; huffman table
;; Third, how many empty squares are there? Do they have FG=BG in addition to popcount 0
;; might improve the statistics
;; Fourth, If we share one Huffman table over the whole attributes, does it improve at all?

(defparameter *images* nil)
(defparameter *image-huffman-lookup* nil)

;; A cache of converted images, since it takes so long to posterize them
;; Unless the posterization changes, we can save this between builds

(defparameter *posterized-image-cache* (make-hash-table :test 'equal))

;; This must be called before each build

(defun reset-image-table ()
  (setf *images* (make-hash-table :test 'equal))
  (setf *image-huffman-lookup* nil))

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
	  out)))

(defun get-posterized-image (file sx sy)
  (aif (gethash file *posterized-image-cache*)
       it
       (setf (gethash file *posterized-image-cache*)
	     (posterize-image sx sy (load-image file sx sy) :reduce-popcount nil))))

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
	(label :colours name)
	(add-hint (length colours) (format nil "~a colour data ~a bytes" name (length colours)))
	(loop for c across colours do (push-byte c)))
      ;;otherwise just add it to the list to be processed
      (setf (gethash name *images*) (list file sx sy))))

(defun image-table ()
  (let ((freq (make-array 256 :element-type 'integer :initial-element 0)))
    (do-hashtable (name image *images*)
      (declare (ignore name))
      (destructuring-bind (file sx sy) image
	(destructuring-bind (pixels colors) (get-posterized-image file sx sy)
	  (declare (ignore colors))
	  ;;get byte frequencies across all images
	  (loop for byte across (pre-encode-image pixels sx sy) do
	       (incf (aref freq byte))))))
    ;;filter out the 0 occurances and sort
    (let ((tbl nil))
      (loop for f across freq
	 for i from 0 do
	   (when (> f 0)
	     (push (list i f) tbl)))
      (setf tbl (huffman (sort tbl #'> :key #'second)))
      (when *compiler-final-pass* (print tbl))
      (setf *image-huffman-lookup*
	    (build-huffman-bit-pattern-lookup tbl))
      (huffman-pop-table :pixel-population tbl "Image Pixel Huffman Table")
      ;;now we have to be able to translate from indices to bytes
      ;;using the amazing tables, which we interleave so we can
      (apply #'db :even-bytes (mapcar #'(lambda (e) (aref *even-bytes* (first e))) tbl))
      (apply #'db :odd-bytes (mapcar #'(lambda (e) (aref *odd-bytes* (first e))) tbl)))))

(defun set-attributes (colour)
  (call-memset colour *char-memory-address* +char-memory-length+))

;; Render an image to the screen
(defun image-decompressor ()
  (label :decompress-pixels)
  (with-namespace :decompress-pixels
    ;;these parameters must be set
    (alias :src :huffman-ptr)
    (alias :dst :A1)
    (alias :width-bytes :D1)
    (alias :height-pixels :D2)
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
    (CPY.ZP :width-bytes)
    (BNE :next)
    (DEC.ZP :height-pixels)
    (BEQ :done)
    (dc "Swap the lookup table from even to odd")
    (dc "and vice-versa, using the XOR trick.")
    ;;cost, 20 cycles per line, 2ms per 104 pixel
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
    (RTS))

  ;;TODO Allow definition of offset labels
  ;;     to make self modifying code easier
  
  (label :decompress-colours)
  (with-namespace :decompress-colours
    ;; X to contain height/8 - 1
    ;; Y to contain width/8
    ;; src to source src+1
    ;; dst to source dst+1
    (STY.AB :width)
    (label :next-row)
    (label+1 :width)
    (LDY 0)
    (label :next-col)
    (label+1 :src)
    (LDA.ABY 0)
    (label+1 :dst)
    (STA.ABY 0)
    (DEY)
    (BPL :next-col)
    (add16.ab (/ +screen-width+ 8) :dst)
    ;;let us assume the carry is clear after the last add
    (LDA.AB (lo-add :src))
    (SEC "width is width -1, so sec to compensate")
    (ADC.AB :width)
    (STA.AB (lo-add :src))
    (LDA.AB (hi-add :src))
    (ADC 0)
    (STA.AB (hi-add :src))
    (DEX)
    (BPL :next-row)
    (RTS))) 

(defun draw-test () 
  (reset-compiler)
  (reset-image-table)
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (label :start)
	   (set-attributes #xf0)
	   (CLD)
	   (LDA 13)
	   (STA.ZP '(:decompress-pixels . :width-bytes))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-pixels . :height-pixels))
	   (sta16.zp (cons :img1 :pixels) '(:decompress-pixels . :src))
	   (sta16.zp *screen-memory-address* '(:decompress-pixels . :dst))
	   (JSR :decompress-pixels)
	   (sta16.ab (cons :img1 :colours) '(:decompress-colours . :src))
	   (sta16.ab *char-memory-address* '(:decompress-colours . :dst))
	   (LDY 12)
	   (LDX 12)
	   (JSR :decompress-colours)
	   (LDA 13)
	   (STA.ZP '(:decompress-pixels . :width-bytes))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-pixels . :height-pixels))
	   (sta16.zp (cons :img2 :pixels) '(:decompress-pixels . :src))
	   (sta16.zp (+ 13 *screen-memory-address*) '(:decompress-pixels . :dst))
	   (JSR :decompress-pixels)
	   (sta16.ab (cons :img2 :colours) '(:decompress-colours . :src))
	   (sta16.ab (+ 13 *char-memory-address*) '(:decompress-colours . :dst))
	   (LDY 12)
	   (LDX 12)
	   (JSR :decompress-colours)
	   (LDA 13)
	   (STA.ZP '(:decompress-pixels . :width-bytes))
	   (ASL)
	   (ASL)
	   (ASL)
	   (STA.ZP '(:decompress-pixels . :height-pixels))
	   (sta16.zp (cons :img3 :pixels) '(:decompress-pixels . :src))
	   (sta16.zp (+ 26 *screen-memory-address*) '(:decompress-pixels . :dst))
	   (JSR :decompress-pixels)
	   (sta16.ab (cons :img3 :colours) '(:decompress-colours . :src))
	   (sta16.ab (+ 26 *char-memory-address*)'(:decompress-colours . :dst))
	   (LDY 12)
	   (LDX 12)
	   (JSR :decompress-colours)
	   (BRK)
	   (memset)
	   (huffman-decoder)
	   (image-decompressor)
	   (dimg :img1 "~/exploratory/images/porsche.bmp" 104 104)
	   (dimg :img2 "~/exploratory/images/cell.bmp" 104 104)
	   (dimg :img3 "~/exploratory/images/dog2.bmp" 104 104)
	   (image-table)
	   (label :end)))
    (build #'pass))
  (monitor-reset #x600)
  (monitor-run)
  (setmem-copy (monitor-buffer)))
