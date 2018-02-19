(defparameter *images* nil)
(defparameter *image-huffman-lookup* nil)

(defparameter *image-cache* (make-hash-table :test 'equal))

;; This must be called before each build

(defun reset-image-table ()
  (setf *images* (make-hash-table :test 'equal))
  (setf *image-huffman-lookup* nil))

;; A cache of converted images, since it takes so long to posterize them
;; Unless the posterization changes, we can save this between builds

(defparameter *posterized-image-cache* nil)

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
  (aif (gethash file *image-cache*)
       it
       (setf (gethash file *image-cache*)
	     (posterize-image sx sy (load-image file sx sy)))))

(defun dimg (name file sx sy)
  (assert (= 0 (mod sx 8)))
  (assert (= 0 (mod sy 8)))
  (if *image-huffman-lookup*
      ;;stick the image data here
      (destructuring-bind (pixels colours) (get-posterized-image file sx sy)
	(dc (format nil "Image ~a ~ax~a (~a)" name sx sy file))
	(let ((data (huffman-encode-vector (pre-encode-image pixels)
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
	  (loop for byte across (pre-encode-image pixels) do
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
    (LDY 0)
    (label :next)
    (STY.ZP :column)
    (JSR :huffman-next)
    (LDA.ZP :height-pixels)
    (AND.IMM 1 "Odd or even scanline?")
    (BEQ :odd)
    (LDA.ABX :even-bytes)
    (BPL :emit "Assume positive, since only 81 indices are possible")
    (label :odd)
    (LDA.ABX :odd-bytes)
    (label :emit)
    (LDY.ZP :column)
    (STA.IZY :dst)
    (INY)
    (CPY.ZP :width-bytes)
    (BNE :next)
    (DEC.ZP :height-pixels)
    (BEQ :done)
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
	   (dimg :img3 "~/exploratory/images/face.bmp" 104 104)
	   (image-table)
	   (label :end)))
    (build #'pass))
  (monitor-reset #x600)
  (monitor-run)
  (setmem-copy (monitor-buffer)))
