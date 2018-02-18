(defparameter *images* nil)
(defparameter *image-huffman-table* nil)
(defparameter *image-huffman-lookup* nil)

;; This must be called before each build

(defun reset-image-table ()
  (setf *images* (make-hash-table :test 'equal))
  (setf *image-huffman-table* nil)
  (setf *image-huffman-lookup* nil))

;; A cache of converted images, since it takes so long to posterize them
;; Unless the posterization changes, we can save this between builds

(defparameter *posterized-image-cache* nil)

(defun reset-image-cache ()
  (setf *image-cache* (make-hash-table :test 'equal)))

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

(defun pre-encode-colors (colors)
  (let ((out (make-array (* 2 (length colors)) :element-type 'integer)))
    (loop for i from 0 to (1- (length colors))
       for j from 0 by 2 do
	 (setf (aref out j) (ash (aref colors i) -4))
	 (setf (aref out (1+ j)) (logand #xf (aref colors i))))
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

(defun huffman-encode-image (file &key (sx 104) (sy 104))
  (let ((img (gethash file *image-cache*)))
    (when (null img)
      (setf img (posterize-image sx sy (load-image file sx sy)))
      (setf (gethash file *image-cache*) img))
    (let* ((data (pre-encode-image (first img)))
	   (freq (image-byte-frequency data))
	   (tbl (huffman freq))
	   (lookup (build-huffman-bit-pattern-lookup tbl)))
      ;;(dump-huffman tbl nil)
      (let ((encoded-data (huffman-encode-vector data lookup)))
	(format t "Size ~a~%" (length encoded-data))))
    (let* ((colors (pre-encode-colors (second img)))
	   (freq (image-byte-frequency colors))
	   (tbl (huffman freq))
	   (lookup (build-huffman-bit-pattern-lookup tbl)))
      (dump-huffman tbl nil)
      (let ((encoded-colors (huffman-encode-vector colors lookup)))
	(format t "Size ~a~%" (length encoded-colors))))))

(defun get-posterized-image (file sx sy)
  (aif (gethash file *image-cache*)
       it
       (posterize-image sx sy (load-image file sx sy))))

;; On the first pass, lets just
(defun dimg (name file sx sy)
  (assert (= 0 (mod sx 8)))
  (assert (= 0 (mod sy 8)))
  (if *image-huffman-table*
      (progn
	(dc (format nil "Image ~a ~ax~a (~a)" name sx sy file))
	(destructuring-bind (pixels colors) (get-posterized-image file sx sy)
	  (huffman-encode
      ;;otherwise just add it to the list to be processed
      (setf (gethash name *images*) (list file sx sy)))))

(defun image-huffman-table ()
  (let ((freq (make-array 256 :element-type 'integer :initial-element 0)))
    (do-hashtable (name image *images*)
      (declare (ignore name))
      (destructuring-bind (file sx sy) image
	(destructuring-bind (pixels colors) (get-posterized-image file sx sy)
	  (declare (ignore colors))
	  ;;get byte frequencies across all images
	  (loop for byte across pixels do (incf (aref freq byte))))))
    ;;filter out the 0 occurances and sort
    (let ((tbl nil))
      (loop for f across freq
	 for i from 0 do
	   (when (> f 0)
	     (push (list i f) tbl)))
      (setf *image-huffman-table* (huffman (sort tbl #'> :key #'second)))
      (setf *image-huffman-lookup*
	    (build-huffman-bit-pattern-lookup *image-huffman-table*)))))

(defun set-attributes (colour)
  (call-memset colour *char-memory-address* +char-memory-length+))

(defun draw-image (image w h &optional (img-align :right))
  ;;TEST FUNCTION ONLY.
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
  (label :decompress-image)
  (with-namespace :decompress-image
    ;;these parameters must be set
    (alias :src :huffman-ptr)
    (alias :dst :A1)
    (alias :w :D1)
    (alias :h :D2)

    
    (label :emit)
    (dc "Emit a byte to the screen")
    (LDY.ZP :dest-y)
    (STA.IZY :dest)
    (INY)
    (TYA)
    (CMP.ZP :w)
    (BNE :emit-end)
    (dc "Scanline wrap")
    (add16.zp (/ +screen-width+ 8) :dest)
    (LDY 0)
    (DEC.ZP :h)
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
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (draw-image :odd 104 104)
	   (BRK)
	   (image-decompressor)
	   (dimg :odd "/home/dan/Downloads/odd.bmp" 104 104)
	   (label :end)))
    (build #'pass))
  (monitor-reset #x600)
  (monitor-run)
  (setmem-copy (monitor-buffer)))
