;; A location is defined by its label. It has a title, some text and an image
;; it will also have a bunch of responses to user input, and responses to timed
;; events etc.

;;todo intern strings

(defparameter *act-font* :present)
(defparameter *act-colour* #x10)

(defun call-typeset (cstr font y)
  (sta16.zp cstr '(:typeset-cs . :str))
  (sta16.zp (cons :font font) :font)
  (sta16.zp (+ *screen-memory-address*
	       (* (/ +screen-width+ 8) y))
	    '(:typeset . :raster))
  (JSR :typeset-cs))

;;TODO scroll live messages
;;TODO render live message
;;TODO enter user character, delete user character
;;TODO left image align
;;TODO user input caret |
;;TODO fleuron          | use same drawing thingy
;;TODO get image size in dloc

(defun live-row (i)
  (+ 3 (* (+ i 13) *line-height*)))

(defun scroll ()
  (label :scroll)
  (call-memcpy (scradd (live-row 1) 0)
	       (scradd (live-row 0) 0)
	       (* 4 +screen-width-bytes+ *line-height*))
  (call-memset 55 (scradd (live-row 4) 0)
	       (* +screen-width-bytes+ *line-height*)))

(defun dloc (name title img-file img-align text
	     &key (font *act-font*) (colour *act-colour*))
  (with-namespace name
    (label :draw)
    (dc (format nil "Draw ~a (~a)" title name))
    (call-cls colour)
    (call-typeset :title font 0)
    (call-typeset :text font (+ 1 *line-height*))
    (draw-image :image 104 104 img-align)
    (RTS)
    (dc (format nil "Image data for ~a (~a)" title name))
    (dimg :image img-file 104 104)
    (dcs :title title)
    (dcs :text
	 (if img-file
	     (justify-with-image text 104 (- 104 (+ 1 *line-height*)) font)
	     (justify-with-image text 0 0 font)))))

(defun location-test ()
  (reset-compiler)
  (reset-symbol-table)
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (label :loc-test)

	   (JSR '(:rickety-stair . :draw))

	   (call-typeset "Live 1"
			 :present (+ 3 (* 13 *line-height*)))
	   (call-typeset "Live 2"
			 :past (+ 3 (* 14 *line-height*)))
	   (call-typeset "Live 3"
			 :future (+ 3 (* 15 *line-height*)))
	   (call-typeset "Live 4"
			 :present (+ 3 (* 16 *line-height*)))
	   (call-typeset "User input"
			 :past (+ 3 (* 17 *line-height*)))

	   (scroll)

	   (BRK)

	   (dolist (v '("Live 1"
			"Live 2"
			"Live 3"
			"Live 4"
			"User input"))
	     (dcs v v))
	   
	   (cls)
	   (memcpy)
	   (memset)
	   (typeset)
	   (image-decompressor)

	   (font-data)
	   	   
	   (dloc :rickety-stair
		 "DANK STAIRCASE"
		 "/home/dan/Downloads/cellardoor.bmp" :right
		 "You are on a dank and foul smelling staircase. The door at the top is haloed with a brilliant light, encouraging you towards it. From behind the door emanates the sound of voices and merriment. A party? You feel like you should remember... These are extra lines added to the test string so that we have a total of 12, which gives enough room for the user input.")
	   (label :end)))
      
    (build #'pass))
  
  (monitor-reset #x600)
  (monitor-run)
  
  (setmem-copy (monitor-buffer)))
