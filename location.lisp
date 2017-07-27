;; A location is defined by its label. It has a title, some text and an image
;; it will also have a bunch of responses to user input, and responses to timed
;; events etc.

;;todo intern strings

(defparameter *act-font* :present)
(defparameter *act-colour* #x10)

;;TODO render live message
;;TODO enter user character, delete user character
;;TODO left image align
;;TODO user input caret |
;;TODO fleuron          | use same drawing thingy
;;TODO get image size in dloc

(defparameter *max-lines* 12)

(defun live-row (i)
  (+ 3 (* (+ i 13) *line-height*)))

(defun scroller (label lines)
  (label label)
  (call-memcpy (scradd (live-row 1) 0)
	       (scradd (live-row 0) 0)
	       (* lines +screen-width-bytes+ *line-height*))
  (call-memset 0 (scradd (live-row lines) 0)
	       (* +screen-width-bytes+ *line-height*))
  (RTS))

(defun call-typeset (str font line)
  ;;this is in reality a terrible way to call it
  ;;as it takes so many bytes, this is just for testing
  (sta16.zp (cons :font font) :font)
  (sta16.zp str '(:typeset-cs . :str))
  (sta16.zp (scradd line 0) '(:typeset . :raster))
  (JSR :typeset-cs))
				      
(defun dloc (name title img-file img-align text
	     &key (font *act-font*) (colour *act-colour*))
  (with-namespace name
    (label :draw)
    (dc (format nil "Draw ~a (~a)" title name))
    (cls colour)
    (sta16.zp (cons :font font) :font)
    (call-typeset :title font 0)
    (call-typeset :text font (+ 1 *line-height*))
    ;; todo get image width and height from file
    (let ((sx 104) (sy 104))      
      (draw-image :image sx sy img-align)
      (RTS)
      (dc (format nil "Image data for ~a (~a)" title name))
      (dimg :image img-file sx sy)
      (dcs :title title)
      (let ((text
	     (if img-file
		 (justify-with-image text sx (- sy (+ 1 *line-height*)) font)
		 (justify-with-image text 0 0 font))))
	(assert (< (count #\Newline text) *max-lines*) nil
		(format nil "The location description exceeds ~a lines ~%~a"
			*max-lines*
			text))
	(dcs :text text)))))
	   
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

	   ;;(JSR :scroll-text)

	   (BRK)

	   (dolist (v '("Live 1"
			"Live 2"
			"Live 3"
			"Live 4"
			"User input"))
	     (dcs v v))

	   (scroller :scroll-all 4)
	   (scroller :scroll-text 3)
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
