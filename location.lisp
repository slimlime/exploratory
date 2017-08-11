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

(defun call-typeset (str font line)
  ;;this is in reality a terrible way to call it
  ;;as it takes so many bytes, this is just for testing
  (sta16.zp (cons :font font) :font)
  (sta16.zp str '(:typeset-cs . :str))
  (sta16.zp (scradd line 0) '(:typeset . :raster))
  (JSR :typeset-cs))

(defparameter *text-line-offset* 1)

(defun fleuron ()
   (with-namespace :fleuron
     (alias :raster :A0)
     (alias :left-col :D0)
     (label :middle-fleuron nil)
     (STA.ZP :D0)
     (sta16.zp (scradd (+ *text-line-offset* *font-height*
			  (* *line-height* *max-lines*)) 0) :raster)
     (JMP :draw-fleuron)
     (label :top-fleuron nil)
     (STA.ZP :D0)
     (sta16.zp (scradd *font-height* 0) :raster)
     (label :draw-fleuron)
     (LDX 9)
     (label :next-row)
     (SEC)
     (sbc16.zp +screen-width-bytes+ :raster)
     (LDA.ABX :rope)
     (LDY (1- +screen-width-bytes+))
     (label :next-column)
     (STA.IZY :raster)
     (DEY)
     (CPY.ZP :D0)
     (BPL :next-column)
     (DEX)
     (BPL :next-row)
     (RTS)
     
     (db :rope 0 0 #xc3 #x24 #x99 #x42 #x99 #x24 #xc3 0)))

(defun middle-fleuron-column (text font)
  (if (< (count #\Newline text) (1- *max-lines*))
      0
      (1+ (ash (measure-word (subseq text (1+ (position #\Newline text :from-end t)))
			 font)
	       -3))))

  
(defun location ()
  (label :go-location)
  (with-namespace :location



;;TODO this takes far too many bytes to push all the parameters
;;need to pass them by reference
(defun dloc (name title img-file img-align text
	     &key (font *act-font*) (colour *act-colour*))
  (let ((sx 104) (sy 104))
    (with-namespace name
      (setf text
	    (if img-file
		(justify-with-image text sx (- sy (+ 1 *line-height*)) font)
		(justify-with-image text 0 0 font)))
      (assert (< (count #\Newline text) *max-lines*) nil
	      (format nil "The location description exceeds ~a lines ~%~a"
		      *max-lines*
		      text))    
      (label :draw)
      (dc (format nil "Draw ~a (~a)" title name))
      (cls colour)
      (sta16.zp (cons :font font) :font)
      (call-typeset :title font 0)
      (LDA (1+ (ash (measure-word title font) -3)))
      (JSR :top-fleuron)
      (call-typeset :text font (+ *text-line-offset* *line-height*))
      (LDA (middle-fleuron-column text font))
      (JSR :middle-fleuron)
      ;; todo get image width and height from file
      (draw-image name sx sy img-align)
      (RTS)
      (dc (format nil "Image data for ~a (~a)" title name))
      (dimg name img-file sx sy)
      (dcs :title title)
      (dcs :text text))))

(defun scroller (label lines)
  (label label)
  (call-memcpy (scradd (live-row 1) 0)
	       (scradd (live-row 0) 0)
	       (* lines +screen-width-bytes+ *line-height*))
  (call-memset 0 (scradd (live-row lines) 0)
	       (* +screen-width-bytes+ *line-height*))
  (RTS))

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
	   (fleuron)

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
