;; A location is defined by its label. It has a title, some text and an image
;; it will also have a bunch of responses to user input, and responses to timed
;; events etc.

;;todo intern strings

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

(defun navigator ()
  
  (label :restore-game)
  (dc "Navigate to the restored location")
  (JSR :navigate)
  (game-state-bytes "Location"
    ;;the location word here doubles as the parameter
    ;;to the JSR above, and as the state which is
    ;;saved.
    (dw :current-location 0))
  (BRK)

  (label :navigate)
  
  (with-namespace :navigate
    (alias :loc :A4)
    ;;We could use the 'call' graph to figure out which things
    ;;to check to ensure the variable we use here do clash

    (ensure-aliases-different '(:A4) (get-aliased-labels :typeset-cs))
    (ensure-aliases-different '(:A4) (get-aliased-labels :typeset))
    (ensure-aliases-different '(:A4) (get-aliased-labels :decompress))
   
    (alias :str '(:typeset-cs . :str))
    (alias :dest '(:decompress . :dest))
    (alias :data '(:decompress . :data))
    (JSR :deref-w)
    (dc "Store the address of the current location")
    (dc "In the zeropage so we can use it")
    (label :navigate-no-deref)
    (STX.ZP (lo-add :loc))
    (STA.ZP (hi-add :loc))
    (dc "And in main memory so we can save it")
    (STX.AB (lo-add :current-location))
    (STA.AB (hi-add :current-location))
    (dc "Clear the top half of the screen")
    (call-memset 0 *screen-memory-address*
		 (* (live-row 0) +screen-width-bytes+))

    (dc "Set the current place")
    (LDY 14)
    (LDA.IZY :loc)
    (STA.ZP :current-place)
    (dc "Get the title string")
    (LDY 0)
    (LDA.IZY :loc)
    (STA.ZP (lo-add :str))
    (INY)
    (LDA.IZY :loc)
    (STA.ZP (hi-add :str))
    (sta16.zp (scradd 0 0) '(:typeset . :raster))
    ;;(JSR :typeset-cs)
    (dc "Get the left column for the title fleuron")
    (LDY 2)
    (LDA.IZY :loc)
    (JSR :top-fleuron)
    (dc "Get the text")
    (LDY 3)
    (LDA.IZY :loc)
    (STA.ZP (lo-add :str))
    (INY)
    (LDA.IZY :loc)
    (STA.ZP (hi-add :str)) 
    (sta16.zp (scradd (+ *text-line-offset* *line-height*) 0)
	      '(:typeset . :raster))
    (JSR :typeset-cs)
    (dc "Get the left column for the middle fleuron")
    (LDY 5)
    (LDA.IZY :loc)
    (JSR :middle-fleuron)
    (dc "Get the image width in bytes")
    (LDY 6)
    (LDA.IZY :loc)
    (STA.ZP '(:decompress . :imgw))
    (let ((dst (+ +screen-width-bytes+ *screen-memory-address*)))
      (dc "Work out where to put the image")
      (LDA (lo dst))
      (SEC)
      (SBC.ZP '(:decompress . :imgw))
      (STA.ZP (lo-add :dest))
      (LDA (hi dst))
      (SBC 0)
      (STA.ZP (hi-add :dest))
      (dc "Now the address of the pixels")
      (INY)
      (LDA.IZY :loc)
      (STA.ZP (lo-add :data))
      (INY)
      (LDA.IZY :loc)
      (STA.ZP (hi-add :data))
      (dc "Now the height of the image")
      (INY)
      (LDA.IZY :loc)
      (JSR :decompress)
      (dc "Work out where to put the colour data image")
      (setf dst (+ +screen-width-bytes+ *char-memory-address*))
      (LDA (lo dst))
      (SEC)
      (SBC.ZP '(:decompress . :imgw))
      (STA.ZP (lo-add :dest))
      (LDA (hi dst))
      (SBC 0)
      (STA.ZP (hi-add :dest))
      (dc "Get the colour data for the image")
      (LDY 10)
      (LDA.IZY :loc)
      (STA.ZP (lo-add '(:decompress . :data)))
      (INY)
      (LDA.IZY :loc)
      (STA.ZP (hi-add '(:decompress . :data)))
      (dc "Set the location dispatch table while we are here")
      (INY)
      (LDA.IZY :loc)
      (STA.ZP (lo-add :location-dispatch-table))
      (INY)
      (LDA.IZY :loc)
      (STA.ZP (hi-add :location-dispatch-table))
      (dc "Now the height again and divide by 8; colour attributes")
      (dc "are in blocks of 8x8")
      (LDY 9)
      (LDA.IZY :loc)
      (LSR)
      (LSR)
      (LSR)
      (dc "Tail jump to decompress the colours and return")
      (JMP :decompress))))
  
(defun dloc (name title img-file text)
  (label name)
  (let ((sx 104) (sy 104))
    (with-namespace name
      (setf text
	    (if img-file
		(justify-with-image text sx (- sy (+ 1 *line-height*)) :present)
		(justify-with-image text 0 0 :present)))
      (assert (< (count #\Newline text) *max-lines*) nil
	      (format nil "The location description exceeds ~a lines ~%~a"
		      *max-lines*
		      text))
      ;;0
      (dw :title (dstr title))
      ;;2
      (db :tfleur-col (1+ (ash (measure-word title :present) -3)))
      ;;3
      (dw :text (dstr text))
      ;;5
      (db :mfleur-col (middle-fleuron-column text :present))
      ;;6
      (db :imgw (/ sx 8))
      ;;7
      (dw :pixels (cons name :pixels))
      ;;9
      (db :imgh sy)
      ;;10
      (dw :colours (cons name :colours))
      ;;12
      (dw :dispatch (cons :dispatcher name))
      ;;14
      (db :place (defplace name))
      (dimg name img-file sx sy))))

(defun location-test ()
  
  (reset-compiler)
  (reset-strings)
  (reset-bits)
  (reset-parser)

  ;;this (atm) must be called so the widths are initialised
  ;;todo, make a font-init function which just does the widths.

  (font-data)
  (reset-compiler)

  (let ((pass-number 0))
    (flet ((pass ()
	     (format t "Pass ~a~%" (incf pass-number))
	     (zeropage)	     
	     (org #x600)
	     (CLD)
	     (label :start)

	     (sta16.zp (cons :font :present) :font)
	     
	     (navigate :rickety-stair)
	     
	     (BRK)
	     
	     (memcpy)
	     (memset)
	     (typeset)
	     (image-decompressor)
	     (fleuron)
	     (navigator)
	     (deref-w)
	     
	     (dloc :rickety-stair
		 "DANK STAIRCASE"
		 "/home/dan/Downloads/cellardoor.bmp"
		 "You are on a dank and foul smelling staircase. The door at the top is haloed with a brilliant light, encouraging you towards it. From behind the door emanates the sound of voices and merriment. A party? You feel like you should remember... These are extra lines added to the test string so that we have a total of 12, which gives enough room for the user input.")

	     ;;TODO, This has to go after the strings, otherwise it makes the build
	     ;;unstable. Presumably another pass would fix it, but it should be
	     ;;stable by this point shouldn't it?
	     (string-table)
	     
	     (label :end)
	     (font-data)
	     
	     ))
      (pass)

      (pass)
      (let ((end *compiler-ptr*))
	(pass)
	(assert (= end *compiler-ptr*) nil "Build was not stable"))
      (setf *compiler-final-pass* t)
      (pass))
    
    (monitor-reset #x600)
    ;;(monitor-run)
    (monitor-run :break-on :debug)
    
    (setmem-copy (monitor-buffer))))
