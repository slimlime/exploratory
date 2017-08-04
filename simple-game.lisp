
; A simple game to test all the bits work

;; catchphrases

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *farout* "Far out!")

;;todo what happens e.g. EXAMINE DOOR WINDOW

(defun game-data ()

  ;;Define synonyms
  
  (defword :EXAMINE :LOOK)
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE :PUDDING)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH)
  
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/Downloads/cellardoor.bmp" :right
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape, but your cell has a door...")

  (with-location :dungeon-cell 
    
    (defbits t :key-in-crack :door-locked)
    (defbits nil :slime-examined :slime-licked :crack-examined
	     :door-open :slop-flung)
  
    (action '(EXAMINE SLIME)
      (respond "Millions of sad eyes peer out from the slime.")
      (ifbit :key-in-crack
	     (respond "They seem to be staring at the floor.")))
  
    (action '(EXAMINE WALL)
      (respond "The wall oozes with a repellant green slime.")
      (nifbit :slime-examined
	      (progn
		(setbit :slime-examined)
		(respond "Eugh! The slime is looking at you!"))))

    (action '(EXAMINE FLOOR)
      (respond "There is a crack in the floor.")
      (ifbit :key-in-crack
	     (respond "Perhaps it bears further examination?")))
  
    (action '(EXAMINE CRACK)
      (ifbit :slime-licked
	     (progn
	       (ifbit :key-in-crack
		      (progn
			(respond "A glint of metal shines back at you..."
				 "A key!")
			(setbit :crack-examined))
		      (respond "A crack in the floor, just like any other."
			       "One might hide a small key-like object here."
			       "Like, for example, a key.")))
	     (respond "The Veil of Maia, or your shocking hangover prevents you from seeing anything interesting.")))
	  
    (action '(TAKE KEY)
      (ifbit :crack-examined
	     (ifbit :key-in-crack
		    (progn
		      (respond "You take the shiny key.")
		      (clrbit :key-in-crack))
		    (respond "It's in your pocket..."
			     "Perhaps the dungeon air is getting to you?"))
	     (respond "What key? Do you know something I don't?")))
  
    (action '(EXAMINE DOOR)
      (respond "The door is a grim iron affair with a tiny barred window and a keyhole.") 
      (ifbit :door-open
	     (respond "The door is open.")
	     (respond "The door is closed.")))

    (action '(EXAMINE WINDOW)
      (setbit :slop-flung)
      (respond "A goblin appears at the window. He flings some inedible slop through the bars.")
      (respond *snickering*))

    (action '(EXAMINE SLOP)
      (ifbit :slop-flung
	     (respond "A balanced soup of entrails, small amphibians and mandibles. Ooh! Garlic croutons!")))

    (action '(EAT SLOP)
      (ifbit :slop-flung
	     (respond *thegodslookaway*)))
    
    (action '(EXAMINE KEY)
      (ifbit :crack-examined
	     ;;it is clear we are at the limits here of what we can do
	     ;;without an object model. Where IS the key?
	     (respond "It's a key, man.")
	     (respond "What key?")))
    
    (action '(EXAMINE KEYHOLE) (respond "It's a keyhole, man."))
    (action '(TAKE CRACK) (respond "Inadvisable."))
    (action '(LICK CRACK) (respond *thegodslookaway*))
    (action '(ATTACK SLIME) (respond "Your hand is stayed by the slime's gaze of infinite sadness."))

    (action '(UNLOCK DOOR FINGER)
      (ifbit :door-locked
	     (progn
	       (respond "Wise guy, eh? The lock doesn't budge. Your finger is now sore.")
	       (respond *snickering*))
	     (respond "You do realise you just put your finger in the keyhole of an unlocked door?")))
    
    (action '(UNLOCK DOOR)
      (ifbit :door-locked
	     (ifbit :key-in-crack
		    (respond "With what? Your finger?")
		    (progn
		      (clrbit :door-locked)
		      (respond "The lock mechanism clicks...")))
	     (respond "The door is already unlocked.")))

    (action '(CLOSE DOOR)
      (ifbit :door-open
	     (progn
	       (clrbit :door-open)
	       (respond "The door closes.")
	       (respond *thegodslookaway*))))

    (action '(LOCK DOOR)
      (ifbit :door-locked
	     (respond "The door is already locked.")
	     (progn
	       (setbit :door-locked)
	       (clrbit :door-open)
	       (respond "The lock mechanism clicks shut. You really have got it in for yourself haven't you?"))))

    (action '(EXIT)
      (ifbit :door-open
	     (respond "You go through the door...")
	     (progn
	       (respond "You walk into the closed door. Ouch.")
	       (respond *snickering*))))

    (action '(LICK SLIME)
      (nifbit :slime-licked
	      (progn
		(setbit :slime-licked)
		(respond *farout*)
		(respond "Myriad colours break in waves upon your ears. Maia's cosmic tears rain down on you in a shower of gold. The slime smiles."))
	      (respond "Nothing happens. Your third eye is already open."
		       "But... you do feel a strange urge to grow a beard and play the guitar.")))
		 
    (action '(OPEN DOOR)
      (ifbit :door-open
	     (respond "The door is already open.")
	     (ifbit :door-locked
		    (respond
		     "Have you been licking the slime? It's hallucinogenic."
		     "The door, not unusually for a dungeon, is locked.")
		    (progn
		      (respond *farout*)
		      (respond "The door creaks open.")
		      (setbit :door-open))))))

  (with-location :generic
    (action '(TAKE KEY)
      (nifbit '(:dungeon-cell . :key-in-crack)
	      (respond "You already have the key!"
		       "Perhaps the dungeon air is getting to you.")))))

(defparameter origin #x600)

(defun build-game (&key (full-reset nil) (quick-parser t))

  (when full-reset
    (reset-image-cache))
  
  (reset-compiler)
  (reset-symbol-table)
  (reset-bits)
  (reset-parser)

  ;;this (atm) must be called so the widths are initialised
  ;;todo, make a font-init function which just does the widths.
  (font-data)
  (reset-compiler)
  
  (flet ((pass ()

	   (reset-parser-between-passes)
	   
	   (zeropage)	     
	   (org origin)
	   (CLD)

	   (label :start)

	   (JSR '(:dungeon-cell . :draw))

	   (label :test)
	   (label :test2)
	   (sta16.zp (cons :dispatcher :dungeon-cell)
		     :location-dispatch-table)

	   (BRK)

	   (label :test-input)
	   (JSR :parse)
	   (JSR :dispatch)

	   (BRK)

	   ;;game state
	   
	   (game-data)
	   (bit-table)
	   
	   ;;inline functions we will need

	   (print-message)
	   (memcpy)
	   (memset)
	   (typeset)

	   ;;process all the words we need for the parser
	   
	   (unless *hash-fudge-factors* ;;only build the parser hash table once
	     (build-hash-table quick-parser))	   

	   (parser)
	   
	   (when *word-collisions*
	     (binary-parser))

	   (dispatcher)
	   (string-table)
	   
	   (image-decompressor)
	   (label :end)
	   ;font data is pretty boring so stick it here
	   (font-data)
	   ))
    
    (pass)
    (build-symbol-table)

    ;;penultimate pass to ensure everything got a go and the structure
    ;;hasn't changed

    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))
    
      (setf *compiler-final-pass* t)
      (pass)

      (format t "Build size ~a~%" (- *compiler-ptr* origin))))

(defun enter-input (str &key (break-on 'brk))
  ;;really need to make better 6502 emulator to avoid need
  ;;for copying the buffer
  (let ((buffer (cl-6502:get-range 0)))
    ;;we must clear the buffer for new input
    (loop for i from 0 to *max-input-length* do
	 (setf (aref buffer (+ i (resolve '(:parser . :input)))) 0))
    
    (loop for c across str
       for i from 0 to *max-input-length* do       
	 (setf (aref buffer (+ i (resolve '(:parser . :input))))
	       (to-alphabet-pos c)))
    (setf (cl-6502:get-range 0) buffer))
  (monitor-setpc :test-input)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer))
  ;;(vicky)
  (values))

;;TODO need a test case for the input EXAMINE SLIME as it fails to terminate
;;ends up with two random words at the end
;;TODO the hyphen shouldn't be tacked on in the string table
;;TODO parser seems to take ages, shouldn't take that long
;;TODO fails to parse now, something todo with enter-input
;;todo CLEAVE WIZARD == EXIT DOOR
;;TODO TOUCH SLIME
;;TODO ADD MATTOCK

(defun run-game (&optional (break-on 'BRK))
  (build-game)
  (monitor-reset #x600)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer)))

;;A goblin appears at the door. He flings some inedible slop through the bars.

;;(dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/Downloads/goblincloset.bmp" :right
;;      "You are in the well-appointed closet of the goblin guard Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a single chair (broken), a spare loin-cloth (tattered), a toaster and a hundred-year-old copy of Modern Necromancer magazine.")

;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'

;;You gaze into the toaster. The toaster gazes into you.
