;; A simple game to test all the bits work

;; TODO See why the labels seem to be in the labels list twice
;; oh yes, this is an artefact of them not being in the same
;; place in the second pass. Once the structure is sorted out
;; it should be fine, though it might be better to use a
;; counter which will give the same effect.

;; TODO ensure justify will split on a -
;; TODO ensure that the location string fits and leaves 4 lines at the bottom

;; EXAMINE WALL
;; EXAMINE SLIME
;; EXAMINE FLOOR
;; EXAMINE CRACK
;; OPEN DOOR
;; LICK SLIME
;; EXAMINE CRACK
;; TAKE KEY
;; UNLOCK DOOR
;; OPEN DOOR
;; EXIT

(defun game-data ()

  (defword :EXAMINE :LOOK)
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :OPEN)
  (defword :LOCK)
  (defword :CLOSE)
  (defword :UNLOCK)

  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/Downloads/cellardoor.bmp" :right
	"You are in the dungeon prison of Wangband underneath the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. Surrounding you is a slime that reminds you of lime pudding, if lime pudding had hundreds of eyes and made a rasping, wheezing sound as it oozed out of the cracks in the wall. You must escape.")

  (with-location :dungeon-cell 

    (defword :SLIME :PUDDING)
    (defword :WALL)
    (defword :FLOOR)
    (defword :CRACK)
    (defword :KEY)
    (defword :DOOR)
    (defword :EXIT :OUT :GO)
  
    (defbits t :key-in-crack)
    (defbits nil :slime-examined :slime-licked :crack-examined :door-locked
	     :door-open)
  
    (action '(EXAMINE SLIME)
	    (respond "Millions of sad eyes gaze at you from the slime.")
	    (ifbit :key-in-crack
		   (respond "They seem to be staring at the floor.")))
  
    (action '(EXAMINE WALL)
	    (respond "The wall oozes with a repellant green slime.")
	    (nifbit :slime-examined
		    (progn
		      (setbit :slime-examined)
		      (respond "Eugh! The slime appears to be looking at you!"))))

    (action '(EXAMINE FLOOR)
	    (respond "There is a crack in the floor.")
	    (ifbit :key-in-crack
		   (respond "Perhaps it bears further examination?")))
  
    (action '(EXAMINE CRACK)
	    (ifbit :slime-licked
		   (progn
		     (ifbit :key-in-crack
			    (progn
			      (respond "A glint of metal shines back at you from the crack."
				       "A key!")
			      (setbit :crack-examined))
			    (respond "A crack in the floor, just like any other crack"
				     "One might hide a small object here, perhaps a key?")))
		   (respond "The veil of Maia, or perhaps your shocking hangover"
			    "prevents you from seeing anything interesting.")))
	  
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
	    (ifbit :door-open
		   (respond "The door is open.")
		   (ifbit :door-locked
			  (respond "The door is locked.")
			  (respond "The door is closed."))))

    (action '(UNLOCK DOOR)
	    (ifbit :door-locked
		   (ifbit :key-in-crack
			  (respond "With what? Your finger?")
			  (progn
			    (clrbit :door-locked)
			    (respond "The lock mechanism clicks...")))
		   (respond "The door is already unlocked.")))

    (action '(EXIT DOOR)
	    (ifbit :door-open
		   (respond "You go through the door...")
		   (respond "You walk into the closed door. Ouch.")))

    (action '(LICK SLIME)
	    (ifbit :slime-licked
		   (progn
		     (setbit :slime-licked)
		     (respond "Millions of colours flash in geometric patterns"
			      "before your eyes. Space and time warp as Maia's"
			      "cosmic tears  rain down on you in a shower of gold."
			      "You feel a strange urge to grow a beard and play the guitar."))
		   (respond "Nothing happens. Your third eye is already open.")))
		 
    (action '(OPEN DOOR)
	    (ifbit :door-open
		   (respond "The door is already open.")
		   (ifbit :door-locked
			  (respond
			   "Have you been licking the slime? It's hallucinogenic."
			   "The door, typically for a dungeon, is locked.")
			  (progn
			    (respond "The door creaks open.")
			    (setbit :door-open))))))

  (with-location :generic
    (action '(TAKE KEY)
	    (nifbit '(:dungeon-cell . :key-in-crack)
		    (respond "You already have the key!"
			     "Perhaps the dungeon air is getting to you.")))))

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
	   (zeropage)	     
	   (org #x600)
	   (CLD)

	   (label :start)

	   (JSR '(:dungeon-cell . :draw))
	   
	   (BRK)

	   ;;test harness

	   (label :print-message nil)
	   (BRK)

	   ;;game state
	   
	   (game-data)
	   (bit-table)
	   
	   
	   ;;inline functions we will need

	   (scroller :scroll-all 4)
	   (scroller :scroll-text 3)
	   (memcpy)
	   (memset)
	   (typeset)

	   ;;process all the words we need for the parser
	   
	   (unless *hash-fudge-factors* ;;only build the parser hash table once
	     (build-hash-table quick-parser))	   

	   (parser)
	   
	   (when *word-collisions*
	     (binary-parser))

	   (string-table)
	   (font-data)
	   (image-decompressor)
	   
	   (label :end)))
    
    (pass)
    (build-symbol-table)

    ;;penultimate pass to ensure everything got a go and the structure
    ;;hasn't changed

    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))

    (setf *compiler-final-pass* t)
      (pass)))
      
;;A goblin appears at the door. He flings some inedible slop through the bars.

;;(dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/Downloads/goblincloset.bmp" :right
;;      "You are in the well-appointed closet of the goblin guard Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a single chair (broken), a spare loin-cloth (tattered) and a hundred-year-old copy of Modern Necromancer magazine.")

;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'