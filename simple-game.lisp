
; A simple game to test all the bits work

;; No assembly in here...

;; catchphrases

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")

;;todo what happens e.g. EXAMINE DOOR WINDOW
;;TODO CHECK THAT ALL STATEBITS HAVE BOTH A READ AND A WRITE

(defun synonyms ()
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH))

(defun dungeon-cell ()
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/exploratory/images/cell.bmp"
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape, but your cell has a door...")
  (with-location :dungeon-cell

    (defbits t :door-locked)

    (defobject "SHINY KEY" "It's a key, man." :initial-place :crack)
    ;;(defobject "GOBLIN SLOP" "A balanced soup of entrails, small amphibians and mandibles. Ooh! Garlic croutons!" :initial-place :elsewhere)

    (if-in-place "SHINY KEY" :crack
		 (respond "They seem to be staring at the floor."))
    
    (action '(EXAMINE SLIME)
      (setbit :slime-examined)
      (respond "Millions of eyes peer out from the slime.")
      (if-in-place "SHINY KEY" :crack
		 (respond "They seem to be staring at the floor.")))
    
    (action '(EXAMINE EYES)
      (ifbit :slime-examined
	     
	       (respond "The eyes are deep and sorrowful. Eyes that have seen things best left unseen.")
	       (respond "Your eyes are fine... though you are pretty sure someone, or something is looking at you.")))

    (action '(EXAMINE WALL)
      (respond "The wall oozes with a repellant green slime."
	       "Eugh! The slime is looking at you!"))
    
    (action '(EXAMINE FLOOR)
      (respond "There is a crack in the floor.")
      (if-in-place "SHINY KEY" :crack
		   (respond "Perhaps it bears further examination?")))

    (action '(EXAMINE CRACK)
      (ifbit :slime-licked
	     (if-in-place "SHINY KEY" :crack
			  (progn
			    (respond "A glint of metal shines back at you..."
				     "A key!")
			    (move-object "SHINY KEY" *current-location*))
			  (respond "A crack in the floor, just like any other."
				   "One might hide a small key-like object here."
				   "Like, for example, a key."))
	     (progn
	       (respond "The Veil of Maya prevents you from seeing anything interesting.")
	       (setbit :maya))))

    (action '((EXAMINE VEIL) (EXAMINE MAYA))
      (ifbit :maya
	     (respond "Many believe Maya casts her net of illusion over the world
preventing closed-minded mortals from seeing what is really there.")
	     (respond *whatyoutalkingabout*)))

    (action '(TAKE KEY)
      (if-in-place "SHINY KEY" :crack
		   (respond "What key? Do you know something I don't?")
		   (delegate-action)))
    
    (action '(EXAMINE DOOR)
      (respond "The door is solid wood with a tiny barred window and a keyhole.") 
      (ifbit :door-open
	     (respond "The door is open.")
	     (respond "The door is closed.")))
    
    (action '(EXAMINE WINDOW)
      (nifbit :door-open
	      (progn
		(respond "A goblin appears at the window.")
		(nifbit :slop-flung
			(progn
			  (setbit :slop-flung)
			  (respond "He flings some inedible slop through the bars. You hear a key rattling in the lock."))
			(respond "He tells you to keep the noise down using a stream of vowel-free goblin profanities. KRRPP KRRPP FNRGL!")))))
    
    (action '(EAT FOOD)
      (ifbit :slop-flung
	     (respond "I would hardly call the goblin's slop food.")))

    (action '(EAT SLOP)
      (ifbit :slop-flung
	     (respond *thegodslookaway*)))

    (action '(EXAMINE KEYHOLE) (respond "It's a keyhole, man."))

    (action '(TAKE CRACK) (respond "Inadvisable."))

    (action '(LICK CRACK) (respond *thegodslookaway*))

    (action '(ATTACK SLIME) (respond "Your hand is stayed by the slime's gaze of infinite sadness."))

    (action '(UNLOCK DOOR FINGER)
      (ifbit :door-locked
	     (progn
	       (respond "Wise guy, eh? The lock doesn't budge. Your finger is now sore.")
	       (respond *snickering*))
	     (respond "You put your finger in the keyhole of an unlocked door.")))
    
    (action '((UNLOCK DOOR) (USE KEY DOOR))
      (ifbit :door-locked
	     (if-in-place "SHINY KEY" :inventory
		    (ifbit :slop-flung
			   (progn
			     (clrbit :door-locked)
			     (respond "The lock mechanism clicks..."))
			   (respond "You rattle the key in the lock, but there is something stuck in the other side."))
		    (respond "With what? Your finger?"))
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
    
    (action '((EXIT) (USE DOOR))
      (ifbit :door-open
	     (navigate :corridor)
	     (progn
	       (respond "Ouch! You walk into the closed door.")
	       (respond *snickering*))))

    (action '(LICK SLIME)
      (nifbit :slime-licked
	      (progn
		(setbit :slime-licked)
		(respond *far-out*)
		(respond "Myriad colours break in waves upon your ears. Maya's cosmic tears rain down on you in a shower of gold. The slime smiles."))
	      (respond "Nothing happens. Your third eye is already open."
		       "But... you do feel a strange urge to grow a beard and play the guitar.")))

    (action '(OPEN DOOR)
      (ifbit :door-open
	     (respond "The door is already open.")
	     (ifbit :door-locked
		    (if-in-place "SHINY KEY" :crack
				 ;;give them a clue, if they haven't already
				 ;;found the key
				 (respond
			     "Have you been licking the slime? It's hallucinogenic."
			     "The door, not unusually for a dungeon, is locked.")
			    (respond "The door is locked."))
		    (progn
		      (respond *far-out*)
		      (respond "The door creaks open.")
		      (setbit :door-open)))))))

(defun corridor ()
  (dloc :corridor "CORRIDOR" "/home/dan/exploratory/images/corridor.bmp"
	"A torch-lit corridor. You see a row of cell doors, identical to your own. Moans and pleas waft through the bars- sounds which you are not entirely certain are human in origin. At one end, a brick wall, at the other a green door, different than all the rest.")
  (with-location :corridor
    (defbits nil :green-door-open :torch-carried :torches-examined)
    (action '((EXAMINE TORCHES) (EXAMINE TORCH))
      (progn
	(setbit :torches-examined)
	(respond "The flickering shadows make you think of something profound, like a packet of Cheezows caught in the wind.")))
    (action '(TAKE TORCH)
      (progn
	(setbit :torch-carried)
	(respond "You take one of the torches.")))
    (action '((CHEEZOWS) (PACKET))
      (ifbit :torches-examined
	     (respond "There are no Cheezows! It was a metaphor for your situation.")
	     (respond "What a strange thing to say.")))
    (action '(EXAMINE METAPHOR)
      (respond "That really is taking the biscuit."))
    (action '(EXAMINE BISCUIT)
      (respond "There is no biscuit, man!"))
    (action '(TAKE BISCUIT)
      (respond "Fine. You take the biscuit. Happy now?")
    (action '(ENTER CELL)
      (respond "You enter your cell."))
    (action '(ENTER GREEN DOOR)
      (ifbit :green-door-open
	     (navigate :frazbolgs-closet)
	     (respond "The green door is closed.")))
    (action '(ENTER CELL DOOR)
      (navigate :dungeon-cell))
    (action '(EXAMINE WALL)
      (ifbit :torch-carried
	     (progn
	       (respond *far-out*)
	       (respond "Etched on the wall is a diagram. A triangle sits inside a circle, surrounded by flames. A spell perhaps?"))
	     (respond "It is too dark at this end of the corridor to see anything.")))
    (action '(ENTER DOOR)
      (respond "You will need to be more specific."))
    (action '(EXAMINE GREEN DOOR)
      (respond "You hear mumbling and sighing from behind the door."))
    (action '(KNOCK GREEN DOOR)
      (ifbit :green-door-open
	     (respond "Politely, you knock on the already open green door, but there is no answer.")
	     (progn
	       (setbit :green-door-open)
	       (respond "You knock on the door and wait patiently.")
	       (respond "Presently, it swings open. There appears to be some sort of lodging beyond the threshold."))))
    (action '(KNOCK DOOR)
      (respond "Which one?")))))

(defun frazbolgs-closet ()
  (dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/exploratory/images/porsche.bmp"
	"You are in the well-appointed closet of the goblin Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a spare loin cloth- tattered, a toaster and a hundred-year-old copy of Modern Necromancer magazine.")

;;I need to summon the shade of Wazbolg, my predecessor.

  (with-location :frazbolgs-closet
    (action '(EXAMINE CHAIR)
      (respond "It's broken."))
    (action '(EXIT)
      (navigate :corridor))))

(defun generic-handlers ()
  ;;install the handlers common to all games
  (generic-generic-handlers)
  ;;then generic handlers for all the locations in this game
  (with-location :generic 
  (action '(? ? ?)
    (respond "I don't know what that means."))))

(defparameter origin #x600)
      
(defun build-game (&key (full-reset nil))

  (when full-reset
    (reset-image-cache))
  
  (reset-compiler)
  (reset-symbol-table)
  (reset-bits)
  (reset-parser)
  (reset-object-model)
  (reset-game-state-ranges)

  ;;this (atm) must be called so the widths are initialised
  ;;todo, make a font-init function which just does the widths.
  (font-data)
  (reset-compiler)
  
  (flet ((pass ()

	   (reset-dispatcher)
	   	   
	   (zeropage)	     
	   (org origin)
	   (CLD)

	   (label :start)
	   (cls #x10)
	   (sta16.zp (cons :font :present) :font)
	      
	   (navigate :dungeon-cell)

	   (label :test)
	   (label :test2)
	   (sta16.zp (cons :dispatcher :dungeon-cell)
		     :location-dispatch-table)

	   (BRK)

	   (label :test-input)
	   (JSR :test-render-input)
	   (JSR :parse)
	   (JSR :dispatch)

	   (BRK)

	   ;;game state

	   (synonyms)
	   (dungeon-cell)
	   (corridor)
	   (frazbolgs-closet)	   
	   (generic-handlers)
	   
	   (bit-table)
	   
	   ;;inline functions we will need

	   (deref-w)
	   (print-message)
	   (memcpy)
	   (memset)
	   (typeset)
	   (fleuron)
	   (navigator)

	   ;;testing functions

	   (test-render-input)
	   
	   (object-table)
	   (parser)
	   
	   (dispatcher)
	   (string-table)
	   
	   (image-decompressor)
	   (label :end)
	   ;font data is pretty boring so stick it here
	   (font-data)
	   ))
    
    (pass)
    (build-symbol-table)
    ;;TODO This is BORING
    (setf *word-table-built* t)
    ;;penultimate pass to ensure everything got a go and the structure
    ;;hasn't changed

    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))
    
      (setf *compiler-final-pass* t)
      (pass)

      (format t "Build size ~a~%" (- *compiler-ptr* origin))))

(defun run-game (&key (full-reset nil) (break-on 'BRK))
  (build-game :full-reset full-reset)
  (monitor-reset #x600)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer)))

;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'

;;You gaze into the toaster. The toaster gazes into you. Some toast pops out and hits you in the face.

;;use key- nothing
;;use key on door "You walk into the closed door"
;;OUCH! YOU WALK INTO THE CLOSED DOOR
