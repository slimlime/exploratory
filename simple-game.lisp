;; A simple game to test all the bits work

;; No assembly in here...

;; ...TODO separate out the build process

;; catchphrases

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")

;;todo what happens e.g. EXAMINE DOOR WINDOW
;;TODO CHECK THAT ALL STATEBITS HAVE BOTH A READ AND A WRITE
;;TODO CHECK THAT PLACES HAVE BOTH A SET-USE AND AN IF-USE

(defun synonyms ()
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH :BANG)
  (defword :TURN))

(defun dungeon-cell ()
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/exploratory/images/cell.bmp"
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Gordon. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape, but your cell has a door...")
  (with-location :dungeon-cell

    (defbits t :door-locked)
    (defbits t :lock-jammed)

    (defobject "SHINY KEY" "It's a key, man." :initial-place :nowhere)

    (with-object "SHINY KEY"
      (verb 'EAT (respond "You eat the key. Much, much later, it re-emerges."))
      (verb 'TURN (respond "You turn the key. Nothing happens")))
    
    (defobject "INEDIBLE SLOP" "A balanced soup of entrails, small amphibians and mandibles. Ooh! Garlic croutons!" :name-override "Some inedible slop." :initial-place :nowhere)

    ;;(with-object "INEDIBLE SLOP"
    ;;  (verb 'EAT (respond *thegodslookaway*)))

    ;;(defobject "FINGER BONE" "The digit of a long since departed previous occupant of your cell. Human? YOU decide.")
        
    (action '(EXAMINE SLIME)
      (setbit :slime-examined)
      (respond "Millions of eyes peer out from the slime.")
      (if-in-place "SHINY KEY" :nowhere
		   (respond "Some of them staring at the floor.")))
    
    (action '(EXAMINE EYES)
      (if-bit :slime-examined
	      (respond "The eyes are deep and sorrowful. Eyes that have seen things best left unseen.")
	      (respond "Funny you should mention eyes, you have a strange feeling someone or something is staring at you.")))

    (action '(EXAMINE WALL)
      (respond "The wall oozes with a repellant green slime."
	       "Eugh! The slime is looking at you!"))
    
    (action '(EXAMINE FLOOR)
      (if-in-place "SHINY KEY" :nowhere
		   (progn
		     (respond "There is a crack in the floor. Perhaps it bears further examination?")
		     (setbit :floor-examined))
		   (if-not-bit :gorilla-seen
			       (progn
				 (respond "The sad face of a gorilla peers out from the random pattern of a floor-brick.")
				 (setbit :gorilla-seen))
			       (progn
				 (respond "Apart from the crack, there is nothing there.")
				 (setbit :sought-gorilla)))))
    
    (action '(EXAMINE BRICK)
      (respond "Which one? There's gorillians of them!")
      (if-bit :sought-gorilla (respond "Sorry, that was an insensitive pun. I know you miss that guy.")))
    
    (action '(EXAMINE GORILLA)
      (when-bit :gorilla-seen
	(respond "You search frantically around your cell, but cannot find the gorilla. Will you ever see that noble visage again?")
	(setbit :sought-gorilla)))

    ;; When the game is finished, if :gorilla-seen, his face will reappear for a bonus

    (action '(EXAMINE CRACK)
      (if-bit :floor-examined
	      (if-bit :slime-licked
		      (if-in-place "SHINY KEY" :nowhere
				   (progn
				     (respond "A glint of metal shines back at you..."
					      "A key!")
				     (move-object "SHINY KEY" *current-location*))
				   (respond "A crack in the floor, just like any other."
					    "One might hide a small key-like object here."
					    "Like, for example, a key."))
		      (progn
			(respond "The Veil of Maya prevents you from seeing anything interesting.")
			(setbit :maya)))
	      (respond *whatyoutalkingabout*)))
      
    (action '((EXAMINE VEIL) (EXAMINE MAYA))
      (if-bit :maya
	      (respond "Many believe Maya casts her net of illusion over the world
preventing closed-minded mortals from seeing what is really there.")
	      (respond *whatyoutalkingabout*)))

    (action '(TAKE KEY)
      (if-in-place "SHINY KEY" :nowhere
		   (respond "What key? Do you know something I don't?")
		   (delegate-action)))
    
    (action '(EXAMINE DOOR)
      (respond "The door is solid wood with a tiny barred window and a keyhole.") 
      (if-bit :door-open
	      (respond "The door is open.")
	      (respond "The door is closed.")))
    
    (action '((EXAMINE WINDOW) (BANG DOOR) (BANG WINDOW))
      (if-not-bit :door-open
		  (progn
		    (respond "A goblin appears at the window.")
		    (if-in-place "INEDIBLE SLOP" :nowhere
				 (progn
				   (move-object "INEDIBLE SLOP" *current-location*)
				   (clrbit :lock-jammed)
				   (respond "He flings some inedible slop through the bars. You hear something rattling in the lock."))
				 (respond "He tells you to keep the noise down using a stream of vowel-free goblin profanities. KRRPP KRRPP FNRGL!")))))
    
    (action '(EXAMINE KEYHOLE) (respond "It's a keyhole, man."))

    (action '(TAKE CRACK) (respond "Inadvisable."))

    (action '(LICK CRACK) (respond *thegodslookaway*))

    (action '(ATTACK SLIME) (respond "Your hand is stayed by the slime's gaze of infinite sadness."))

    (action '(UNLOCK DOOR FINGER)
      (if-bit :door-locked
	      (progn
		(respond "Wise guy, eh? The lock doesn't budge. Your finger is now sore.")
		(respond *snickering*))
	      (respond "You put your finger in the keyhole of an unlocked door.")))
    
    (action '((UNLOCK DOOR) (USE KEY DOOR))
      (if-bit :door-locked
	      (if-in-place "SHINY KEY" :inventory
			   (if-not-bit :lock-jammed
				   (progn
				     (clrbit :door-locked)
				     (respond "The lock mechanism clicks..."))
				   (respond "You rattle the key in the lock, but there is something stuck in the other side."))
			   (respond "With what? Your finger?"))
	      (respond "The door is already unlocked.")))
    
    (action '(CLOSE DOOR)
      (if-bit :door-open
	      (progn
		(clrbit :door-open)
		(respond "The door closes.")
		(respond *thegodslookaway*))))

    (action '(LOCK DOOR)
      (if-bit :door-locked
	      (respond "The door is already locked.")
	      (progn
		(setbit :door-locked)
		(clrbit :door-open)
		(respond "The lock mechanism clicks shut. You really have got it in for yourself haven't you?"))))
    
    (action '((EXIT) (USE DOOR))
      (if-bit :door-open
	      (navigate :corridor)
	      (progn
		(respond "Ouch! You walk into the closed door.")
		(respond *snickering*))))

    (action '(LICK SLIME)
      (if-not-bit :slime-licked
		  (progn
		    (setbit :slime-licked)
		    (respond *far-out*)
		    (respond "Myriad colours break in waves upon your ears. Maya's cosmic tears rain down on you in a shower of gold. The slime smiles."))
		  (respond "Nothing happens. Your third eye is already open."
			   "But... you do feel a strange urge to grow a beard and play the guitar.")))

    (action '(OPEN DOOR)
      (if-bit :door-open
	      (respond "The door is already open.")
	      (if-bit :door-locked
		      (if-in-place "SHINY KEY" :nowhere
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
      (if-bit :torches-examined
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
      (if-bit :green-door-open
	     (navigate :frazbolgs-closet)
	     (respond "The green door is closed.")))
    (action '(ENTER CELL DOOR)
      (navigate :dungeon-cell))
    (action '(EXAMINE WALL)
      (if-bit :torch-carried
	     (progn
	       (respond *far-out*)
	       (respond "Etched on the wall is a diagram. A triangle sits inside a circle, surrounded by flames. A spell perhaps?"))
	     (respond "It is too dark at this end of the corridor to see anything.")))
    (action '(ENTER DOOR)
      (respond "You will need to be more specific."))
    (action '(EXAMINE GREEN DOOR)
      (respond "You hear mumbling and sighing from behind the door."))
    (action '(KNOCK GREEN DOOR)
      (if-bit :green-door-open
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
  (reset-vm)
  
  (flet ((pass ()

	   (reset-dispatcher)
	   	   
	   (zeropage)
	   (org origin)
	   (CLD)
	   (label :start)
	   (cls #x10)
	   (sta16.zp (cons :font :present) :font)
	   
	   (JSR :vm-enter)
	   (vm-nav :dungeon-cell)
	   (vm-done)
	   
	   (BRK)

	   (label :test-input)
	   (JSR :test-render-input)
	   (JSR :parse)
	   (JSR :dispatch)

	   (BRK "<-- The correct end point after input")

	   ;;game state
	   (label :game-code-start)
	   (synonyms)

	   (measure-size "Dungeon Cell"
	     (dungeon-cell))
	   (measure-size "Corridor"
	     (corridor))
	   (measure-size "Frazbolg's Closet"
	     (frazbolgs-closet))

	   (generic-handlers)
	   	   
	   (bit-table)

	   (label :game-code-end)

	   ;;inline functions we will need

	   (deref-w)
	   (print-message)
	   (memcpy)
	   (memset)
	   (typeset)
	   (fleuron)
	   (navigator)
	   (vm)
	   
	   ;;testing functions

	   (test-render-input)

	   (measure-size "Object Table"
	     (object-table))
	   (parser)

	   (measure-size "Dispatcher"
	     (dispatcher))
	   (measure-size "String Table"
	     (string-table))
	   
	   (image-decompressor)
	   (label :end)
	   ;font data is pretty boring so stick it here
	   (measure-size "Fonts"
	     (font-data))))
    
    (pass)
    (build-symbol-table)
    ;;TODO This is BORING
    (setf *word-table-built* t)
    ;;penultimate pass to ensure everything got a go and the structure
    ;;hasn't changed

    ;;these two passes to optimize dead vm branches
    (pass)
    (pass)
    
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
(defun escape-dungeon ()
  (mapc #'enter-input '("EXAMINE FLOOR" "LICK SLIME" "EXAMINE CRACK" "TAKE KEY" "BANG DOOR" "TAKE SLOP" "UNLOCK DOOR" "OPEN DOOR" "EXIT")))
