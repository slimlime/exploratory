;; A simple game to test all the bits work

;; No assembly in here...
;; ..(label has leaked through...)

;; ...TODO separate out the build process
;; ...EXAMINE FINGER doesn't work COULD MAKE IT FINGER-BONE AND SYNONYMISE WITH BONE
;; ...but EXAMINE BONE does
;; TODO EAT DOOR -> I don't see that.
;; TODO IT IT IT and Also, With What? etc
;; SMILE AT SLIME
;;TODO EXAMINE FLOOR FAILS TO SET THE BIT (OR THE CHECK FAILS)
;; catchphrases

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")
(defparameter *donothave* "You don't have that.")
(defparameter *cantdoit* "You can't do that!")

;;todo what happens e.g. EXAMINE DOOR WINDOW
;;TODO CHECK THAT ALL STATEBITS HAVE BOTH A READ AND A WRITE
;;TODO CHECK THAT PLACES HAVE BOTH A SET-USE AND AN IF-USE

(defun synonyms ()
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH :BANG)
  (defword :TURN)
  (defword :LOCK :KEYHOLE))

(defun dungeon-cell ()
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/exploratory/images/cell.bmp"
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Gordon. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape... Perhaps you could try the door?")
  (with-location :dungeon-cell

    (defbits t :door-locked)
    (defbits t :lock-jammed)

    (defobject "SHINY KEY" "It's a key, man." :nowhere nil 
      (verb 'EAT (respond "You eat the key. Much, much later, it re-emerges."))
      (verb 'TURN (respond "You turn the key. Nothing happens")))
    
    (defobject "INEDIBLE SLOP" "A balanced soup of entrails, small amphibian parts and mandibles. Ooh! Garlic croutons!" :nowhere "Some inedible slop."
      (verb 'EAT
	(move-object this :nowhere)
	(respond *thegodslookaway*)))

    (defobject "FINGER BONE" "The long, slender digit of a long since departed previous occupant of your cell. Human? YOU decide." nil nil
      (verb 'POKE
	(if-word 'KEYHOLE
		 (if-has this
			 (progn
			   (label :poke-finger)
			   (move-object this :nowhere)
			   (respond *far-out*)
			   (clrbit :lock-jammed)
			   (respond "The bony finger pokes out the blockage in the lock and crumbles to dust, having fulfilled its destiny."))
			 (respond "The finger seems to say, 'Not there! You fool!'"))
		 (respond *donothave*))))
    
    (action '(EXAMINE SLIME)
      (setbit :slime-examined)
      (respond "Millions of eyes peer out from the slime.")
      (if-in-place "SHINY KEY" :nowhere
		   (respond "Some of them are staring at the floor.")))
    
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
		      (with-object "SHINY KEY"
			(if-in-place this :nowhere
				     (progn
				       (respond "A glint of metal shines back at you..."
						"a key!")
				       (move-object this here))
				     (respond "A crack in the floor, just like any other."
					      "One might hide a small key-like object here."
					      "Like, for example, a key.")))
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
      (unless-bit :door-open
	(respond "A goblin appears at the window.")
	(with-object "INEDIBLE SLOP"
	  (if-in-place this :nowhere
		       (progn
			 (move-object this here)
			 (respond "He flings some inedible slop through the bars.")
			 (when-bit :lock-jammed
			   (respond "You hear something rattling in the lock.")))
		       (respond "He tells you to keep the noise down using a stream of vowel-free goblin profanities. KRRPP KRRPP FNRGL!")))))
    
    (action '(EXAMINE KEYHOLE)
      (respond "A plain old rusty keyhole.")
      (if-bit :lock-jammed
	      (respond "It appears to be blocked.")))
    
    ;;AT 5 bytes per entry this is pretty spicy. What would it cost in the verb table
    ;;3 bytes, but plus the cost of it being an object -the examin cost etc.
    ;;need to calculate cost benefit.
    (action '(TAKE CRACK) (respond "Inadvisable."))
    (action '(LICK CRACK) (respond *thegodslookaway*))

    (action '(ATTACK SLIME) (respond "Your hand is stayed by the slime's gaze of infinite sadness."))

    ;;We could do this with double dispatch properly
    (action '(UNLOCK DOOR BONE)
      (if-bit :door-locked
	      (if-has "FINGER BONE"
		      (goto :poke-finger)
		      (progn
			(respond "Garrgh! You put your own finger in a rusty keyhole. Better get it checked out.")
			(respond *snickering*)))
	      (respond "You put your finger in the keyhole of an unlocked door.")))
    
    (action '((UNLOCK DOOR) (USE KEY DOOR))
      (if-bit :door-locked
	      (if-has "SHINY KEY"
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
    
    (defobject "IMAGINARY BISCUIT" "A biscuit that exists only in your head." :nowhere nil
	(verb 'EAT
	  (respond "Mmmm! Satisfying.")
	  (move-object this :nowhere)))
    
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
	     (respond *whatyoutalkingabout*)))
    (action '(EXAMINE METAPHOR)
      (setbit :metaphor-examined)
      (respond "That really is taking the biscuit."))

    (with-object "IMAGINARY BISCUIT"
      (action '(EXAMINE BISCUIT)
	;;TODO has and here combined seems useful, since it is similar to
	;;find object.
	(when-has this (delegate-action))
	(when-in-place this here (delegate-action))
	(if-bit :metaphor-examined
		(respond "There is no biscuit, man!")
		(respond *whatyoutalkingabout*)))
    
      (action '(TAKE BISCUIT)
	(when-has this (delegate-action))
	(when-in-place this here (delegate-action))
	(if-in-place this :nowhere
		     (respond "Fine. You take the imaginary biscuit. Happy now?")
		     (respond "Ha! You lost your imaginary biscuit"))
	(move-object this here)))
    
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
      (respond "Which one?"))))

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

(defun run-simple-game (&key (full-reset nil) (break-on 'BRK))
  (run-game :dungeon-cell
	    #'(lambda ()
		(synonyms)
		(measure-size "Dungeon Cell"
		  (dungeon-cell))
		(measure-size "Corridor"
		  (corridor))
		(measure-size "Frazbolg's Closet"
		  (frazbolgs-closet))
		(generic-handlers))
	    :full-reset full-reset :break-on break-on))

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
