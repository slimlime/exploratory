;; A simple game to test all the bits work

;; No assembly in here...
;; ..(label has leaked through...)

;; SMILE AT SLIME
;; catchphrases
;;TODO CHECK THAT PLACES HAVE BOTH A SET-USE AND AN IF-USE
;;Additionally, we need ot be able to reset the game state to the beginning
;;'(:door-locked t :lock-jammed f) etc
;;todo, sign state so that it only works with compatible version

;;TODO Verbs where the condition is that the object is actually in
;;     the inventory, rather than just present, e.g. verb 'x (if-has this xx)

;;TODO Don't want to have VM- instructions in this definition

;;TODO Double dispatch still a bit clumsy

;;if-has OBJ then XXX else respond *donothave* seems very common

;;TODO Have a rebuild, which recompiles and resets all the game state back to what it was
;;TODO 

(defparameter *game-dictionary*
  #("the" "You " " you" "The " "ing" "door" " biscuit" " is "
    "ck" " of" "in" "slime" " that" "green " ", " "key" "You"
              "Ther" " and" "ll" "already " "have" "close" " floor"))

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")
(defparameter *cantdoit* "You can't do that!")

(defparameter *exit-words* (make-hash-table :test 'equal))

(defun synonyms ()
  (defexit-word :EXIT :OUT :GO)
  (defexit-word :N :NORTH)
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH :BANG)
  (defword :TURN)
  (defword :EXAMINE :DESCRIBE)
  (defword :LOCK :KEYHOLE))

(defun dungeon-cell ()
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/exploratory/images/cell.bmp"
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Gordon. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape... Perhaps you could try the door?")
  (with-location :dungeon-cell
    (defbits t :door-locked)
    (defbits t :lock-jammed)
    
    (object "SHINY KEY" (:description "It's a key, man."
				      :place :nowhere)
      (action '(TAKE KEY)
	(format t "~a~%" *action-context*)
	(if-in-place "SHINY KEY" :nowhere
	  (respond *whatyoutalkingabout*)
	  (delegate-action)))
      (verb 'EAT (respond "You eat the key. Much, much later, it re-emerges."))
      (verb 'TURN (respond "You turn the key. Nothing happens.")))
    
    (object "INEDIBLE SLOP"
	(:description "A balanced soup of entrails, small amphibian parts and mandibles. Ooh! Garlic croutons!"
		      :place :nowhere
		      :name-override "Some inedible slop.")
      (verb 'EAT
	(move-object this :nowhere)
	(respond *thegodslookaway*)))

    (label :unblock-keyhole)
    (move-object "FINGER BONE" :nowhere)
    (respond *far-out*)
    (clrbit :lock-jammed)
    (respond "The bony finger pokes out the blockage in the lock and crumbles to dust, having fulfilled its destiny.")
    (vm-done)
    
    (object '("FINGER BONE" "BONE FINGER")
	(:description "The long, slender digit of a long since departed previous occupant of your cell. Human? YOU decide.")
      (verb 'EAT
	(respond *thegodslookaway*))
      (verb 'POKE
	(ensure-has this)
	(if-object "KEY HOLE"
		   (goto :unblock-keyhole)
		   (respond "The finger seems to say, 'Not there! You fool!'"))))
    (fixture "REPELLENT SLIME" ()
      (verb 'EXAMINE
	(respond "Millions of eyes peer out from the slime.")
	(setbit :slime-examined)
	(if-in-place "SHINY KEY" :nowhere
	  (respond "Some of them are staring at the floor.")))
      
      (verb 'LICK
	(if-not-bit :slime-licked
	    (progn
	      (setbit :slime-licked)
	      (respond *far-out*)
	      (respond "Myriad colours break in waves upon your ears. Maya's cosmic tears rain down on you in a shower of gold. The slime smiles."))
	  (respond "Nothing happens. Your third eye is already open."
		   "But... you do feel a strange urge to grow a beard and play the guitar.")))
      (verb 'ATTACK (respond "Your hand is stayed by the slime's gaze of infinite sadness.")))
    
    (fixture "SLIME EYES" ()
      
      ;;this could be done by moving the object from nowhere to here
      ;;but it would need a separate handler for the "slime not examined" case
      
      (verb 'EXAMINE
	(if-bit :slime-examined
		(respond "The eyes are deep and sorrowful. Eyes that have seen things best left unseen.")
		(respond "Funny you should mention eyes. You do have a strange feeling that someone, or something is staring at you."))))
    
    (fixture "CELL WALL" ()
      (verb 'EXAMINE
	(respond "The wall oozes with a repellent green slime.
Eugh! The slime is looking at you!")
	(set-it "REPELLENT SLIME")))
    
    (fixture "CELL FLOOR" ()
      
      (verb 'EXAMINE
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
	      (setbit :sought-gorilla))))))
    
    (fixture "FLOOR BRICK" ()
      
      (verb 'EXAMINE
	(respond "Which one? There's gorillians of them!")
	(if-bit :sought-gorilla
		(respond "Sorry, that was an insensitive pun. I know you miss that guy."))))
    
    (fixture "FLOOR CRACK" ()
      
      (verb 'TAKE (respond "Inadvisable."))
      
      (verb 'LICK (respond *thegodslookaway*))
      
      (verb 'EXAMINE
	(if-bit :floor-examined
		(if-bit :slime-licked
			(with-object "SHINY KEY"
			  (if-in-place this :nowhere
			    (progn
			      (respond "A glint of metal shines back at you..."
				       "a key!")
			      (move-object this here)
			      (set-it this))
			    (respond "A crack in the floor, just like any other."
				     "One might hide a small key-like object here."
				     "Like, for example, a key.")))
			(respond "The Veil of Maya prevents you from seeing anything interesting."))
		(respond *whatyoutalkingabout*))))
    
    (fixture '("KEY HOLE" "KEYHOLE") ()
      ;;double dispatch definitely needed...
      (verb 'EXAMINE
	(respond "A plain old rusty keyhole.")
	(if-bit :lock-jammed
		(respond "It appears to be blocked.")))
      (verb '(UNBLOCK POKE)
	(if-object "FINGER BONE"
		   (progn
		     (ensure-has "FINGER BONE")
		     (goto :unblock-keyhole))
		   (respond "Great idea. But with what?"))))
    
    (fixture "GORILLA" ()
      (verb 'EXAMINE
	(when-bit :gorilla-seen
	  (respond "You search frantically around your cell, but cannot find the gorilla. Will you ever see that noble visage again?")
	  (setbit :sought-gorilla))))

    #|(progn
      (respond "Garrgh! You put your own finger in a rusty keyhole. Better get it checked out.")
      (respond *snickering*)
      (vm-done)))))|#
    
    (fixture "CELL DOOR" ()
      (verb 'UNLOCK
	(if-object "FINGER BONE"
		   (progn
		     (ensure-has "FINGER BONE")
		     (goto :unblock-keyhole))
		   (if-bit :door-locked
			   (if-has "SHINY KEY"
				   (if-not-bit :lock-jammed
				       (progn
					 (clrbit :door-locked)
					 (respond "The lock mechanism clicks..."))
				     (respond "You rattle the key in the lock, but there is something stuck in the other side."))
				   (respond "With what? Your finger?"))
			   (respond "The door is already unlocked."))))
      
      (verb 'BANG
	(label :bang-door)
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
      
      (verb 'EXAMINE
	(respond "The door is solid wood with a tiny barred window and a keyhole.") 
	(if-bit :door-open
		(respond "The door is open.")
		(respond "The door is closed.")))
      
      (verb 'CLOSE
	(if-bit :door-open
		(progn
		  (clrbit :door-open)
		  (respond "The door closes.")
		  (respond *thegodslookaway*))
		(respond "The door is already closed.")))
      
      (verb 'LOCK
	(if-bit :door-locked
		(respond "The door is already locked.")
		(progn
		  (setbit :door-locked)
		  (clrbit :door-open)
		  (respond "The lock mechanism clicks shut. You really have got it in for yourself haven't you?"))))
      
      (verb 'USE
	(label :exit)
	(if-bit :door-open
		(navigate :corridor "You go through the door.")
		(progn
		  (respond "Ouch! You walk into the closed door.")
		  (respond *snickering*))))
      
      (verb 'OPEN
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
			  (setbit :door-open))))))
    
    (fixture "CELL WINDOW"
	(:description "A small barred window, just big enough for the face of a jailer to sneer at you through.")
      (verb 'BANG
	(goto :bang-door)))
    
    (action '(EXIT)
      (goto :exit))
    
    (action '((EXAMINE VEIL) (EXAMINE MAYA))
      (respond "Many believe Maya casts her net of illusion over the world
preventing closed-minded mortals from seeing what is really there."))))

(defun corridor ()
  (dloc :corridor "CORRIDOR" "/home/dan/exploratory/images/corridor.bmp"
	"A torch-lit corridor. You see a row of cell doors, identical to your own. Moans and pleas waft through the bars- sounds which you are not entirely certain are human in origin. At one end, a brick wall, at the other a green door, different than all the rest.")
  
  (with-location :corridor

    (object "RED BALL" (:description "Just an unimportant red ball."))
    (object "GREEN BALL" (:description "Just an unimportant green ball."))
    
    (object "IMAGINARY BISCUIT"
	(:description "A biscuit that exists only in your head."
		      :place :nowhere)
      (verb 'EAT
	(respond "Mmmm! Satisfying.")
	(move-object this :nowhere)))

    (fixture "FLICKERING TORCHES" (:description "A row of flickering torches.")
      (verb 'EXAMINE
	(if-bit :torches-examined
		(delegate-action))
		(progn
		  (setbit :torches-examined)
		  (respond "The flickering shadows make you think of something profound, like a packet of Cheezows caught in the wind."))))
      (verb 'TAKE
	(respond "Don't be greedy."))
 
    ;; this is for an object which is hidden from look initially, but we can just
    ;; take it as its presence is implied by the row of torches.

    (object "FEEBLE TORCH" (:description "The torch gives out a feeble light." :place :nowhere)
      (action '(TAKE TORCH)
	(if-in-place this :nowhere
	  (progn
	    (respond "You take one of the torches from the wall.")
	    (move-object this :inventory))
	  (delegate-action))))

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
	(when-bit :metaphor-examined
	  (if-in-place this :nowhere
	    (progn
	      (respond "Fine. You take the imaginary biscuit. Happy now?")
	      (move-object this :inventory))
	    (respond "Ha! You lost your imaginary biscuit.")))))
    
    (action '(ENTER CELL)
      (respond "You enter your cell."))
    
    (action '(ENTER GREEN DOOR)
      (if-bit :green-door-open
	      (navigate :frazbolgs-closet "You go through the green door.")
	      (respond "The green door is closed.")))
    
    (action '(ENTER CELL DOOR)
      (navigate :dungeon-cell "You go through the cell door."))
    
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
      (navigate :corridor "You leave Frazbolg's Closet."))))

(defun generic-handlers () 
  ;;install the handlers common to all locations
  
  ;;install the handlers common to all games
  (generic-generic-handlers))

(defun run-simple-game (&key (break-on 'BRK) (print t))
  (run-game :dungeon-cell
	    #'(lambda ()
		(synonyms)
		(dungeon-cell)
		(corridor)
		(frazbolgs-closet)
		(generic-handlers))
	    *game-dictionary*
	    :print print
	    :break-on break-on))

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


