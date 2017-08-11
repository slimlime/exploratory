
; A simple game to test all the bits work

;; catchphrases

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")

;;todo what happens e.g. EXAMINE DOOR WINDOW

(defun synonyms ()
  (defword :EXAMINE :LOOK)
  (defword :TAKE :GET :PICK :GRAB)
  (defword :LICK :EAT :TASTE)
  (defword :SLIME :OOZE :PUDDING)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH))

(defun dungeon-cell ()
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
      (nifbit :door-open
	      (progn
		(respond "A goblin appears at the window.")
		(nifbit :slop-flung
			(progn
			  (setbit :slop-flung)
			  (respond "He flings some inedible slop through the bars."))
			(respond "He tells you to keep the noise down using a colourful stream of goblin profanities.")))))
    (action '(EXAMINE SLOP)
      (ifbit :slop-flung
	     (respond "A balanced soup of entrails, small amphibians and mandibles. Ooh! Garlic croutons!")))
    (action '(EAT FOOD)
      (ifbit :slop-flung
	     (respond "I would hardly call the goblin's slop food.")))
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
    (action '((EXIT) (USE DOOR))
      (ifbit :door-open
	     (progn
	       (respond *far-out*)
	       (respond "You go through the door..."))
	     (progn
	       (respond "You walk into the closed door. Ouch.")
	       (respond *snickering*))))
    (action '(LICK SLIME)
      (nifbit :slime-licked
	      (progn
		(setbit :slime-licked)
		(respond *far-out*)
		(respond "Myriad colours break in waves upon your ears. Maia's cosmic tears rain down on you in a shower of gold. The slime smiles."))
	      (respond "Nothing happens. Your third eye is already open."
		       "But... you do feel a strange urge to grow a beard and play the guitar.")))
    (action '(OPEN DOOR)
      (ifbit :door-open
	     (respond "The door is already open.")
	     (ifbit :door-locked
		    (ifbit :key-in-crack
			   (respond "The door is locked.")
			   (progn
			     (respond
			      "Have you been licking the slime? It's hallucinogenic."
			      "The door, not atypically for a dungeon, is locked.")))
		    (progn
		      (respond *far-out*)
		      (respond "The door creaks open.")
		      (setbit :door-open)))))))

(defun corridor ()
  (dloc :corridor "CORRIDOR" "/home/dan/Downloads/bedroom.bmp" :right
	"A torch-lit corridor. You see a row of cell doors, identical to your own. Moans and pleas waft through the bars- sounds which you are not entirely certain are human in origin. At one end, a brick wall, at the other a green door, different than all the rest.")
  (with-location :corridor
    (defbits nil :green-door-open :torch-carried :torches-examined)
    (action '((EXAMINE TORCHES) (EXAMINE TORCH))
      (progn
	(setbit :torches-examined)
	(respond "The flickering shadows make you think of something profound, like a orange crisp-packet caught in the wind.")))
    (action '(TAKE TORCH)
      (respond "You take one of the torches."))
    (action '((CRISP) (PACKET))
      (ifbit :torches-examined
	     (respond "There is no crisp packet! It was a metaphor for your situation.")
	     (respond "What a strange thing to say.")))
    (action '(EXAMINE METAPHOR)
      (respond "That is really taking the biscuit."))
    (action '(ENTER CELL)
      (respond "You enter your cell."))
    (action '(ENTER GREEN DOOR)
      (ifbit :green-door-open
	     (respond "You step through the green door.")
	     (respond "The green door is closed.")))
    (action '(ENTER CELL DOOR)
      (respond "You go back into your cell."))
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
	     (respond "Politely, you knock on the already open green door, but theres is no answer.")
	     (progn
	       (setbit :green-door-open)
	       (respond "You knock on the door and wait patiently.")
	       (respond "Presently, it swings open. There appears to be some sort of lodging beyond the threshold."))))
    (action '(KNOCK DOOR)
      (respond "Which one?"))))

(defun frazbolgs-closet ()
  (dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/Downloads/porsche.bmp" :right
	"You are in the well-appointed closet of the goblin Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a spare loin cloth- tattered, a toaster and a hundred-year-old copy of Modern Necromancer magazine.")

;;I need toto summon the shade of Wazbolg, my predecessor.

  (with-location :frazbolgs-closet
    (action '(EXAMINE CHAIR)
      (respond "It's broken."))
    (action '(EXIT)
      (respond "You leave Frazbolg's closet."))))

(defun generic-handlers ()
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

	   (JSR '(:corridor . :draw))

	   (label :test)
	   (label :test2)
	   (sta16.zp (cons :dispatcher :corridor)
		     :location-dispatch-table)

	   (BRK)

	   (label :test-input)
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

	   (print-message)
	   (memcpy)
	   (memset)
	   (typeset)
	   (fleuron)
	   
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

(defun run-game (&optional (break-on 'BRK))
  (build-game)
  (monitor-reset #x600)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer)))

;;A goblin appears at the door. He flings some inedible slop through the bars.
;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'

;;You gaze into the toaster. The toaster gazes into you.
