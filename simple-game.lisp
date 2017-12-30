
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
  (defword :SLIME :OOZE)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH))

(defun dungeon-cell ()
  (dloc :dungeon-cell "DUNGEON CELL" "/home/dan/exploratory/images/cell.bmp"
	"You are in the dungeon prison of Wangband under the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. A lime-green slime oozes out of the wall, making a rasping, wheezing sound. You must escape, but your cell has a door...")
  (with-location :dungeon-cell

    ;;todo verify object description, and refactor it with the response verification
    (defobject "BRONZE HOOK" "A sharp metal hook with a spherical handle.")
    (defobject "TEST OBJECT" "An object with no purpose in this life.")
    (defobject "SWORD" "A very important sword." :name-override "The sword of destiny")

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
	     (respond "The Veil of Maia prevents you from seeing anything interesting.")))
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
			  (respond "He flings some inedible slop through the bars. You hear a key rattling in the lock."))
			(respond "He tells you to keep the noise down using a stream of vowel-free goblin profanities. KRRPKCHK DRGKPK!")))))
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
	     (respond "You put your finger in the keyhole of an unlocked door.")))
    (action '((UNLOCK DOOR) (USE KEY DOOR))
      (ifbit :door-locked
	     (ifbit :key-in-crack
		    (respond "With what? Your finger?")
		    (ifbit :slop-flung
			   (progn
			     (clrbit :door-locked)
			     (respond "The lock mechanism clicks..."))
			   (respond "You rattle the key in the lock, but there is a key stuck in the other side.")))
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
		(respond "Myriad colours break in waves upon your ears. Maia's cosmic tears rain down on you in a shower of gold. The slime smiles."))
	      (respond "Nothing happens. Your third eye is already open."
		       "But... you do feel a strange urge to grow a beard and play the guitar.")))
    (action '(OPEN DOOR)
      (ifbit :door-open
	     (respond "The door is already open.")
	     (ifbit :door-locked
		    (nifbit :key-in-crack
			   (respond "The door is locked.")
			   (respond
			      "Have you been licking the slime? It's hallucinogenic."
			      "The door, not atypically for a dungeon, is locked."))
		    (progn
		      (respond *far-out*)
		      (respond "The door creaks open.")
		      (setbit :door-open)))))))

(defun test-exit-dungeon-cell ()
  (mapc #'enter-input '("EXAMINE WINDOW" "LICK SLIME" "EXAMINE CRACK" "TAKE KEY" "UNLOCK DOOR" "OPEN DOOR" "USE DOOR")))	

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
      (respond "That is really taking the biscuit."))
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
  (with-location :generic

    (db :object-count 0)

    (action '(INVENTORY) '(I)
      (respond "You have...")
      (LDA 1 "Inventory is Place 1")
      (label :scan-objects)
      (dc "Reset matching object count to 0")
      (LDX 0)
      (STX.AB :object-count)
      ;;shared entry point with LOOK
      (LDY (objects-count))
      (label :next-object)
      (dc "List the object in the place in A")
      (dc "Look in one-based object places table")
      (CMP.ABY (1- (resolve '(:object-table . :places))))
      (BNE :object-not-here)
      (dc "Save A and Y")
      (PHA)
      (TYA)
      (PHA)
      (dc "Now print the object name")
      ;;See below, this self-modifying code needs refactoring
      (LDA.ABY (1- (resolve '(:object-table . :name-hi))))
      (STA.AB :name-hi)
      (LDA.ABY (1- (resolve '(:object-table . :name-lo))))
      (STA.AB :name-lo)
      (JSR '(:print-message . 1))
      (DB :name-lo 0)
      (DB :name-hi 0)
      (dc "Restore Y and A")
      (PLA)
      (TAY)
      (PLA)
      (INC.AB :object-count)
      (label :object-not-here)
      (DEY)
      (BNE :next-object)
      (LDX.AB :object-count)
      (BNE :not-empty)
      (respond "Nothing.")
      (label :not-empty)
      (RTS))

    (action '(EXAMINE)

      ;;TODO. It would be nice if there was a way of specifying
      ;;      a pattern matcher that only took a single word
      ;;      i.e. the difference between EXAMINE and EXAMINE ? ?
            
      (dc "Load the second and third words into the name")
      (dc "and adjective positions")
      ;;THIS SECTION SHOULD BE REFACTORED, AS IT WILL BE
      ;;USED IN ALL ACTIONS THAT REQUIRE A GENERIC WORD
      (LDA.AB (+ 2 (resolve '(:parser . :words))))
      (BEQ :no-adjective)
      (STA.ZP '(:object-table . :name))
      (LDA.AB (+ 1 (resolve '(:parser . :words))))
      (STA.ZP '(:object-table . :adjective))
      (JMP :loaded-words)
      (label :no-adjective)
      (STA.ZP '(:object-table . :adjective))
      (LDA.AB (+ 1 (resolve '(:parser . :words))))
      (STA.ZP '(:object-table . :name))
      (BNE :loaded-words)
      (dc "No name or adjective was specified")
      (dc "this means just LOOK around")
      (respond "You take a look around and see...")
      (LDA.ZP :current-place)
      (JMP :scan-objects)
      (label :loaded-words)     
      (JSR :find-object-index)
      (BCS :duplicate-found)
      (BEQ :not-found)
      (dc "Now print the description")
      ;;The object description must be one line long
      ;;also the calling convention is poor. Should not need
      ;;self modifying code- TODO provide a run-time entry point
      ;;for the string
      ;;TODO support multi-line object descriptions.
      ;;Perhaps
      ;;could hook into the rendering routine to scroll the line
      ;;on each newline encountered OR, store the number of lines
      ;;in the object description, this will need an extra byte
      ;;or will need to scavenge some bits somewhere
      (LDA.ABY (1- (resolve '(:object-table . :description-hi))))
      (STA.AB :description-hi)
      (LDA.ABY (1- (resolve '(:object-table . :description-lo))))
      (STA.AB :description-lo)
      (JSR '(:print-message . 1))
      (DB :description-lo 0)
      (DB :description-hi 0)
      (RTS)
      (label :duplicate-found)
      (respond "Which one?")
      (RTS)
      (label :not-found)
      (respond "I don't know what that is."))
      
    (action '(TAKE KEY)
      (nifbit '(:dungeon-cell . :key-in-crack)
	      (respond "You already have the key!"
		       "Perhaps the dungeon air is getting to you.")))))

(defparameter origin #x600)

(defun test-render-input ()
  (label :test-render-input)
  (with-namespace :test-render-input
    ;;this simulates the user having already entered the text on the bottom
    ;;row and pressing return. The existing text in the 4 lines above is
    ;;scrolled up and the text is posted on line 4 (not five, which remains empty)
    (call-memcpy (scradd (live-row 1) 0)
		 (scradd (live-row 0) 0)
		 (* 4 +screen-width-bytes+ *line-height*))
    (call-memset 0 (scradd (live-row 3) 0)
		 (* 1 +screen-width-bytes+ *line-height*))
    (sta16.zp (scradd (live-row 3) 0) '(:typeset . :raster))
    (alias :tmp-raster :A3)
    (alias :pos :D4)
    (alias :str :A2)
    (label :typeset-is nil)
    (LDY 0)
    (STY.ZP '(:typeset . :shift))
    (STY.ZP '(:typeset . :prev-width))
    (sta16.zp '(:parser . :input) :str)
    (label :next)
    (STY.ZP :pos)
    (LDA.IZY :str)
    (BNE :not-space)
    (LDA (lo '(:present . #\ )))
    (STA.ZP (lo-add '(:typeset . :char)))
    (LDA (hi '(:present . #\ )))
    (STA.ZP (hi-add '(:typeset . :char)))
    (JMP :emit)
    (label :not-space)
    (TAX)
    (LDA.ABX (1- (resolve ':1ch-lo)))
    (CLC)
    (dc "Add the offset of the font")
    (ADC.ZP (lo-add :font))
    (STA.ZP (lo-add '(:typeset . :char)))	
    (LDA.ABX (1- (resolve ':1ch-hi)))
    (ADC.ZP (hi-add :font))
    (STA.ZP (hi-add '(:typeset . :char)))
    (label :emit)
    (JSR :typeset)
    (LDY.ZP :pos)
    (INY)
    (CPY *max-input-length*)
    (BNE :next)
    (RTS)

    ;; We shouldn't need this table really, once the compressed
    ;; string table contains all the letters.
    
    (let ((lo (list :1ch-lo))
	  (hi (list :1ch-hi)))
      (loop for c across "ABCDEFGHIJKLMNOPQRSTUVWXYZ" do
	   (let ((offset (- (resolve (cons :present c))
			    (resolve '(:font . :present)))))
	     (push (lo offset) lo)
	     (push (hi offset) hi)))
      (apply #'db (nreverse lo))
      (apply #'db (nreverse hi)))))
  
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
      
(defun build-game (&key (full-reset nil))

  (when full-reset
    (reset-image-cache))
  
  (reset-compiler)
  (reset-symbol-table)
  (reset-bits)
  (reset-parser)
  (reset-object-model)

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
