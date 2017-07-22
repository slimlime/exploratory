;; A simple game to test all the bits work
;; 3 rooms
;; EXAMINE WALL
;; TAKE COIN
;; OPEN DOOR
;; NORTH
;; TAKE SPANNER
;; TALK GOBLIN
;; TALK GOBLIN
;; TALK GOBLIN
;; GIVE COIN GOBLIN or GIVE GOBLIN COIN
;; EXAMINE SCROLL
;; SAY FRANGIBLE
;; ENTER PORTAL
;; CAST SPELL
;; THROW SPANNER WIZARD

;; TODO make a file called 'declarations' or somesuch, with
;; defloc, defresponse etc

;; TODO dcs should intern strings, we should ensure that when we
;;      pass strings anywhere literally, they should be checked
;;      against existing strings.

(dloc :dungeon-cell "DUNGEON CELL" "/home/dan/Downloads/cellardoor.bmp" :right
      "You are in the dungeon prison of Wangband underneath the fortress of the Black Wizard, Beelzepops. Home to stench-rats, were-toads, sniveling goblins and you. Of the current denizens, you are currently the most wretched. Surrounding you is a slime that reminds you of lime pudding, if lime pudding had hundreds of eyes and made a rasping, wheezing sound as it oozed out of the cracks in the wall. You must escape.")

(with-location :dungeon-cell
  (action '(EXAMINE SLIME)
	  (respond "Millions of sad eyes gaze at you from slime.")
	  (nifbit :key-in-crack
		  (respond "They seem to be staring at a crack in the floor.")))
  
  (action '(EXAMINE WALL)
	  (respond "The wall oozes with unpleasant smelling slime."))
  
  (action '(EXAMINE FLOOR)
	  (respond "There is a crack in the floor.")
	  (ifbit :key-in-crack
		 (respond "Perhaps it bears further examination?")))
  
  (action '(EXAMINE CRACK)
	  (setbit :crack-examined)
	  (ifbit :key-in-crack
		 (respond "A glint of metal shines back at you from the crack."
			  "A key!")
		 (respond "A crack in the floor, just like any other crack"
			  "One might hide a small object here, perhaps a key?")))
  
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
		 (respond "The door is already locked."
			  "Have you been licking the slime? It's hallucinogenic.")))

  (action '(LICK SLIME)
	  (ifbit :slime-licked
		 (progn
		   (setbit :slime-licked)
		   (respond "Millions of colours flash in geometric patterns"
			    "before your eyes. Space and time warp as"
			    "Maia's cosmic tears rain down on you."
			    "You have a strange urge to grow a beard and play guitar."))
		 (respond "Nothing happens. Your third eye is already open.")))
		 
  (action '(OPEN DOOR)
	  (ifbit :door-open
		 (respond "The door is already open.")
		 (ifbit :door-locked
			(respond "The door is locked.")
			(progn
			  (respond "The door creaks open.")
			  (setbit :door-locked))))))))

(with-location :generic
  (action '(TAKE KEY)
	  (nifbit :key-in-crack
		  (respond "You already have the key!"
			   "Perhaps the dungeon air is getting to you."))))
  
  

;;A goblin appears at the door. He flings some inedible slop through the bars.

(dloc :frazbolgs-closet "FRAZBOLG'S CLOSET" "/home/dan/Downloads/goblincloset.bmp" :right
      "You are in the well-appointed closet of the goblin guard Frazbolg. Over centuries of guarding his prisoners he has amassed an impressive collection of posessions, a single chair (broken), a spare loin-cloth (tattered) and a hundred-year-old copy of Modern Necromancer magazine.")

;;You see a spanner

;;Frazbolg is engrossed in an article 'Entrails. Worth the mess?'
;;Frazbolg is engrossed in an article 'Platelet infusions. Here's why.'
;;Frazbolg is engrossed in an article 'Tealeaves. Primitive superstition or refreshing alternative to intestinal tracts?'
;;Frazbolg is engrossed in an article 'Cardio for Graverobbing.'
;;Frazbolg is engrossed in an article 'Lady necromancers. Here to stay?'
;;Frazbolg is engrossed in an article 'One weird trick for a bloated corpse.'
;;Frazbolg says 'I am stuck here until I can afford Necromancer school.'



(dloc :wizards-tower "WIZARD'S TOWER" "/home/dan/Downloads/wizardstower.bmp" :right
      "You are in the tower of the Black Wizard, Beelzepops. 
