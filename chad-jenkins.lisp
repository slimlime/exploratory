;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; CHAD JENKINS THE ORB OF CHEE-ZOW ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; LOOK is broken in the basement stairs
;; The fixtures, dog and dog-bowl must show up in LOOK- at the moment dog does not even though show is true.
;; Sort out absolute paths in dloc for loading image
;; Esnure that correct game dictionary is used
;; There is a bug that means that space and newline are confused if
;; none of the strings wrap. This should not be a problem in practice
;; Handlers of the form (? X) are not valid
;; If you can get cereberus's collar, that would be impressive. It would
;; be nice to make objects fit inside other objects.
;; saton and satan handlers to generic, every room handlers

;;
;; Chad Jenkins and the Orb of Chee-Zow
;;
;; You wake, dazed and confused in the basement of a suburban house,
;; the last thing you remember is a disembodied voice calling "Find the Orb
;; of Chee-Zow... Woooo!"  "It is your destiny! Wooo!"
;; Clearly you have fallen and broken your head, smoked something dodgy or a
;; combination of the two. Who are these weird nerds playing Castles and
;; Cryptids in the basement? Why do they worship Saton? What is that mysterious
;; knocking and crying coming from upstairs? You don't care and have no desire
;; to find out- but leaving is proving trickier than you anticipated...
;;
;; An adventure in three acts.
;;
;; The first two acts will have the structure GCD, with a clue of foreshadowing
;; for the next.  So, something said by a character in the preceeding act will
;; be symbolically true in the next act. Each act an 'object' will be dropped
;; through a portal.

;; Final act will be 'failure' except
;; it won't be, you will wake up in the basement, the nerds ask if you have
;; brought the Chee-Zows and are coming back to the game, but it's daytime, Maxine is
;; in the Porsche and she is calling you.

;; ACT I    - The Present
;;
;; Clues    - Maxine says... The Nerds say...
;; Object   - Flush the dog collar/tiny key down the loo (have to make it so doing this
;;            allows you to proceed, without making it obvious, can't allow
;;            going to the next act unless this is done)
;; Goal     - "Find the Orb of Chee-Zow"
;;            Find out what the hell is going on and get out of the house
;; Conflict - Dog, Maxine, Garage Door
;; Disaster - The garage door slams on your head... sending you spiralling into..

;; ACT II   - The Past
;;
;; Clues    - 
;; Goal     - Rescue Princess Maxinette
;; Conflict - Lord Chadric and his russet steed blocks your path
;; Disaster - You tumble into the well of time.

;; ACT III  - The Future
;; Goal     - Defeat Galacto Imperator Chadrix, retrieve the Orb.
;;            the orb is actually a bowl of cheesy puffs.
;; Conflict - Maxi 3000 is held captive- the orb will be smashed if you
;;            attempt to rescue her.
;; Disaster - You resue Maxi 3000, but the orb is smashed...






(defparameter *game-dictionary* #("You " ". " " the" "Cereber" "alcove" "The" " fizz" "basement" "in" "ou" "th" " bottle" "don't " " of" "and "))
;;(create-dictionary 140 *string-table* 2 8 #())

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure you know what you are talking about.")
(defparameter *cantdoit* "You can't do that!")
(defparameter *indeed* "Indeed.")

(defun chad-synonyms ()
  (defword :NORTH :N)
  (defword :SOUTH :S)
  (defword :EAST :E)
  (defword :WEST :W)
  (defword :LEFT :L)
  (defword :RIGHT :R)
  (defword :DOWN :D)
  (defword :UP :U)
  (defword :TAKE :GET :PICK :GRAB)
  (defword :EXIT :OUT :GO)
  (defword :ATTACK :KILL :HIT :CLEAVE :PUNCH :BANG)
  (defword :EXAMINE :DESCRIBE)
  (defword :LOCK :KEYHOLE)
  (defword :DOG :DOGGY))

#| Chad starts on the basement stairs, the player is unaware of what he is doing
   or how he got there. He finds out from a group of roleplaying gamers that he
   is one of their number, and it is his quest to bring snacks. But who is the
   mysterious (and possibly beautiful) Maxine? Who does the red Porsche with
   one faulty light belong to?

   Puzzles

   i) The garage is dark, a bulb must be found and inserted into the socket
      or Chad may slip and fall into a pile of paint cans and other detritus.
      The bulb is available from the basement stairs. If the basement stairs
      are dark and the dog is high from the energy drink, it will fall down
      and break its neck.

   ii) The garage door opener is in the glovebox of the Porsche, but the Porsche
      is locked. The Porsche can be opened by pushing down the faulty light. The
      garage door opener will work only if it has batteries. Batteries can be
      found in the clock radio Maxine throws at your head, when you offer her
      the tissues.

    iii) The dog is guarding the stairs up to the upper floor. To take care
      of it, we must feed it Ka-Zow energy drink, the drink for the dog-tired.
      
    iv) Maxine has the clock radio, to get it from her, offer her some tissues
        from the bathroom, because she has been crying. She will hurl the radio
        at you in a rage.

|#

(defun act1 ()
  (location :basement-stairs "BASEMENT STAIRS" "/home/dan/Downloads/cellardoor.bmp"
      "You are in a grim passageway at the bottom of a dank, evil flight of stairs. Wafting through an alcove to the right you hear the sound of polyhedral dice being rolled, goblins being slain and fizzy pop being guzzled. The sound of fun. To the left a mysterious passage disappears round a dark corner, to who knows where?"
    (action '((LEFT) (SOUTH))
      (navigate :garage "You go round the dark corner."))
    (action '((RIGHT) (NORTH)) (label :go-right)
      (navigate :basement "You go through the alcove."))
    (action '(UP)
      (label :go-up)
      (navigate :kitchen "You go up the stairs."))
    (action '(SATAN)
      (respond "You recite Judas Priest lyrics backwards, but nothing happens."))
    (action '(SATON)
      (respond "Saton hears you. Satan, however, laughs and spreads his wings."))
    (fixture "BASEMENT STAIRS" (:description "Phew! They appear to be regular, non-evil basement stairs of the sort found in many suburban houses.")
      (verb 'CLIMB
	(goto :go-up)))
    (fixture "BASEMENT PASSAGEWAY" (:description "It's not mysterious at all! Seems to lead to a car garage to the left and through an alcove to the right to another room."))
    (fixture "BASEMENT ALCOVE" (:description "Above the alcove, someone has scratched a half-assed pentagram.")
      (verb 'ENTER
	(goto :go-right)))
    (fixture "ALCOVE PENTAGRAM" (:description "Underneath the badly drawn pentagram the word 'SATON' has been inscribed.")))

  (location :garage "GARAGE" "/home/dan/Downloads/porsche.bmp"
      "Red porsche. Paint can, etc."
    (action '(NORTH)
      (navigate :basement-stairs "You leave the garage by the North passage.")))
  
  (location :basement "BASEMENT ANNEX" "/home/dan/Downloads/porsche.bmp"
      "Nerds' basement"
    (action '((SOUTH) (OUT))
      (navigate :basement-stairs "You leave the nerds to their game."))

    (object '("KAZOW BOTTLE" "ENERGY DRINK" "FIZZY POP" "BOTTLE KAZOW")
	(:description "A bottle of 'KaZow!' brand fizzy energy drink. 'Dog-tired? Drink KaZow.'"
		      :name-override "A bottle of fizzy pop."
		      :place :nowhere)
      (verb '(OPEN SHAKE)
	(ensure-has this)
	(if-bit :bottle-empty
		(respond "The bottle is empty.")
		(respond "The contents fizz like boiling acid. Best keep it closed, eh?")))
      (verb 'DRINK
	(ensure-has this)
	(if-bit :bottle-empty
		(respond "The bottle is empty.")
		(respond "You read the label, 'Sick Orange Flavour' and think better of it.")))
      (verb '(POUR EMPTY TIP)
	(ensure-has this)
	(if-bit :bottle-empty
		(respond "It's empty.")
		(if-object "METAL BOWL"
			   (progn
			     (respond "The dog drinks the liquid. It runs around in a furious circle then disappears into the den. You hear barking... then screaming...")
			     (setbit :bottle-empty)
			     (move-object "LITTLE DOG" :den))
		  (respond "You decide not to waste it.")))))

    (action '(SATON)
      (respond "The gamers stand-up and make a chilling hand gesture. They throw back their hoods, rotate three times and shout 'Hail Saton!'")
      (unless-bit :saton-hailed
	(progn
	  (respond "Someone tosses you a bottle of fizzy pop.")
	  (setbit :saton-hailed)
	  (move-object "KAZOW BOTTLE" :inventory))
	(respond "They sit back down and continue their game as if nothing unusual had happened."))))

  (location :kitchen "KITCHEN" "/home/dan/Downloads/porsche.bmp"
      "Suburban kitchen"
    (on-entry
      (when-in-place "LITTLE DOG" here
	(respond "You see a little dog scampering around on the linoleum!")))
    (action '(DOWN)
      (navigate :basement-stairs "You go down the stairs."))
    (action '(WEST)
      )
    (action '(UP)
      (if-in-place "LITTLE DOG" here
	(respond "Cereberus blocks your path. It is difficult to say whether he wants to play, or to rip you to shreds. The stairs are off-limits...")
	(navigate :upstairs-landing "You go up the stairs.")))
    (object "METAL BOWL"
	(:description "A metal dog-bowl.")
      (verb 'TAKE
	(if-in-place "LITTLE DOG" here
	  (respond "Cereberus growls. You put the bowl down, worried you might be dragged off to Hades in his tiny jaws.")
	  (respond "You can't pick it up, it is chained to the sink."))))
    (object '("LITTLE DOG" "LITTLE CEREBERUS") (:description "A sweet, small dog with eyes as big as saucers. His collar reads, 'Little Cereberus'. Thankfully he has only one head." :take nil)
      (verb '(TAKE PET STROKE)
	(respond "The dog leaps into your arms, licks you thoroughly in the face area, then continues to patrol the perimeter."))
      (verb 'ATTACK
	(respond "No.")))
    (fixture "PARENTS DEN" ()
	(verb 'EXAMINE
	  (respond "There is a tell-tale glow and drone coming from the den. Someone's parents are watching television.")
	  (if-in-place "LITTLE DOG" :den
	    (respond "Cereberus is in the den.")))
      (verb 'ENTER
	(respond "Cereberus growls. He is very protective of his den. He is a very good boy."))))
  
  (location :upstairs-landing "UPSTAIRS LANDING" "/home/dan/Downloads/porsche.bmp"
      "Landing"
    (action '(DOWN)
      (navigate :kitchen "You go down the stairs."))
    (action '(WEST)
      (navigate :bathroom "You go into the bathroom."))
    (action '(EAST)
      (navigate :maxines-bedroom "You go into Maxine's bedroom.")))
  
  (location :bathroom "BATHROOM" "/home/dan/Downloads/porsche.bmp"
      "Bathroom"
    (action '(OUT)
      (navigate :upstairs-landing "You go back onto the landing.")))
  
  (location :maxines-bedroom "MAXINE'S BEDROOM" "/home/dan/Downloads/porsche.bmp"
      "Bedroom"
    (action '((OUT) (WEST))
      (navigate :upstairs-landing "You go back onto the landing."))))

;;act ii ideas
;;LONELY PATH
;;WOODED GLADE
;;THE SPATCHCOCKED RAT
;;CASTLE DRAWBRIDGE
;;CHADRIC'S KEEP
;;CASTLE GARDEN
;;PRINCESS MAXINETTE'S TOWER (which naturally thrusts into the sky)

(defun run-chad-jenkins (&key (break-on 'BRK) (print t))
  (run-game :basement-stairs
	    #'(lambda ()
		(chad-synonyms)
		(act1)
		(generic-handlers))
	    *game-dictionary*
	    :print print
	    :break-on break-on))
