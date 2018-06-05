;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;; CHAD JENKINS AND THE QUEST FOR THE ORB OF CHEE-ZOW ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; Combine dloc with a body parameter which is wrapped in with-location
;; Sort out absolute paths in dloc for loading image
;; Esnure that correct game dictionary is used
;; There is a bug that means that space and newline are confused if
;; none of the strings wrap. This should not be a problem in practice
;; BUG enter NORTH when NORTH is not a valid action gives the respons "I don't see that"
;; Handlers of the form (? X) are not valid
;; LOOK is broken in the basement stairs
;; The fixtures, dog and dog-bowl must show up in LOOK.

(defparameter *game-dictionary* #())

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")
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
  (defword :LOCK :KEYHOLE))


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
      "You are in a grim passageway at the bottom of a dank, evil flight of stairs. Wafting through an alcove to the right you can hear the sound of polyhedral dice being rolled, goblins being slain and fizzy pop being guzzled. The sound of fun. To the left the mysterious passage disappears round a dark corner, to who knows where?"
    (action '((LEFT) (SOUTH))
      (navigate :garage))
    (action '((RIGHT) (NORTH))
      (label :go-right)
      (navigate :basement "You go through the alcove."))
    (action '(UP)
      (label :go-up)
      (navigate :kitchen "You go up the stairs."))
    (action '(SATAN)
      (respond "You recite some Judas Priest backwards, but nothing happens."))
    (action '(SATON)
      (respond "Saton hears you."))
    (fixture "BASEMENT STAIRS" (:description "Phew! They appear to be regular, non-evil basement stairs of the sort found in many suburban houses.")
      (verb 'CLIMB
	(goto :go-up)))
    (fixture "BASEMENT PASSAGEWAY" (:description "It's not mysterious at all! Seems to lead to a car garage to the left and through an alcove to the right to another room."))
    (fixture "BASEMENT ALCOVE" (:description "Above the alcove, someone has scratched a half-assed pentagram.")
      (verb 'ENTER
	(goto :go-right)))
    (fixture "ALCOVE PENTAGRAM" (:description "Underneath the badly drawn pentagram the word `SATON' has been scratched.")))

  (location :garage "GARAGE" "/home/dan/Downloads/porsche.bmp"
      "Red porsche. Paint can, etc."
    (action '(NORTH)
      (navigate :basement-stairs)))
  
  (location :basement "BASEMENT ANNEX" "/home/dan/Downloads/porsche.bmp"
      "Nerds' basement"
    (action '((SOUTH) (OUT))
      (navigate :basement-stairs))

    (object '("ENERGY DRINK" "KAZOW BOTTLE")
	(:description "A bottle of KaZow! brand energy juice. `Dog-tired? Drink KaZow.'" :place :nowhere)
      (verb 'DRINK
	(if-bit :bottle-empty
		(respond "It's empty.")
		(respond "You read the label, `Sick Orange Flavour' and think better of it.")))
      (verb '(POUR EMPTY TIP)
	(ensure-has this)
	(if-bit :bottle-empty
		(respond "It's empty.")
		(if-object "DOG BOWL"
			   (progn
			     (respond "You pour the fizzing orange liquid into the dog-bowl.")
			     (respond "The dog laps it up. It runs around in a circle then disappears into the den. You hear barking and screaming...")
			     (setbit :bottle-empty)
			     (move-object "VICIOUS DOG" :den))
		  (respond "You decide not to waste it.")))))

    (action '(SATON)
      (respond "The gamers stand-up make a curious hand gesture, turn around three times, then throw back their hoods and shout `Hail Saton!'")
      (if-bit :saton-hailed
	      (respond "They sit down and resume their game.")
	      (progn
		(respond "One of the gamers tosses you a bottle of KaZow! energy drink.")
		(setbit :saton-hailed)
		(move-object "ENERGY DRINK" :inventory)))))

  (location :kitchen "KITCHEN" "/home/dan/Downloads/porsche.bmp"
      "Suburban kitchen"
    (action '(DOWN)
      (navigate :basement-stairs "You go down the stairs."))
    (action '(WEST)
      (navigate :den))
    (action '(UP)
      (navigate :upstairs-landing))
    (fixture "DOG BOWL" (:description "A metal dog bowl.")
      (verb 'TAKE (respond "It is chained to the sink.")))
    (fixture "VICIOUS DOG" (:description "A vicious dog, with eyes as big as plates and a heart as black as coal.")))
  
  (location :den "DEN" "/home/dan/Downloads/porsche.bmp"
      "The parents den"
    (action '(EAST)
      (navigate :kitchen)))
  
  (location :upstairs-landing "UPSTAIRS LANDING" "/home/dan/Downloads/porsche.bmp"
      "Landing"
    (action '(DOWN)
      (navigate :kitchen))
    (action '(WEST)
      (navigate :bathroom))
    (action '(EAST)
      (navigate :maxines-bedroom)))
  
  (location :bathroom "BATHROOM" "/home/dan/Downloads/porsche.bmp"
      "Bathroom"
    (action '(OUT)
      (navigate :upstairs-landing)))
  
  (location :maxines-bedroom "MAXINE'S BEDROOM" "/home/dan/Downloads/porsche.bmp"
      "Bedroom"
    (action '(OUT)
      (navigate :upstairs-landing))))

;;act ii ideas
;;LONELY PATH
;;WOODED GLADE
;;THE SPATCHCOCKED RAT
;;CASTLE DRAWBRIDGE
;;CHADRIC'S KEEP
;;CASTLE GARDEN
;;PRINCESS MAXINETTE'S TOWER

(defun run-chad-jenkins (&key (break-on 'BRK) (print t))
  (run-game :basement-stairs
	    #'(lambda ()
		(chad-synonyms)
		(act1)
		(generic-handlers))
	    *game-dictionary*
	    :print print
	    :break-on break-on))
