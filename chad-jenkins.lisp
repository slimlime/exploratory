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
;; There is no response to the user navigation- probably should be

(defparameter *game-dictionary* #())

(defparameter *snickering* "You hear the faint sound of snickering...")
(defparameter *thegodslookaway* "The gods look away in shame.")
(defparameter *far-out* "Far out!")
(defparameter *whatyoutalkingabout* "I am sure YOU know what you are talking about.")
(defparameter *cantdoit* "You can't do that!")

(defun chad-synonyms ()
  (defword :NORTH :N)
  (defword :SOUTH :S)
  (defword :EAST :E)
  (defword :WEST :W)
  (defword :LEFT :L)
  (defword :RIGHT :R)
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
      "You are in a passageway at the bottom of a dank flight of stairs. Wafting in from the right you hear the sound of polyhedral dice being rolled, goblins being slain and fizzy pop bottles being guzzled. Sounds like fun. To the left the mysterious passage disappears round a dark corner."
    (action '((LEFT) (SOUTH))
      (navigate :garage))
    (action '((RIGHT) (NORTH))
      (navigate :basement))
    (action '(UP)
      (label :go-up)
      (navigate :kitchen))
    (fixture "BASEMENT STAIRS" (:description "Just ordinary stairs in an ordinary damp basement." :name-override "Some stairs.")
      (verb 'CLIMB
	(goto :go-up))))

  (location :garage "GARAGE" "/home/dan/Downloads/porsche.bmp"
      "Red porsche. Paint can, etc."
    (action '(NORTH)
      (navigate :basement-stairs)))
  
  (location :basement "BASEMENT ANNEX" "/home/dan/Downloads/porsche.bmp"
      "Nerds' basement"
    (action '((SOUTH) (OUT))
      (navigate :basement-stairs)))

  (location :kitchen "KITCHEN" "/home/dan/Downloads/porsche.bmp"
      "Suburban kitchen"
    (action '(DOWN)
      (navigate :basement-stairs))
    (action '(WEST)
      (navigate :den))
    (action '(UP)
      (navigate :upstairs-landing)))
  
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
