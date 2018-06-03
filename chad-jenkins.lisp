;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;; CHAD JENKINS AND THE QUEST FOR THE ORB OF CHEE-ZOW ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; Combine dloc with a body parameter which is wrapped in with-location
;; Sort out absolute paths in dloc for loading image
;; 

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

(defun act1 ()
  ;;NOTE could probably add &body parameter to dloc which drops into a
  ;;with-location.
  (dloc :basement-stairs "BASEMENT STAIRS" "/home/dan/Downloads/cellardoor.bmp"
	"Bottom of a flight of stairs.")

  (with-location :basement-stairs
    (action '(LEFT)
      (navigate :garage))
    (action '(RIGHT)
      (navigate :basement)))

  (dloc :garage "GARAGE" "/home/dan/Downloads/porsche.bmp"
	"Red porsche. Paint can, etc.")

  (with-location :garage
    (action '(NORTH)
      (navigate :basement-stairs)))

  (dloc :basement "BASEMENT ANNEX" "/home/dan/Downloads/porsche.bmp"
	"Nerds' basement")

  (with-location :basement
    (action '((SOUTH) (OUT))
      (navigate :basement-stairs)))

  (dloc :kitchen "KITCHEN" "/home/dan/Downloads/porsche.bmp"
	"Suburban kitchen")

  (with-location :kitchen
    (action '(DOWN)
      (navigate :basement-stairs))
    (action '(WEST)
      (navigate :den))
    (action '(UP)
      (navigate :upstairs-landing)))
  
  (dloc :den "DEN" "/home/dan/Downloads/porsche.bmp"
	"The parents den")
  
  (with-location :den
    (action '(EAST)
      (navigate :kitchen)))

  (dloc :upstairs-landing "UPSTAIRS LANDING" "/home/dan/Downloads/porsche.bmp"
	"Landing")

  (with-location :upstairs-landing
    (action '(DOWN)
      (navigate :kitchen))
    (action '(WEST)
      (navigate :bathroom))
    (action '(EAST)
      (navigate :maxines-bedroom)))
  
  (dloc :bathroom "BATHROOM" "/home/dan/Downloads/porsche.bmp"
	"Bathroom")

  (with-location :bathroom
    (action '(OUT)
      (navigate :upstairs-landing)))
  
  (dloc :maxines-bedroom "MAXINE'S BEDROOM" "/home/dan/Downloads/porsche.bmp"
	"Bedroom")

  (with-location :maxines-bedroom
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
