;;;;;;;;;;;;;
;;;TESTING;;;
;;;;;;;;;;;;;

(defparameter *warn-only* t)

(defmacro test (description &body body)
  `(progn
     (monitor-reset :start)
     (monitor-run :break-on 'BRK :print nil)
     (handler-case
	 (progn
	   ,@body)
       (error (e)
	 (let ((msg (format nil "~a FAILED~%~t'~a'" ,description e)))
	   (if *warn-only*
	       (format t "~a~%" msg)
	       (assert nil nil msg)))))))

(defun run-tests ()
  (run-simple-game :print nil)
  (terpri)
  
    (test "Examining slime"
      (assert-clr :slime-examined)
      (test-input "EXAMINE SLIME")
      (assert-set :slime-examined))
    
    (test "Can't take the slime"
      (assert-object-in "REPELLENT SLIME" :dungeon-cell)
      (test-input "TAKE SLIME")
      (assert-object-in "REPELLENT SLIME" :dungeon-cell))
    
    (test "Poking out the blockage requires bone"
      (assert-set :lock-jammed)
      (test-input "POKE BONE IN LOCK")
      (assert-set :lock-jammed))

    (test "Examining bone sets 'it'"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "EXAMINE BONE" "TAKE IT")
      (assert-object-in "FINGER BONE" :inventory))

    (test "Looking when unique object sets 'it'"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "LOOK" "TAKE IT")
      (assert-object-in "FINGER BONE" :inventory))

    (test "Looking when not unique object clears 'it'"
      (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK")
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (assert-object-in "SHINY KEY" :dungeon-cell)
      (test-input "LOOK")
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (assert-object-in "SHINY KEY" :dungeon-cell))

    (test "Examining something sets it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "LOOK" "EXAMINE SLIME" "TAKE IT")
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Unique inventory item sets it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE" "EXAMINE SLIME" "I" "DROP IT")
      (assert-object-in "FINGER BONE" :dungeon-cell))

    (test "Non-unique inventory item clears it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE"
		  "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
		  "TAKE KEY" "I" "DROP IT")
      (assert-object-in "SHINY KEY" :inventory)
      (assert-object-in "FINGER BONE" :inventory))
    
    (test "Taking and dropping the bone, qualified with adjective"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP FINGER BONE")
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Taking and dropping the bone, noun only"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE BONE")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP BONE")
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Taking and dropping the bone, as adjunct noun"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP FINGER")
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Poking out the blockage and losing the bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "POKE BONE IN LOCK")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
    
    (test "Poke keyhole with bone, with bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "POKE KEYHOLE WITH BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
    
    (test "Poke keyhole with bone, without bone"
      (assert-set :lock-jammed)
      (test-input "POKE KEYHOLE WITH BONE")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Unlock door with finger, with finger"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH FINGER")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
    
    (test "Unlock door with finger, withut finger"
      (assert-set :lock-jammed)
      (test-input "UNLOCK DOOR WITH FINGER")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Unlock door with bone, with bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
    
    (test "Unlock door with bone, without bone"
      (assert-set :lock-jammed)
      (test-input "UNLOCK DOOR WITH BONE")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "POKE BONE IN LOCK, without bone"
      (assert-set :lock-jammed)
      (test-input "POKE BONE IN LOCK")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))
    
    (test "Examining the floor"
      (assert-object-in "SHINY KEY" :nowhere)
      (assert-clr :floor-examined)
      (test-input "EXAMINE FLOOR")
      (assert-set :floor-examined)
      (assert-clr :gorilla-seen)
      (assert-clr :sought-gorilla))
    
    (test "Examining the crack"
      (test-input "EXAMINE CRACK")
      (assert-object-in "SHINY KEY" :nowhere)
      (test-input "EXAMINE FLOOR" "EXAMINE CRACK")
      (test-input "LICK SLIME")
      (assert-object-in "SHINY KEY" :nowhere)
      (test-input "EXAMINE CRACK")
      (assert-object-in "SHINY KEY" :dungeon-cell))
    
    (test "The window"
      (assert-clr :door-open)
      (assert-object-in "INEDIBLE SLOP" :nowhere)
      (test-input "BANG WINDOW")
      (assert-object-in "INEDIBLE SLOP" :dungeon-cell))
    
    (test "Can take key"
      (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK" "TAKE KEY")
      (assert-object-in "SHINY KEY" :inventory))
    
    (test "Can bang on door and get slop"
      (assert-object-in "INEDIBLE SLOP" :nowhere)
      (test-input "BANG DOOR")
      (assert-object-in "INEDIBLE SLOP" :dungeon-cell))
    
    (test "Eating the slop makes it go away"
      (test-input "BANG DOOR" "EAT SLOP")
      (assert-object-in "INEDIBLE SLOP" :nowhere))
    
    (test "Can escape"
      (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
		  "TAKE KEY" "TAKE FINGER BONE" "POKE BONE IN LOCK"
		  "KNOCK DOOR" "UNLOCK DOOR" "OPEN DOOR" "USE DOOR")
      (assert-location :corridor)))

(let ((*warn-only* nil))
  (time
   (progn
     (run-tests)
     (terpri))))
