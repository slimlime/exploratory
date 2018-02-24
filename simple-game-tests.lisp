;;;;;;;;;;;;;
;;;TESTING;;;
;;;;;;;;;;;;;

(defun run-tests (&optional (warn-only t))
  (run-simple-game :print nil)
  (terpri)
  (macrolet ((test (description &body body)
		   `(progn
		      (monitor-reset :start)
		      (monitor-run :break-on 'BRK :print nil)
		      (handler-case
			  (progn
			    ,@body)
			(error (e)
			  (let ((msg (format nil "~a FAILED~%~t'~a'" ,description e)))
			    (if warn-only
			        (format t "~a~%" msg)
				(assert nil nil msg))))))))
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
    (test "Not poking out the blockage and not losing the bone"
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
	  (assert-clr :maya)
	  (test-input "EXAMINE CRACK")
	  (assert-clr :maya)
	  (test-input "EXAMINE FLOOR" "EXAMINE CRACK")
	  (assert-set :maya)
	  (test-input "LICK SLIME")
	  (assert-object-in "SHINY KEY" :nowhere)
	  (test-input "EXAMINE CRACK")
	  (assert-object-in "SHINY KEY" :dungeon-cell))
    (test "The window"
	  (assert-clr :door-open)
	  (assert-object-in "INEDIBLE SLOP" :nowhere)
	  (test-input "BANG WINDOW")
	  (assert-object-in "INEDIBLE SLOP" :dungeon-cell)
	  (assert-set :lock-jammed))
    (test "Can escape"
	  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
		      "TAKE KEY" "TAKE FINGER BONE" "POKE BONE IN LOCK"
		      "KNOCK DOOR" "UNLOCK DOOR" "OPEN DOOR" "USE DOOR")
	  (assert-location :corridor))))

(time
 (progn
   (run-tests)
   (terpri)))
