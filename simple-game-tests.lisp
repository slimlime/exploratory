;;;;;;;;;;;;;
;;;TESTING;;;
;;;;;;;;;;;;;

;; this compiles the game, each time a test is run it resets the monitor
;; by copying the compile buffer into the monitor buffer so each test is independent.

(defun init-game ()
  (monitor-reset :start)
  (monitor-run :break-on 'BRK :print nil))

(defmacro game-test (description &body body)
  `(test ,description
     (init-game)
     ,@body))

(defmacro deferred-game-test (description &body body)
  `(deferred-test ,description
     (init-game)
     ,@body))
     
(run-simple-game :print nil)

(game-test "Unknown exit message"
  (test-input "N")
  (assert-msg *cant-go-that-way*))

(game-test "Unknown exit synonym message"
  (test-input "NORTH")
  (assert-msg *cant-go-that-way*))

(game-test "Known exit, override message"
  (test-input "OUT")
  (assert-msg *snickering*))

(game-test "Unknow three word gives error"
  (test-input "BAA BAB BDE")
  (assert-msg *unknown-word*))

(game-test "Unknown two word gives error"
  (test-input "FLIBBLE WIBBLE")
  (assert-msg *unknown-word*))

(game-test "Unknown single word gives error"
  (test-input "RUN")
  (assert-msg *dont-understand*))

(game-test "Drop object we don't have"
  (test-input "DROP BONE")
  (assert-msg *do-not-have*))

(game-test "Do something weird to an object"
  (test-input "TAKE BONE" "KILL BONE")
  (assert-msg *cant-do-that*))

(game-test "Do something weird to it"
  ;;this was failing because IT was defined in the place nowhere
  ;;so wasn't being matched
  (test-input "TAKE BONE" "KILL IT")
  (assert-msg *cant-do-that*))

(deferred-game-test "IT is cleared after something without an object"
  (test-input "EXAMINE BONE" "EXIT" "TAKE IT")
  (assert-msg *be-more-specific*))

(game-test "Two objects dont make an IT"
  (test-input "POKE BONE IN KEYHOLE" "TAKE IT")
  (assert-msg *be-more-specific*))

(game-test "Do something weird to an object, with an override"
  (test-input "TAKE BONE" "EAT IT")
  (assert-msg *thegodslookaway*))

(game-test "Take object you already have"
  (test-input "TAKE BONE" "TAKE BONE")
  (assert-msg *already-have*))

(game-test "Take a fixture"
  (test-input "TAKE SLIME")
  (assert-msg *cant-take-that*))

(game-test "Take message works"
  (test-input "TAKE BONE")
  (assert-msg *you-took-it*))

(game-test "Dropped it message"
  (test-input "TAKE BONE" "DROP IT")
  (assert-msg *you-dropped-it*))

(game-test "I don't see that message"
  (test-input "TAKE BALL")
  (assert-msg *dont-see-that*))

(game-test "I don't see that with override"
  (test-input "TAKE KEY")
  (assert-msg *whatyoutalkingabout*))

(game-test "After an inventory with one item, does EXAMINE IT show correct message"
      (test-input "TAKE FINGER BONE" "I" "EXAMINE IT")
      (assert-object-in "FINGER BONE" :inventory)
      (assert-msg "The long, slender digit of a long since
departed previous occupant of your cell.
Human? YOU decide."))

(game-test "With more than one object in inventory, be more specific than IT"
  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
	      "TAKE KEY" "TAKE FINGER BONE" "I" "EXAMINE IT")
  (assert-object-in "SHINY KEY" :inventory)
  (assert-object-in "FINGER BONE" :inventory)
  (assert-msg *be-more-specific*))

(game-test "Examining slime"
      (assert-clr :slime-examined)
      (test-input "EXAMINE SLIME")
      (assert-set :slime-examined))

(game-test "Can't take the slime"
      (assert-object-in "REPELLENT SLIME" :dungeon-cell)
      (test-input "TAKE SLIME")
      (assert-object-in "REPELLENT SLIME" :dungeon-cell))

(game-test "Poking out the blockage requires bone"
      (assert-set :lock-jammed)
      (test-input "POKE BONE IN LOCK")
      (assert-set :lock-jammed))

(game-test "Examining bone sets 'it'"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "EXAMINE BONE" "TAKE IT")
      (assert-object-in "FINGER BONE" :inventory))

(game-test "Looking when unique object sets 'it'"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "LOOK" "TAKE IT")
      (assert-object-in "FINGER BONE" :inventory))

(game-test "Looking when not unique object clears 'it'"
      (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK")
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (assert-object-in "SHINY KEY" :dungeon-cell)
      (test-input "LOOK")
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (assert-object-in "SHINY KEY" :dungeon-cell)
      (test-input "WHAT")
      (assert-msg *it-is-nothing*))

(game-test "Examining something sets it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "LOOK" "EXAMINE SLIME" "TAKE IT")
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Examining the floor crack sets it to be the key"
  ;;this is to test (set-it this)
  ;;and it really blew up!
  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK" "WHAT")
  (assert-msg "A shiny key."))

(game-test "Examining the floor it"
  ;;was some corruption doing this- because the object it was
  ;;being set to the floor which doesn't have a what description
  (test-input "EXAMINE FLOOR" "WHAT")
  (assert-msg *something*))

(game-test "Examining the floor crack doesn't mess up it"
  (test-input "EXAMINE FLOOR" "EXAMINE CRACK" "WHAT")
  (assert-msg *something*))

(game-test "Unique inventory item sets it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE" "EXAMINE SLIME" "I" "DROP IT")
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Non-unique inventory item clears it"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE"
		  "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
		  "TAKE KEY" "I" "DROP IT")
      (assert-object-in "SHINY KEY" :inventory)
      (assert-object-in "FINGER BONE" :inventory))

(game-test "Taking and dropping the bone, qualified with adjective"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER BONE")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP FINGER BONE")
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Taking and dropping the bone, noun only"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE BONE")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP BONE")
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Taking and dropping the bone, as adjunct noun"
      (assert-object-in "FINGER BONE" :dungeon-cell)
      (test-input "TAKE FINGER")
      (assert-object-in "FINGER BONE" :inventory)
      (test-input "DROP FINGER")
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Poking out the blockage and losing the bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "POKE BONE IN LOCK")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))

(game-test "Poke keyhole with bone, with bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "POKE KEYHOLE WITH BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))

(game-test "Poke keyhole with bone, without bone"
      (assert-set :lock-jammed)
      (test-input "POKE KEYHOLE WITH BONE")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Unlock door with finger, with finger"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH FINGER")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))

(game-test "Unlock door with finger bone, with finger bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH FINGER BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
  
(game-test "Unlock door with finger, without finger"
      (assert-set :lock-jammed)
      (test-input "UNLOCK DOOR WITH FINGER")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Unlock door with bone, with bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))

(game-test "Unlock door with bone, without bone"
      (assert-set :lock-jammed)
      (test-input "UNLOCK DOOR WITH BONE")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "POKE BONE IN LOCK, without bone"
      (assert-set :lock-jammed)
      (test-input "POKE BONE IN LOCK")
      (assert-set :lock-jammed)
      (assert-object-in "FINGER BONE" :dungeon-cell))

(game-test "Examining the floor"
      (assert-object-in "SHINY KEY" :nowhere)
      (assert-clr :floor-examined)
      (test-input "EXAMINE FLOOR")
      (assert-set :floor-examined)
      (assert-clr :gorilla-seen)
      (assert-clr :sought-gorilla))

(game-test "Examining the crack"
      (test-input "EXAMINE CRACK")
      (assert-object-in "SHINY KEY" :nowhere)
      (test-input "EXAMINE FLOOR" "EXAMINE CRACK")
      (test-input "LICK SLIME")
      (assert-object-in "SHINY KEY" :nowhere)
      (test-input "EXAMINE CRACK")
      (assert-object-in "SHINY KEY" :dungeon-cell))

(game-test "The window"
      (assert-clr :door-open)
      (assert-object-in "INEDIBLE SLOP" :nowhere)
      (test-input "BANG WINDOW")
      (assert-object-in "INEDIBLE SLOP" :dungeon-cell))

(game-test "Can take key"
      (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK" "TAKE KEY")
      (assert-object-in "SHINY KEY" :inventory))

(game-test "Can bang on door and get slop"
      (assert-object-in "INEDIBLE SLOP" :nowhere)
      (test-input "BANG DOOR")
      (assert-object-in "INEDIBLE SLOP" :dungeon-cell))

(game-test "Eating the slop makes it go away"
      (test-input "BANG DOOR" "EAT SLOP")
      (assert-object-in "INEDIBLE SLOP" :nowhere))

(game-test "Knocking on door doesn blow up"
  ;;was a missing 0 vtable terminator
  (test-input "KNOCK DOOR"))

(defparameter *corridor-state* nil)

(game-test "Can escape"
  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
	      "TAKE KEY" "TAKE FINGER BONE" "POKE BONE IN LOCK"
	      "UNLOCK DOOR" "OPEN DOOR" "USE DOOR")
  (assert-location :corridor)
  (setf *corridor-state* (dump-state-base64)))

(game-test "Test restoring state"
  (restore-game *corridor-state* :print nil)
  (assert-object-in "SHINY KEY" :inventory)
  (assert-location :corridor))

(game-test "Entering cell goes back to cell"
  (restore-game *corridor-state* :print nil)
  (test-input "ENTER CELL")
  (assert-location :dungeon-cell))

(game-test "Entering cell goes back to cell- don't see balls."
  (restore-game *corridor-state* :print nil)
  (test-input "ENTER CELL" "LOOK")
  (assert (string/= "A red ball." (first *print-transcript*))))

(game-test "Second examine action is delegated"
  ;;testing whether a verb-action can be delegated.
  (restore-game *corridor-state* :print nil)
  (test-input "EXAMINE TORCHES" "EXAMINE TORCHES")
  (assert-msg "A row of flickering torches."))

(game-test "When IT is nil, give a better message than I don't see that"
  (restore-game *corridor-state* :print nil)
  (test-input "LOOK" "TAKE IT")
  (assert-msg *be-more-specific*))

(game-test "Empty it object, TAKE"
  (test-input "EXAMINE FLIB" "TAKE IT")
  (assert-msg *be-more-specific*))

(game-test "IT doesn't follow you around"
  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
	      "TAKE KEY" "TAKE BONE" "POKE IT IN KEYHOLE"
	      "UNLOCK DOOR" "OPEN IT" "DROP KEY" "EXIT" "TAKE IT")
  (assert-object-in "SHINY KEY" :dungeon-cell))
