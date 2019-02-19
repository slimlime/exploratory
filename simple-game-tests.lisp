;;;;;;;;;;;;;
;;;TESTING;;;
;;;;;;;;;;;;;

(defvar *tests-warn-only* t)

(defun test-fn (description body-fn deferred)
  ;;deferred tests will show a warning, unless
  ;;they pass, in which case they will assert :-)
  ;;non-deferred tests will assert, unless *tests-warn-only*
  ;;is set to true.
  (monitor-reset :start)
  (monitor-run :break-on 'BRK :print nil)
  (let ((result t))
    (handler-case
	(funcall body-fn)
      (error (e)
	(progn
	  (setf result nil)
	  (let ((msg (format nil "~a FAILED~%~t'~a'" description e)))
	    (if (or *tests-warn-only* deferred)
		(format t "~a~%" msg)
		(error "~S" msg))))))
    (when (and result deferred)
      (error "The deferred test '~S' actually passed. Undefer it." description)))
  (values))

(defmacro test (description &body body)
  `(test-fn ,description #'(lambda () ,@body) nil))

(defmacro deferred-test (description &body body)
  `(test-fn ,description #'(lambda () ,@body) t))

;; this compiles the game, each time a test is run it resets the monitor
;; by copying the compile buffer into the monitor buffer so each test is independent.

(run-simple-game :print nil)

(test "Unknown exit message"
  (test-input "N")
  (assert-msg *cant-go-that-way*))

(test "Unknown exit synonym message"
  (test-input "NORTH")
  (assert-msg *cant-go-that-way*))

(test "Known exit, override message"
  (test-input "OUT")
  (assert-msg *snickering*))

(test "Unknow three word gives error"
  (test-input "BAA BAB BDE")
  (assert-msg *unknown-word*))

(test "Unknown two word gives error"
  (test-input "FLIBBLE WIBBLE")
  (assert-msg *unknown-word*))

(test "Unknown single word gives error"
  (test-input "RUN")
  (assert-msg *dont-understand*))

(test "Drop object we don't have"
  (test-input "DROP BONE")
  (assert-msg *do-not-have*))

(test "Do something weird to an object"
  (test-input "TAKE BONE" "KILL IT")
  (assert-msg *cant-do-that*))

(test "Do something weird to an object, with an override"
  (test-input "TAKE BONE" "EAT IT")
  (assert-msg *thegodslookaway*))

(test "Take object you already have"
  (test-input "TAKE BONE" "TAKE BONE")
  (assert-msg *already-have*))

(test "Take a fixture"
  (test-input "TAKE SLIME")
  (assert-msg *cant-take-that*))

(test "Take message works"
  (test-input "TAKE BONE")
  (assert-msg *you-took-it*))

(test "Dropped it message"
  (test-input "TAKE BONE" "DROP IT")
  (assert-msg *you-dropped-it*))

(test "I don't see that message"
  (test-input "TAKE BALL")
  (assert-msg *dont-see-that*))

(test "I don't see that with override"
  (test-input "TAKE KEY")
  (assert-msg *whatyoutalkingabout*))

(test "After an inventory with one item, does EXAMINE IT show correct message"
      (test-input "TAKE FINGER BONE" "I" "EXAMINE IT")
      (assert-object-in "FINGER BONE" :inventory)
      (assert-msg "The long, slender digit of a long since
departed previous occupant of your cell.
Human? YOU decide."))

;;The reason this doesn't pass is that the object IT is always replaced by
;;either an actual single object or a 0.
;;
;;Perhaps we shouldn't replace IT with 0, but leave it as is if it cannot
;;be unambiguously identified, that way we could modulate the message to
;;make more sense.
;;
;;(test "With more than one object in inventory, be more specific than IT"
;;  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
;;		"TAKE KEY" "TAKE FINGER BONE" "I" "EXAMINE IT")
;;  (assert-object-in "SHINY KEY" :inventory)
;;  (assert-object-in "FINGER BONE" :inventory)
;;  (assert-msg *be-more-specific*) ))

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

(test "Unlock door with finger bone, with finger bone"
      (assert-set :lock-jammed)
      (test-input "TAKE BONE" "UNLOCK DOOR WITH FINGER BONE")
      (assert-clr :lock-jammed)
      (assert-object-in "FINGER BONE" :nowhere))
  
(test "Unlock door with finger, without finger"
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

(defparameter *corridor-state* nil)

(test "Can escape"
  (test-input "LICK SLIME" "EXAMINE FLOOR" "EXAMINE CRACK"
	      "TAKE KEY" "TAKE FINGER BONE" "POKE BONE IN LOCK"
	      "KNOCK DOOR" "UNLOCK DOOR" "OPEN DOOR" "USE DOOR")
  (assert-location :corridor)
  (setf *corridor-state* (dump-state-base64)))

(test "Test restoring state"
  (restore-game *corridor-state* :print nil)
  (assert-object-in "SHINY KEY" :inventory)
 
  (assert-location :corridor))
