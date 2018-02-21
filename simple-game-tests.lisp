;;;;;;;;;;;;;
;;;TESTING;;;
;;;;;;;;;;;;;

(run-simple-game :print nil)

(defmacro test (&body body)
  `(progn
     (monitor-reset :start)
     (monitor-run :break-on 'BRK :print nil)
     ;;(setmem-copy (monitor-buffer))
     ,@body))

(test "Examining slime"
  (assert-clr :slime-examined)
  (test-input "EXAMINE SLIME")
  (assert-set :slime-examined))

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

#|(test "Taking and dropping the bone, as adjunct noun"
  (assert-object-in "FINGER BONE" :dungeon-cell)
  (test-input "TAKE FINGER")
  (assert-object-in "FINGER BONE" :inventory)
  (test-input "DROP FINGER")
  (assert-object-in "FINGER BONE" :dungeon-cell))|#

(test "Poking out the blockage and losing the bone"
  (assert-set :lock-jammed)
  (test-input "TAKE BONE" "POKE BONE IN LOCK")
  (assert-clr :lock-jammed)
  (assert-object-in "FINGER BONE" :nowhere))
