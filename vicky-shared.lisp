;; shared VIC-II parameters


;; 0 - black   1 - white       2 - red        3 - cyan
;; 4 - purple  5 - green       6 - blue       7 - yellow
;; 8 - orange  9 - brown       A - light red  B - dark grey
;; C - grey    D - light green E - light blue F - light grey

(defparameter *c64-colours* #(#x000000 #xFFFFFF #x68372B #x70A4B2
			      #x6F3D86 #x588D43 #x352879 #xB8C76F
			      #x6F4F25 #x433900 #x9A6759 #x444444
			      #x6C6C6C #x9AD284 #x6C5EB5 #x959595))

(defparameter *screen-memory-address* #x8000)
(defparameter *char-memory-address* #x7000)

(defconstant +char-memory-length+ 1000)
(defconstant +screen-memory-length+ 8000)
(defconstant +screen-width+ 320)
(defconstant +screen-width-bytes+ 40)
(defconstant +screen-height+ 200)

(defun scradd (row col)
  (+ *screen-memory-address* (* +screen-width-bytes+ row) col))

(defun scrxy (addr)
  "Get the xy byte offsets from a screen memory pointer"
  (multiple-value-bind (y x)
      (floor (- addr *screen-memory-address*) +screen-width-bytes+)
    (values x y)))

(defun scrpxy (addr &optional bit)
  "Get the xy pixel offsets from a screen memory pointer and
an optional bit position, i.e. 0-7"
  (multiple-value-bind (x y)
      (scrxy addr)
    (values (+ (* 8 x) (nil->0 bit)) y)))

(let ((*screen-memory-address* 1000))
  (assert (multiple-value-bind (x y)
	      (scrxy 1001)
	    (and (= x 1) (= y 0))))
  (assert (multiple-value-bind (x y)
	      (scrxy 1042)
	    (and (= x 2) (= y 1))))
  (assert (multiple-value-bind (x y)
	      (scrpxy 1001)
	    (and (= x 8) (= y 0))))
  (assert (multiple-value-bind (x y)
	      (scrpxy 1042)
	    (and (= x 16) (= y 1)))))

