;; shared VIC-II parameters

(defparameter *c64-colours* #(#x000000 #xFFFFFF #x68372B #x70A4B2
			      #x6F3D86 #x588D43 #x352879 #xB8C76F
			      #x6F4F25 #x433900 #x9A6759 #x444444
			      #x6C6C6C #x9AD284 #x6C5EB5 #x959595))

(defparameter *screen-memory-address* #x8000)
(defparameter *char-memory-address* #x7000)

(defconstant +char-memory-length+ 1000)
(defconstant +screen-memory-length+ 8000)
(defconstant +screen-width+ 320)
(defconstant +screen-height+ 200)
