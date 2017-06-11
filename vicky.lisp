(defparameter *colours* (make-array 16))
(defparameter *render-width* (* 2 +screen-width+))
(defparameter *render-height* (* 2 +screen-height+))
(defparameter *sx* 2)
(defparameter *sy* 2)

(defun map-colour (surface-fp r g b)
  (sdl-cffi::sdl-map-rgb (sdl-base:pixel-format surface-fp)
			 b g r))

;; 0 - black   1 - white       2 - red        3 - cyan
;; 4 - purple  5 - green       6 - blue       7 - yellow
;; 8 - orange  9 - brown       A - light red  B - dark grey
;; C - grey    D - light green E - light blue F - light grey

(defun map-c64-colours (surface-fp)
  (loop for i from 0 to 15 do
       (setf (aref *colours* i)
	     (let ((colour (aref *c64-colours* i)))
	       (map-colour surface-fp (ash colour -16)
			   (logand #xff (ash colour -8))
			   (logand #xff colour))))))

(defun vicky ()
  (map-memory)
  (sdl:window *render-width* *render-height* :title-caption "VICKY")
  (sdl:with-init ()
    (let ((surface (sdl:create-surface *render-width*
				       *render-height*
				       :bpp 24)))
      (let* ((redraw nil)
	     (timer (make-timer #'(lambda () (setf redraw t)))))
	(schedule-timer timer 0 :repeat-interval 1)
	
	(sdl:with-events ()
	  (:idle ()
		 (when redraw
		   (let ((sfp (sdl:fp surface)))
		     (map-c64-colours sfp) ; I wonder if this really has to be done for every instance
		     (lispbuilder-sdl-base::with-pixel (p sfp)
		       (let ((x 0) (y 0))
			   (loop for ptr from 0 to (1- +screen-memory-length+) do
				(let* ((byte (getmem (+ ptr *screen-memory-address*)))
				       (bit #x80)
				       (attribute (getmem (+ *char-memory-address*
							     (mod ptr 40)
							     (* 40 (floor ptr 320)))))
				       (fg (aref *colours* (ash attribute -4)))
				       (bg (aref *colours* (logand #xf attribute))))
				  (loop while (not (= 0 bit)) do
				       (let ((color (if (= 0 (logand bit byte)) bg fg))) 
					 (lispbuilder-sdl-base::write-pixel p x y color)
					 (lispbuilder-sdl-base::write-pixel p (1+ x) y color)	
					 (setf bit (ash bit -1))
					 (incf x *sx*)))
				  (when (>= x *render-width*)
				    (incf y *sy*)
				    (when (>= y *render-height*)
				      (return))
				    (setf x 0)))))))
		   (setf redraw nil)
		   (sdl:blit-surface surface)
		   (sdl:update-display)))
	  (:key-down-event (:key key)
			   (when (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)))
	  (:quit-event () 
		       (unschedule-timer timer)
		       t)
	  (:video-expose-event ()
			       (sdl:update-display)))))))

;(dolist (timer (list-all-timers)) (unschedule-timer timer))
