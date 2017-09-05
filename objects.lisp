;; objects are adjective noun. Adjectives and nouns are words
;; words have single byte meanings. So we have a natural object
;; id of a pair. That is quite expensive to keep as a table so
;; I think a reduction table from pair->single byte might be
;; in order.

;; OBJECTS have a CODE and an ID where CODE = (NOUN . ADJ)
;; NOUN ADJ -> ID as two tables we can dereference to OBJECT ID which is a byte
;; PLACE ID
;; LOCATION has a PLACE ID
;; ELSEWHERE and INVENTORY are PLACES

(defparameter *current-location* nil)

(defparameter *object-name->id* nil)
(defparameter *object-id->data* nil)
(defparameter *place->id* nil)
(defparameter *next-place-id* nil)

(defun defplace (place)
  (aif (gethash place *place->id*)  
       it
       (progn
	 (setf (gethash place *place->id*) *next-place-id*)
	 (1- (incf *next-place-id*)))))

(defun reset-object-model ()
  (setf *object-name->id* (make-hash-table :test 'equal))
  (setf *object-id->data* (make-hash-table :test 'equal))
  (setf *place->id* (make-hash-table :test 'equal))
  (setf *next-place-id* 0)
  
  (defplace :elsewhere)
  (defplace :inventory))

;;TODO justify-with-prompt, here and in (respond)

;;split an object name into a noun and adjective
(defun split (name) 
  (let ((pos (position #\  name)))
    (if pos
	(cons (subseq name 0 pos)
	      (subseq name (1+ pos)))
	(cons name nil))))

(defun defobject (name description &key (initial-place *current-location*))
  ;;Ok, so we're going to take the name and split it
  ;;if there are two words then the first word is the
  ;;adjective, which is not normally required unless
  ;;there are multiple objects of the same name.
  (let* ((pair (split name))
	 (name (if (cdr pair) (cdr pair) (car pair)))
	 (adj (if (cdr pair) (car pair) nil)))
    (unless (gethash name *word->meaning*)
      (defword name))
    (when adj
      (unless (gethash adj *word->meaning*)
	(defword adj)))
    (let ((id (cons (gethash name *word->meaning*)
		    (gethash adj *word->meaning*))))
      (setf (gethash name *object-name->id*) id)
      (setf (gethash id *object-id->data*)
	    (let* ((text (justify-with-image description
					     5 4 *act-font*))
		   (lines (1+ (count #\Newline text))))
	      (assert (= 1 lines) nil (format nil "Object description must be one line ~a" text))
	    (list name initial-place (dstr text)))))))

;;First use case- EXAMINE [ADJECTIVE] OBJECT

;;Entry in generic table EXAMINE ? ?

;;If adjective, look for adjective/noun in place
;;If no adjective, look for noun in place
;;keep looking. If find another one, display "Which one?"
;;If no other one, then retrieve description address for object

;;What do we have? One or two word meanings
;;What do we need to do?
;;See if they are in the place.
;;Then pull out, e.g. the description
;;Linear search seems to be ok in this circumstance, perhaps replace
;;with binary search if necessary- I doubt it will be unless we need to do C64
;;version even then meh.

;; TODO Need to increase to 5 words

;; TODO need to replace this word-table-built flag with something
;; generic to say that the first pass has happened

;; The structure is not optimal- we are going to have two tables
;; one for the name ids and one for the adjective ids. They will
;; be sorted by name id then adjective id. This obviously wastes
;; space in the name id table as there will be duplicates, one
;; for each the with same name. It is not obvious how to make
;; this better without making some trade-off which is also not
;; so excellent. Linear search will be used at first, though it
;; could be improved by binary search.

(defun object-table ()
  ;;return values - Y = index of matching item
  ;;                C = Set if not unique
  ;;                Z = Set if not found
  (when *word-table-built*
    (label :find-object-index)
    (zp-b :current-place)
    (with-namespace :find-object-index
      (alias :name :D0)
      (alias :adjective :D1)
      (alias :pos :A0)
      (alias :found-index :D2)
      (dc "Linear search for the name")
      (LDY 0)
      (STY.ZP :found-index)
      (label :next-name)
      (LDA.ZP :name)
      (label :next-name1)
      (INY)
      (dc "Check in one-based name table")
      (CMP.ABY (1- (resolve :names)))
      (BEQ :found-name)
      (BPL :next-name1)
      (label :not-found)
      ;;i.e. the name id < the name id in the table, and since they
      ;;are in order of name id.
      (CLC "Definitely not a duplicate if we are here")
      (LDY.ZP :found-index)
      (RTS)
      (label :found-name)
      (dc "Check adjective")
      (LDA.ZP :adjective)
      (BEQ :adjective-matches "Zero adjective always matches...")
      (CMP.ABY (1- (resolve :adjectives)))
      (BNE :next-name)
      (label :adjective-matches)
      (dc "Now check it is in our place")
      (LDA.ABY (1- (resolve :places)))
      (BEQ :next-name "Object is elsewhere")
      (CMP 1)
      (BEQ :found "Object is in our inventory")
      (CMP.ZP :current-place)
      (BEQ :found "Object is in our current location")
      ;;TODO check to see if the place is a container
      ;;then go up there.
      (BNE :next-name)
      (label :found)
      (dc "If we already have found one then return")
      (LDA.ZP :found-index)
      (BNE :already-found)
      (STY.ZP :found-index)
      (BEQ :next-name)
      (label :already-found)
      (dc "Carry AND not-zero, i.e. duplicate AND found")
      (SEC)
      (RTS)

      ;;make a list of words and sort it (id-wise) by name
      ;;then adjective.
      
      (let ((objects nil))
	(maphash #'(lambda (k v)
		     ;;basically this number is the 16 bit object id
		     ;;name adjective
		     (push (cons (logior (ash (nil->0 (car k)) 8)
					 (nil->0 (cdr k)))
				 v)
			   objects))
		 *object-id->data*)
	;;sort them so they are in name adjective order
	
	(setf objects (sort objects #'< :key #'car))

	;; function to initialise the objects
	
	(label :init-objects nil)

	(LDY (length objects))
	(label :copy-place)
	(LDA.ABY (1- (resolve :initial-places)))
	(STA.ABY (1- (resolve :places)))
	(DEY)
	(BNE :copy-place)
	(RTS)
	
	;;now we can generate the object data tables
	
	(apply #'db :names (mapcar #'(lambda (o) (ash (car o) -8)) objects))
	(apply #'db :adjectives (mapcar #'(lambda (o) (logand #xff (car o))) objects))
	(let ((places (mapcar #'(lambda (o)
				  (nil->0 (gethash (third o) *place->id*)))
			      objects)))
	  (apply #'db :places places)
	  (apply #'db :initial-places places))
	
	(apply #'db :description-hi (mapcar #'(lambda (o) (hi (fourth o))) objects))
	(apply #'db :description-lo (mapcar #'(lambda (o) (lo (fourth o))) objects))))))

(defun dump-objects ()
  (let ((objects nil))
	(maphash #'(lambda (k v)
		     ;;basically this number is the 16 bit object id
		     ;;name adjective
		     (push (cons (logior (ash (nil->0 (car k)) 8)
					 (nil->0 (cdr k)))
				 v)
			   objects))
		 *object-id->data*)
	;;sort them so they are in name adjective order
	
	(setf objects (sort objects #'< :key #'car))
	(dolist (object objects)
	  (print object))))

(defun dump-places ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) *place->id*))

;;test finding object id

(defun object-tester (name-id adj-id current-place)
  (reset-compiler)
  (reset-symbol-table)
  (font-data)
  (reset-compiler)
  (let ((p 0))
    (flet ((pass ()
	     ;(format t "Pass ~a~%" (incf p))
	     (zeropage)	     
	     (org #x600)
	     (CLD)
	     (label :start)
	   
	     (JSR :init-objects)
	   
	     (LDA name-id)
	     (STA.ZP '(:find-object-index . :name))
	     (LDA adj-id)
	     (STA.ZP '(:find-object-index . :adjective))
	     (LDA current-place)
	     (STA.ZP :current-place)

	     (JSR :find-object-index)
	  
	     (BRK)
	   
	     (object-table)
	     (string-table)
	   
	     (label :end)

	     (font-data)))
      (pass)
      (build-symbol-table)
      (setf *word-table-built* t)
    
      (pass)
      (let ((end *compiler-ptr*))
	(pass)
	(assert (= end *compiler-ptr*) nil "Build was not stable"))
      (setf *compiler-final-pass* t)
      (pass)
    
      ;;(format t "Build size ~a~%" (- *compiler-ptr* origin))
      ))

  (monitor-reset #x600)
  (monitor-run :print nil)

  (multiple-value-bind (buffer pc sp sr a x y)
      (funcall *monitor-get-state*)
    (declare (ignore buffer pc sp x a y))
    (values (if (zerop (logand sr 2)) :found :not-found)
	    (if (zerop (logand sr 1)) :unique :duplicate))))

(defun test-object-find (adjective name place expected-found expected-duplicate)
  (multiple-value-bind (found duplicate)
      (object-tester (nil->0 (gethash name *word->meaning*))
		     (nil->0 (gethash adjective *word->meaning*))
		     (gethash place *place->id*))
    (assert (eq found expected-found))
    (assert (eq duplicate expected-duplicate))))

(reset-object-model)
(reset-parser)
(font-data)

(defplace :ur)
(defplace :nippur)
(defplace :babylon)

(defobject "MARDUK STATUE" "A bronze statue" :initial-place :ur)
(defobject "STONE STATUE" "A stone statue" :initial-place :ur)
(defobject "GINGER BISCUIT" "A tasty snack" :initial-place :ur)
(defobject "ENTRAILS" "Animal guts" :initial-place :nippur)
(defobject "POCKET FLUFF" "Lovely pocket fluff" :initial-place :inventory)
(defobject "OBSIDIAN CUBE" "Black cube" :initial-place :elsewhere)
(defobject "CAT FLUFF" "Cat fluff" :initial-place :babylon)

(test-object-find "MARDUK" "STATUE" :ur :found :unique)
(test-object-find "STONE" "STATUE" :ur :found :unique)
(test-object-find "GINGER" "BISCUIT" :ur :found :unique)
(test-object-find nil "BISCUIT" :ur :found :unique)
(test-object-find "POCKET" "FLUFF" :ur :found :unique)
(test-object-find "POCKET" "FLUFF" :nippur :found :unique)
(test-object-find nil "FLUFF" :ur :found :unique)
(test-object-find nil "FLUFF" :nippur :found :unique)
(test-object-find nil "FLUFF" :babylon :found :duplicate)
(test-object-find nil "STATUE" :ur :found :duplicate)
(test-object-find nil "STATUE" :nippur :not-found :unique)

(test-object-find "MARDUK" "STATUE" :nippur :not-found :unique)
(test-object-find "STONE" "STATUE" :nippur :not-found :unique)
(test-object-find nil "ENTRAILS" :nippur :found :unique)
(test-object-find nil "ENTRAILS" :ur :not-found :unique)

(test-object-find "GINGER" "STATUE" :ur :not-found :unique)
(test-object-find "STONE" "BISCUIT" :ur :not-found :unique)
(test-object-find "ENTRAILS" "FLUFF" :ur :not-found :unique)
(test-object-find nil "GINGER" :ur :not-found :unique)
	  









      
      
      

