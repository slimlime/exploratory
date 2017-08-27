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

(defparameter *object-id->data* nil)
(defparameter *place->id* nil)
(defparameter *next-place-id* nil)

(defun defplace (place)
  (setf (gethash place *place->id*) *next-place-id*)
  (incf *next-place-id*))

(defun reset-object-model ()
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

(defun defobject (name place description)
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
    (setf (gethash (cons (gethash name *word->meaning*)
			 (gethash adj *word->meaning*))
		   *object-id->data*)
	  (list name place (dstr (justify-with-image description
			    5 4 *act-font*))))))

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
  ;;                A (and Z) with other matching item
  (when *word-table-built*
    (label :find-object-index)
    (zp-b :current-place)
    (with-namespace :get-object-index
      (alias :name :D0)
      (alias :adj :D1)
      (alias :pos :A0)
      (alias :found-index :D2)
      (dc "Linear search for the name")
      (LDY 0)
      (STY.ZP :result)
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
      (dc "We went past it, can't possibly find it (or another) now")
      (LDY :found-index)
      (LDA 0)
      (RTS)
      (label :found-name)
      (dc "Check adjective")
      (LDA :adj)
      (BEQ :adjective-matches "Zero adjective always matches...")
      (CMP.ABY (1- (resolve :adjectives)))
      (BNE :next-name)
      (label :adjective-matches)
      (dc "Now check it is in our place")
      (LDA.ABY :places)
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
      (LDY.ZP :found-index)
      (BNE :already-found)
      (STY.ZP :found-index)
      (BEQ :next-name)
      (label :already-found)
      (LDA.ZP :found-index)
      (RTS)

      ;;make a list of words and sort it (id-wise) by name
      ;;then adjective.

      (let ((objects nil))
	(maphash #'(lambda (k v)
		     ;;basically this number is the 16 bit object id
		     ;;name adjective
		     (push (cons (logior (ash (car k) 8)
					 (cdr k))
				 v)
			   objects))
		 *object-id->data*)
	;;sort them so they are in name adjective order
	
	(setf objects (sort objects #'< :key #'car))

	;; function to initialise the objects
	
	(label :init-objects)
	(LDY (length object))
	(label :copy-place)
	(LDA.ABY (1- (resolve :initial-places)))
	(STA.ABY (1- (resolve :places)))
	(DEY)
	(BNE :copy-place)
	
	;;now we can generate the object data tables
	
	(apply #'db :names (mapcar #'(lambda (o) (ash (car o) -8))))
	(apply #'db :adjectives (mapcar #'(lambda (o) (logand #xff (car o)))))
	(let ((places (mapcar #'(lambda (o) (gethash *place->id* (cadr o))))))
	  (apply #'db :places places)
	  (apply #'db :initial-places places))
	(apply #'db :description-hi (mapcar #'(lambda (o) (hi (caddr o)))))
	(apply #'db :description-lo (mapcar #'(lambda (o) (lo (caddr o)))))))))


      
      
      

