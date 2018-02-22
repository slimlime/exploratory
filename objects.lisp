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
(defparameter *object-name->index* nil)
(defparameter *place->id* nil)
(defparameter *next-place-id* nil)
(defparameter *current-object* nil)
(defparameter *object->vtable* nil)

;; Define a new place, probably not necessary to call from game
;; code as the call which actually use the place will call this
;; indirectly.
(defun defplace (place)
  (aif (gethash place *place->id*)  
       it
       (progn
	 (setf (gethash place *place->id*) *next-place-id*)
	 (1- (incf *next-place-id*)))))

;;ELSEWHERE = 0
;;INVENTORY = 1
(defun reset-object-model ()
  (setf *object-name->id* (make-hash-table :test 'equal))
  (setf *object-id->data* (make-hash-table :test 'equal))
  (setf *place->id* (make-hash-table :test 'equal))
  (setf *object->vtable* (make-hash-table :test 'equal))
  (setf *object-name->index* (make-hash-table :test 'equal))
  (setf *next-place-id* 0)
  
  (defplace :nowhere)
  (defplace :inventory))

;;TODO justify-with-prompt, here and in (respond)

;;split an object name into a noun and adjective
(defun split (name) 
  (let ((pos (position #\  name)))
    (if pos
	(cons (subseq name 0 pos)
	      (subseq name (1+ pos)))
	(cons name nil))))

(defun ends-with-punctuation (string)
  (let ((char (char string (1- (length string)))))
    (or (char= char #\')
	(char= char #\.)
	(char= char #\!)
	(char= char #\?))))

(defun warn-if-not-punctuated (s)
  (when *compiler-final-pass*
    (unless (ends-with-punctuation s)
      (format t "WARNING string '~a' does not end with punctuation.~%" s))))

(defun name-with-indefinite-article (name)
  (let ((name (string-downcase name)))
    (format nil "~a ~a."
	    (if (find (elt name 0) "aeiou")
		"An"
		"A")
	    name)))

(assert (string= "An apple." (name-with-indefinite-article "APPLE")))
(assert (string= "A telephone." (name-with-indefinite-article "TELEPHONE")))

(defun decompose-name (name)
  "Decompose a name into noun adjective"
  (let ((pair (split name)))
    (values (if (cdr pair) (cdr pair) (car pair))
	    (if (cdr pair) (car pair) nil))))

(defun make-object-id (noun adj)
  "Turn an object name into a cons pair of word ids"
  (unless (gethash noun *word->meaning*)
    (defword noun))
  (when adj
    (unless (gethash adj *word->meaning*)
      (defword adj)))
  (cons (gethash noun *word->meaning*)
	(gethash adj *word->meaning*)))

(defun make-object-data (name name-override noun initial-place description)
  "Return a list of data associated with an object"
  (let* ((text (justify-with-image description
				   5 4 *act-font*))
	 (lines (1+ (count #\Newline text))))
    (assert (< lines 4) nil (format nil "Object description must be 1-3 lines, was ~a ~a" lines text))
    (list noun
	  initial-place
	  (dstr text)
	  (dstr (if name-override
		    (progn
		      (warn-if-not-punctuated name-override)
		      name-override)
		    (name-with-indefinite-article name)))
	  lines
	  name)))

(defun defobject-fn (names description initial-place name-override)
  (when (stringp names)
    (setf names (list names)))
  (let ((name (if (listp names) (first names) names)))
    (when (null initial-place)
      (setf initial-place *current-location*))
    ;;firstly ensure the place exists
    (defplace initial-place)
    ;;Ok, so we're going to take the name and split it
    ;;if there are two words then the first word is the
    ;;adjective, which is not normally required unless
    ;;there are multiple objects of the same name.
    (multiple-value-bind (noun adj)
	(decompose-name name)
      (let ((id (make-object-id noun adj)))
	(setf (gethash id *object-id->data*)
	      (make-object-data name name-override noun initial-place description))
	(dolist (alias names)
	  (setf (gethash alias *object-name->id*) id))))))
	
(defmacro with-object (object &body body)
  `(let ((*current-object* ,object))
     ,@body))

(defmacro defobject (names description initial-place display-name-override &body body)
  "Define an object. Initial place may be nil for 'here'. Name-override may be nil for
standard object display name e.g. 'A golden apple.' Names can be a list of names,
all of which will refer to the same object."
  (let ((names-sym (gensym)))
    `(let ((,names-sym ,names))
       (with-object ,names-sym
	 (defobject-fn ,names-sym ,description ,initial-place ,display-name-override)
	 ,@body))))
     
(defun object-table ()
  ;;return values - Y = index of matching item
  ;;                C = Set if not unique
  ;;                Z = Set if not found
  (when *word-table-built*
    ;;Current-place does not need to be saved as it will
    ;;be set implicitly by a call to restore the game state
    ;;when there is a call to navigate made.
    (zp-b :current-place)
    (with-namespace :object-table
      (alias :noun :D0)
      (alias :adjective :D1)
      (alias :pos :A0)
      (alias :found-index :D2)
      (when (resolves '(:parser . :words))
	;;some of the test functions don't use the parser
	;;so this entry point won't compile- exclude it if
	;;that is the case
	(label :find-object-index-from-input nil)
	(dc "Get the third word, e.g. TAKE ADJ NOUN")
	(LDA.AB (+ 2 (resolve '(:parser . :words))))
	(BEQ :no-adjective "Could be of form TAKE NOUN")
	(STA.ZP :noun)
	(LDA.AB (+ 1 (resolve '(:parser . :words))))
	(STA.ZP :adjective)
	(JSR :find-object-index)
	(BEQ :ignore-word-three)
	(RTS)
	(dc "We didn't find it, but what if the third word")
	(dc "isn't part of the first object? Could be TAKE NOUN1 NOUN2")
	(label :ignore-word-three)
	(LDA 0)
	(label :no-adjective)
	(STA.ZP :adjective)
	(LDA.AB (+ 1 (resolve '(:parser . :words))))
	(STA.ZP :noun))
      (label :find-object-index nil)
      (dc "Linear search for the noun")
      (LDY 0)
      (STY.ZP :found-index)
      (label :next-noun)
      (LDA.ZP :noun)
      (label :next-noun1)
      (INY)
      (dc "Check in one-based name table")
      (CMP.ABY (1- (resolve :names)))
      (BEQ :found-name)
      (BGE :next-noun1)
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
      (BNE :next-noun)
      (label :adjective-matches)
      (dc "Now check it is in our place")
      (LDA.ABY (1- (resolve :places)))
      (BEQ :next-noun "Object is elsewhere")
      (CMP 1)
      (BEQ :found "Object is in our inventory")
      (CMP.ZP :current-place)
      (BEQ :found "Object is in our current location")
      ;;TODO check to see if the place is a container
      ;;then go up there.
      (BNE :next-noun)
      (label :found)
      (dc "If we already have found one then return")
      (LDA.ZP :found-index)
      (BNE :already-found)
      (STY.ZP :found-index)
      (BEQ :next-noun)
      (label :already-found)
      (dc "Carry AND not-zero, i.e. duplicate AND found")
      (SEC)
      (RTS)
      ;;Objects and their aliases are in noun-id adjective-id order in
      ;;these tables for searching.
      (let ((name-word-ids nil)
	    (next-index 1)
	    (id->index (make-hash-table :test 'equal))
	    (objects nil))
	(clrhash *object-name->index*)
	(do-hash-values (id *object-name->id*)
	  (push id name-word-ids)
	  (let ((index (gethash id id->index)))
	    (unless index
	      (setf index next-index)
	      ;;but each object only has one entry in the object table
	      (setf (gethash id id->index) index)
	      (incf next-index)
	      (push (gethash id *object-id->data*) objects))
	    ;;all object names and alternate names map to the same index
	    (setf (gethash (sixth (gethash id *object-id->data*))
			   *object-name->index*)
		  index)))
	(setf objects (nreverse objects))
	;;sort order, by name then adjective id
	(setf name-word-ids (sort name-word-ids #'< :key #'cdr))
	(setf name-word-ids (stable-sort name-word-ids #'< :key #'car))
	(apply #'db :names (mapcar #'car name-word-ids))
	(apply #'db :adjectives (mapcar #'cdr name-word-ids))
	(apply #'db :object-index (mapcar #'(lambda (id) (gethash id id->index)) name-word-ids))
	(when *compiler-final-pass*
	  (print name-word-ids)
	  (print objects)
	  (do-hashtable (k v *object-name->index*) (format t "~a->~a~%" k v)))
	;; function to initialise the objects
	(label :init-objects nil)
	(LDY (length objects))
	(label :copy-place)
	(LDA.ABY (1- (resolve :initial-places)))
	(STA.ABY (1- (resolve :places)))
	(DEY)
	(BNE :copy-place)
	(RTS)
	;; now we can generate the object data tables
	(let ((places (mapcar #'(lambda (o)
				  (nil->0 (gethash (second o) *place->id*)))
			      objects)))
	  (game-state-bytes "Object Places"
	    (apply #'db :places places))
	  (apply #'db :initial-places places))
	;; object name strings
	(apply #'db :name-hi (mapcar #'(lambda (o) (hi (fourth o))) objects))
	(apply #'db :name-lo (mapcar #'(lambda (o) (lo (fourth o))) objects))
	;; object verb handlers
	(labels ((verb-addr (o)
		   (if (gethash (sixth o) *object->vtable*) 
		       (cons :vtable (sixth o))
		       0)))
	  (apply #'db :verb-hi (mapcar #'(lambda (o) (hi (verb-addr o))) objects))
	  (apply #'db :verb-lo (mapcar #'(lambda (o) (lo (verb-addr o))) objects)))
	;; object descriptions
	(apply #'db :description-hi (mapcar #'(lambda (o) (hi (third o))) objects))
	(apply #'db :description-lo (mapcar #'(lambda (o) (lo (third o))) objects))
	(apply #'db :description-lines (mapcar #'(lambda (o) (lo (fifth o))) objects))
	(maphash #'(lambda (object verb-handlers)
		     (label object :vtable)
		     (dolist (verb-handler verb-handlers)
		       (dc (format nil "~a" (car verb-handler)) t)
		       (db nil (word-id (car verb-handler)))
		       (dw nil (cdr verb-handler)))
		     (db nil 0))
		 *object->vtable*)))))

(defun dump-objects ()
  (dolist (object (build-object-table))
    (print object)))

(defun objects-count ()
  (hash-table-count *object-id->data*))

(defun dump-places ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) *place->id*))

(defun place-id (place)
  "Get the id for a place, e.g. :dungeon"
  (let ((id (gethash place *place->id*)))
    (when *compiler-final-pass*
      (assert id (place) "~a is not a valid place" place))
    (if id id 0)))

(defun object-id (name)
  (if *compiler-final-pass*
      (gethash name *object-name->index*)
      0))

(defun object-place-address (name)
  "Get the object place address"
  (+ -1 (resolve '(:object-table . :places)) (object-id name)))

;;test finding object id

(defun test-object-definitions ()
  (defplace :ur)
  (defplace :nippur)
  (defplace :babylon)
  
  (defobject "MARDUK STATUE" "A bronze statue" :ur nil)
  (defobject "STONE STATUE" "A stone statue" :ur nil)
  (defobject "GINGER BISCUIT" "A tasty snack" :ur nil)
  (defobject "ENTRAILS" "Animal guts" :nippur nil)
  (defobject "POCKET FLUFF" "Lovely pocket fluff" :inventory nil)
  (defobject "OBSIDIAN CUBE" "Black cube" :nowhere nil)
  (defobject "CAT FLUFF" "Cat fluff" :babylon nil))	   

(defun object-tester (name-id adj-id current-place)
  (reset-compiler)
  (reset-strings)
  (font-data)
  (reset-compiler)
  (reset-object-model)
  (reset-parser)
	   
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (label :start)
	   
	   (JSR :init-objects)
	   
	   (LDA name-id)
	   (STA.ZP '(:object-table . :noun))
	   (LDA adj-id)
	   (STA.ZP '(:object-table . :adjective))
	   (LDA current-place)
	   (STA.ZP :current-place)
	   
	   (JSR :find-object-index)
	   
	   (BRK)

	   (test-object-definitions)

	   (object-table)

	   (string-table #() t)
	   (huffman-decoder)
	   
	   (label :end)
	   
	   (font-data)))
    (pass)
    (setf *word-table-built* t)
    
    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))
    (setf *compiler-final-pass* t)
    (pass)
    
    ;;(format t "Build size ~a~%" (- *compiler-ptr* origin))
    )
  
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
#|
(font-data)
(reset-object-model)
(reset-parser)

(test-object-definitions)

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

;; Add parse input tests
;; So this test case comes from POKE BONE DOOR which didn't work
;; because the finder is looking for a Bone Door rather than
;; Bone and ignoring the door.
;; (test-object-find "BISCUIT" "DOG" :ur :found :unique)
      
(assert (= 1 (object-id "MARDUK STATUE")))
(assert (= 2 (object-id "STONE STATUE")))
(assert (= 3 (object-id "GINGER BISCUIT")))
(assert (= 4 (object-id "ENTRAILS")))
(assert (= 5 (object-id "POCKET FLUFF")))
(assert (= 6 (object-id "CAT FLUFF")))
(assert (= 7 (object-id "OBSIDIAN CUBE")))

|#
