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

;; TODO The lower level parser should probably not return any index
;; if the there is a duplicate
;; TODO The show property can be inferred and doesn't need to be a bit
;;      as if the name is null then it can't be shown.
;; TODO empty strings should not be inlined, see the description for CELL FLOOR
;; TODO There is a CPY 6, what is the 6, I forget

(defparameter here nil "The current location")
(defparameter this nil "The current object")

(defparameter *object-name->data* nil)
(defparameter *object-name->index* nil)
(defparameter *place->id* nil)
(defparameter *next-place-id* nil)
(defparameter *object->vtable* nil)

;;object property bitmasks

(defparameter *object-take* 1)
(defparameter *object-show* 2)

(defun object-properties (take show)
  "Return a bitmask for the non-null properties specified"
  (logior
   (if take *object-take* 0)
   (if show *object-show* 0)))

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
  (setf *object-name->data* (make-hash-table :test 'equal))
  (setf *object-name->index* (make-hash-table :test 'equal))
  (setf *place->id* (make-hash-table :test 'equal))
  (setf *object->vtable* (make-hash-table :test 'equal))
  (setf *next-place-id* 0)
  (defplace :nowhere)
  (defplace :inventory))

;;TODO justify-with-prompt, here and in (respond)

;;split an object name into noun, adjective
(defun split (name) 
  (let ((pos (position #\  name)))
    (if pos
	(values (subseq name (1+ pos))
		(subseq name 0 pos))
	(values name nil))))

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

;;TODO make a struct for the object data it is becoming too unwieldy as a list

(defun make-object-data (name names description initial-place name-override take show)
  "Return a list of data associated with an object"
  (unless show
    (assert (null name-override) nil
	    "Objects that are not shown in LOOK do not need a name override"))
  (let* ((text (justify-with-image description
				   5 4 *act-font*))
	 (lines (1+ (count #\Newline text))))
    (assert (< lines 4) nil (format nil "Object description must be 1-3 lines, was ~a ~a" lines text))
    (list
     names
     (if show ;If we don't show an object, its 'name' can never be seen, e.g in LOOK
	 (dstr (if name-override
		   (progn
		     (warn-if-not-punctuated name-override)
		     name-override)
		   (name-with-indefinite-article name)))
	 nil)
     (dstr text)
     initial-place
     take
     show)))

(defun defobject-fn (names &key show description initial-place name-override take)
  (when description
    (setf description (smart-quote description)))
  (when (stringp names)
    (setf names (list names)))  
  (let ((name (first names)))
    (when (null initial-place)
      (setf initial-place here))
    ;;firstly ensure the place exists
    (defplace initial-place)
    ;;define the object
    (setf (gethash name *object-name->data*)
	  (make-object-data name names description initial-place name-override take show))
    ;;ensure all object names and adjectives have words defined for them
    (let ((nouns (make-hash-table :test 'equal)))
      (dolist (name names)
	(multiple-value-bind (noun adj)
	    (split name)
	  (assert (null (gethash noun nouns)) nil (format nil "Can't have same noun as alias for same object, ~a ~a and ~a ~a" adj noun (gethash noun nouns) noun))
	  ;;note, i'm sure we could, if we needed to do this, at the moment
	  ;;the object searcher thinks it is two different objects and asks
	  ;;to 'be more specific'
	  (setf (gethash noun nouns) adj)
	  (unless (gethash noun *word->meaning*)
	    (defword noun))
	  (when adj
	    (unless (gethash adj *word->meaning*)
	      (defword adj))))))))
 
(defun first-or-it (s)
  (if (listp s) (first s) s))

(defmacro with-object (object &body body)
  `(let ((this ,object))
     ,@body))

(defmacro object (names (&key description place name-override (take t) (show t)) &body body)
  "Define an object. Initial place may be nil for 'here'. Name-override may be nil for
standard object display name e.g. 'A golden apple.' Names can be a list of names,
all of which will refer to the same object."
  (let ((names-sym (gensym)))
    `(let ((,names-sym ,names))
       (with-object (first-or-it ,names-sym)
	 (defobject-fn ,names-sym
	     :description ,description
	     :initial-place ,place
	     :name-override ,name-override
	     :take ,take
	     :show ,show)
	 ,@body))))

(defmacro fixture (names (&key description place name-override) &body body)
  `(object ,names (:description ,description
				:place ,place
				:name-override ,name-override
				:show nil
				:take nil)
     ,@body))

(defun object-id (name)
  (let ((id (gethash name *object-name->index*)))
  (when *compiler-final-pass*
    (assert id nil "Object ~a was not defined" name))
  (nil->0 id)))

(defun object-table ()

  (defword "IT")
  
  ;;return values - Y = index of matching item
  ;;                C = Set if not unique
  ;;                Z = Set if not found
  (when *word-table-built*
    ;;Current-place does not need to be marked as game state as it will
    ;;be set implicitly by a call to restore the game state
    ;;when there is a call to navigate made.
    (zp-b :current-place)
    
    (db :object1 0)
    (db :object2 0)
    
    (with-namespace :object-table
      (alias :noun :D0)
      (alias :adjective :D1)
      (alias :pos :A0)
      (alias :found-index :D2)
      (alias :word-index :D3)
      
      (game-state-bytes "It"
	(db :it 0))

      (when (resolves '(:parser . :words))
	(alias :words :A1)
	(label :parse-objects nil)
	(sta16.zp (resolve '(:parser . :words)) :words)
     	(LDY 0)
	(STY.AB :object1)
	(STY.AB :object2)
	(LDA 1)
	(STA.ZP :word-index)
	(label :next)
	(LDY.ZP :word-index)
	(CPY 6)
	(BGE :done)
	(JSR :find-object)
	(LDA.AB :object1)
	(BNE :set-object2)
	(STY.AB :object1)
	(BEQ :next)
	(label :set-object2)
	(STY.AB :object2)
	(CPY 0 "Keep searching for object 2")
	(BEQ :next)
	(label :done)
	(dc "Unless there were two words")
	(dc "Set 'it' to be object one")
	(CLC "Clear carry to indicate not a duplicate")
	(LDA.AB :object2)
	(BNE :clear-it)
	(LDA.AB :object1)
	(STA.AB :it)
	(RTS)
	(label :clear-it)
	(LDA 0)
	(STA.AB :it)
	(RTS)
	(label :find-object)
	(INY)
	(LDA.IZY :words)
	(BEQ :try-without-adjective)
	(STA.ZP :noun)
	(DEY)
	(LDA.IZY :words)
	(STA.ZP :adjective)
	(JSR :find-object-index)
	(BCS :duplicate)
	(BEQ :try-without-adjective)
	(INC.ZP :word-index)
	(INC.ZP :word-index)
	(RTS)
	(label :try-without-adjective)
	(LDY.ZP :word-index)
	(LDA.IZY :words)
	(STA.ZP :noun)
	(LDA 0)
	(STA.ZP :adjective)
	(JSR :find-object-index)
	(BCS :duplicate)
	(INC.ZP :word-index)
	(RTS)
	(label :duplicate)
	(LDY 0)
	(STY.AB :it)
	(STY.AB :object1)
	(STY.AB :object2)
	(PLA)
	(PLA)
	(RTS))

      (label :is-it)
      (dc "The word is IT")
      (LDY.AB :it)
      (dc "TODO- set a flag to indicate IT was not defined.")
      (BEQ :dont-have-it)
      (label :it-exists)
      (LDA.ABY (1- (resolve :places)))
      (CMP 1)
      (BEQ :have-it "IT is in the inventory")
      (CMP.ZP :current-place)
      (BEQ :have-it "IT is here")
      (label :dont-have-it)
      (LDY 0)
      (SEC "Pretend that IT is a duplicate, triggering message to be more specific.")
      (RTS)
      (label :have-it)
      (CLC)
      (RTS)
      
      (label :find-object-index nil)
      (dc "Linear search for the noun")
      (LDA.ZP :noun)
      (CMP (word-id 'IT))
      (BEQ :is-it)
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
      (dc "Dereference into the object index table")
      (LDX.ABY (1- (resolve :object-index)))
      (dc "Now check it is in our place")
      (LDA.ABX (1- (resolve :places)))
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
      (STX.ZP :found-index)
      (BEQ :next-noun)
      (label :already-found)
      (dc "Carry AND not-zero, i.e. duplicate AND found")
      (SEC)
      (RTS)
      (let ((objects nil)
	    (names nil))
	(clrhash *object-name->index*)
	(let ((index 1))
	  (do-hash-values (data *object-name->data*)
	    (push data objects)
	    (dolist (name (first data))
	      ;;add all names for objects to a list
	      (multiple-value-bind (noun adjective)
		  (split name)
		;;put in global lookup so we can find it later
		(setf (gethash name *object-name->index*) index)
		;;also push into a list so we can make a lookup table in code
		(push (list (gethash noun *word->meaning*)
			    (if adjective
				(gethash adjective *word->meaning*)
				0)
			    index)
		      names)))
	    (incf index)))
	
	;;reverse the objects as they were pushed in
	(setf objects (nreverse objects))
	;;now sort as the searching algorithm expects them to be in
	;;noun-id order
	(setf names (stable-sort (sort names #'< :key #'second)
				 #'< :key #'first))
	(apply #'db :names (mapcar #'first names))
	(apply #'db :adjectives (mapcar #'second names))
	(apply #'db :object-index (mapcar #'third names))
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
	(let ((places (mapcar
		       #'(lambda (o)
			   (aif (gethash (fourth o) *place->id*)
				it
				(assert nil nil "Place ~a was not defined" (fourth o))))
		       objects)))
	  (game-state-bytes "Object Places"
	    (apply #'db :places places))
	  (apply #'db :initial-places places))
	;; object name strings
	(hilo-table :name-hi :name-lo (mapcar #'(lambda (o) (nil->0 (second o))) objects))
	;; object verb handlers (a table of vtables)
	(labels ((verb-addr (o)
		   (let ((name (caar o)))
		     (if (gethash name *object->vtable*) 
			 (cons :vtable name)
			 0))))
	  (hilo-table :verb-hi :verb-lo (mapcar #'(lambda (o) (verb-addr o)) objects)))
	;; object descriptions
	(hilo-table :description-hi :description-lo (mapcar #'third objects))
	;; object properties
	(apply #'db :properties
	       (mapcar #'(lambda (o) (object-properties (fifth o) (sixth o))) objects))

	(maphash #'(lambda (object verb-handlers)
		     (label object :vtable)
		     (dolist (verb-handler verb-handlers)
		       (dc (format nil "~a" (car verb-handler)) t)
		       (db nil (word-id (car verb-handler)))
		       (dw nil (cdr verb-handler)))
		     (db nil 0))
		 *object->vtable*)))))

(defun dump-objects ()
  (do-hashtable (name data *object-name->data*)
    (format t "~3d ~20a $~4,'0x $~4,'0x ~20a TAKE:~a SHOW:~a ~s~%"
	    (object-id name)
	    name
	    (second data)
	    (third data)
	    (fourth data)
	    (if (fifth data) "Y" "N")
	    (if (sixth data) "Y" "N")
	    (cdar data))))

(defun dump-places ()
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) *place->id*))

(defun place-id (place)
  "Get the id for a place, e.g. :dungeon"
  (let ((id (gethash place *place->id*)))
    (when *compiler-final-pass*
      (assert id (place) "~a is not a valid place" place))
    (if id id 0)))

(defun object-place-address (name)
  "Get the object place address"
  (+ -1 (resolve '(:object-table . :places)) (object-id name)))

;;test finding object id

(defun test-object-definitions ()
  (defplace :ur)
  (defplace :nippur)
  (defplace :babylon)

  (object "MARDUK STATUE" (:description "A bronze statue" :place :ur))
  (object "STONE STATUE" (:description "A stone statue" :place :ur))
  (object "GINGER BISCUIT" (:description "A tasty snack" :place :ur))
  (object "ENTRAILS" (:description "Animal guts" :place :nippur))
  (object "POCKET FLUFF" (:description "Lovely pocket fluff" :place :inventory))
  (object "OBSIDIAN CUBE" (:description "Black cube" :place :nowhere))
  (object "CAT FLUFF" (:description "Cat fluff" :place :babylon))
  (object "GREEN BALL" (:description "Green ball" :place :ur))
  (object "RED BALL" (:description "Red ball" :place :ur))
  (object '("FINGER BONE" "BONE FINGER") (:description "A bony finger" :place :inventory)))

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
(test-object-find nil "BONE" :inventory :found :unique)
(test-object-find nil "FINGER" :inventory :found :unique)
(test-object-find "FINGER" "BONE" :inventory :found :unique)
(test-object-find "BONE" "FINGER" :inventory :found :unique)

(test-object-find nil "BALL" :ur :found :duplicate)

(assert (= (object-id "FINGER BONE")
	   (object-id "BONE FINGER")))

(defun parse-objects-tester (input &key (break-on 'brk))
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
	   (LDA (nil->0 (gethash :ur *place->id*)))
	   (STA.ZP :current-place)
	   (JSR :parse)
	   (JSR :parse-objects)
	   (BRK)
	   (test-object-definitions)
	   (object-table)
	   (parser)
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
    (pass))
  
  ;; install the string into the input buffer
  
  (loop for c across input
        for i from 0 to *max-input-length* do       
       (setf (aref *compiler-buffer* (+ i (resolve '(:parser . :input))))
	     (to-alphabet-pos c)))
  
  (monitor-reset #x600)
  (monitor-run :print nil :break-on break-on)
  
  (coerce (subseq (monitor-buffer)
		  (resolve :object1)
		  (1+ (resolve :object2)))
	  'list))

(defun test-parse-objects (input object1 object2)
  ;;(format t "Testing ~a~%" input)
  (let ((objects (parse-objects-tester input)))
    (when object1 (assert (equal (object-id object1) (first objects))))
    (when object2 (assert (equal (object-id object2) (second objects))))))

(test-parse-objects "TAKE MARDUK STATUE" "MARDUK STATUE" nil)
(test-parse-objects "TAKE BISCUIT" "GINGER BISCUIT" nil)
(test-parse-objects "HIT MARDUK STATUE STONE STATUE" "MARDUK STATUE" "STONE STATUE")
(test-parse-objects "HIT MARDUK STATUE GINGER BISCUIT" "MARDUK STATUE" "GINGER BISCUIT")
(test-parse-objects "HIT MARDUK STATUE BISCUIT" "MARDUK STATUE" "GINGER BISCUIT")
(test-parse-objects "HIT BISCUIT MARDUK STATUE" "GINGER BISCUIT" "MARDUK STATUE")
(test-parse-objects "HIT FLUFF BISCUIT" "POCKET FLUFF" "GINGER BISCUIT")
(test-parse-objects "HIT MARDUK STATUE WITH GINGER BISCUIT" "MARDUK STATUE" "GINGER BISCUIT")
(test-parse-objects "HIT MARDUK STATUE WITH BISCUIT" "MARDUK STATUE" "GINGER BISCUIT")
(test-parse-objects "HIT BISCUIT WITH MARDUK STATUE" "GINGER BISCUIT" "MARDUK STATUE")
(test-parse-objects "HIT FLUFF WITH BISCUIT" "POCKET FLUFF" "GINGER BISCUIT")
