;; Actions and handler that should be common across
;; games, e.g. INVENTORY, EXAMINE etc

;; TODO When there is a duplicate object, one of which is in the
;; inventory, there is no point in saying "Be more specific"
;; we should only find the objects which are in the inventory

;; Test-game stuff also in here

(defparameter *be-more-specific* "You'll have to be more specific...")
  
(defun generic-generic-handlers ()

  (fixture "IT" (:place :inventory))
  
  (with-location :generic
    (defword :INVENTORY :I)

    (custom-action '(WHAT)
      (with-namespace :what
	(dc "What is IT?")
	(LDY.AB '(:object-table . :it))
	(BNE :something)
	(respond-raw "Nothing.")
	(RTS)
	(label :something)
	(dc "Now print the object name")
	(LDA.ABY (1- (resolve '(:object-table . :name-hi))))
	(LDX.ABY (1- (resolve '(:object-table . :name-lo))))
	(JMP :print-message)))
    
    (custom-action '(LOOK)
      (with-namespace :look
	(respond-raw "You take a look around and see...")
	(LDA *object-show*)
	(dc "Do NOT show objects that do not want to be listed")
	(STA.AB '(:inventory . :object-property-mask))
	(LDA.ZP :current-place)
	(JMP '(:inventory . :scan-objects))))

    (custom-action '(INVENTORY)
      (with-namespace :inventory
	(respond-raw "You have...")
	(RTS) ;;NOCOMMIT
	(LDA 255)
	(dc "For inventory, show everything")
	(STA.AB :object-property-mask)
	(LDA 1 "Inventory is Place 1")
	(label :scan-objects)
	(dc "Reset matching object count to 0")
	(LDX 0)
	(STX.AB :object-count)
	;;Assume that IT is the last entry in the list
	(assert (string= (cadar (sort (mapcar #'(lambda (v) (list (object-id (caar v)) (caar v)))
					      (hash-values *object-name->data*)) #'> :key #'first)) "IT") nil "IT was not the last thing in the object list")
	(LDY (1- (hash-table-count *object-name->data*)))
	(label :next-object)
	(dc "List the object in the place in A")
	(dc "Look in one-based object places table")
	(PHA)
	(CMP.ABY (1- (resolve '(:object-table . :places))))
	(BNE :object-not-here)
	(dc "Get object properties")
	(LDA.ABY (1- (resolve '(:object-table . :properties))))
	(dc "Apply a mask to filter out undesirables, self modifying.")
	(label+1 :object-property-mask)
	(AND.IMM 0)
	(BEQ :object-not-here)
	(dc "Save object index")
        (STY.AB '(:object-table . :it))
	(dc "Now print the object name")
	(LDA.ABY (1- (resolve '(:object-table . :name-hi))))
	(LDX.ABY (1- (resolve '(:object-table . :name-lo))))
	(JSR :print-message)
	(dc "Restore Y and A, i.e. IT and object-count")
	(LDY.AB '(:object-table . :it))
	(INC.AB :object-count)
	(label :object-not-here)
	(PLA)
	(DEY)
	(BNE :next-object)
	(LDX.AB :object-count)
	(BNE :not-empty)
	(respond-raw "Nothing.")
	(label :not-empty)
	(CPX 1)
	(BEQ :unique)
	(LDA 0 "Clear It")
	(STA.AB '(:object-table . :it))
	(label :unique)
	(RTS)
	(dc "Temporary counter when scanning objects")
	(db :object-count 0)))

    ;; Note, generic verbs are implemented in 6502 rather than VM
    
    (with-namespace :take
      (label :doit)
      (dc "Do we already have this?")
      (LDA 1)
      (CMP.ABY (1- (resolve '(:object-table . :places))))
      (BNE :can-take-it?)
      (respond-raw "You already have that.")
      (RTS)
      (label :can-take-it?)
      (LDA *object-take*)
      (AND.ABY (1- (resolve '(:object-table . :properties))))
      (BNE :take-it)
      (respond-raw "You can't take that.")
      (RTS)
      (label :take-it)
      (dc "Set the place to inventory")
      (STA.ABY (1- (resolve '(:object-table . :places))))
      (respond-raw "You took it!")
      (RTS))

    (with-namespace :drop
      (label :doit)
      (dc "Check we have this object about our person")
      (LDA 1)
      (CMP.ABY (1- (resolve '(:object-table . :places))))
      (BNE :do-not-have)
      (label :drop-it)
      (dc "Now set its place to the current place")
      (LDA.ZP :current-place)
      (STA.ABY (1- (resolve '(:object-table . :places))))
      (respond-raw "You dropped it!")
      (RTS)
      (label :do-not-have)
      (respond-raw "You don't have that.")
      (RTS))

    (with-namespace :examine
      (label :doit)
      (dc "Print the description")
      (LDA.ABY (1- (resolve '(:object-table . :description-hi))))
      (LDX.ABY (1- (resolve '(:object-table . :description-lo))))
      (JSR :print-message)
      (RTS))

    (with-namespace :verb-handler
      (label :generic-vtable)
      (flet ((entry (verb ns);
	       ;;TODO Make defword idempotent and remove this check
	       (unless (gethash (symbol-name verb) *word->meaning*)
		 (defword verb))
	       (dc (format nil "~a" verb))
	       (db nil (word-id verb))
	       (dw nil (cons ns :doit))))
	(entry 'EXAMINE :examine)
	(entry 'TAKE :take)
	(entry 'DROP :drop)))

    (custom-action '(? ? ? ?)
      (with-namespace :verb-handler
	(alias :vtable :A0)
	(alias :object-id :D0)
	(LDA.AB :word-count)
	(CMP 2)
	(BLT :not-verb-object)	
	(LDA.AB '(:parser . :words))
	(BNE :valid-ish-verb)
	(respond-raw "I don't know that word.")
	(label :not-verb-object)
	(RTS)
	(label :valid-ish-verb)
	(JSR :parse-objects)
	(BCS :duplicate-found)
	(LDY.AB :object1)
	(BEQ :not-found)
	(STY.ZP :object-id)
	(dc "Look for the verb handler for an object")
	(LDX 2 "Two goes, object specific then generic")
	(LDA.ABY (1- (resolve '(:object-table . :verb-hi))))
	(BEQ :nope "No handlers are on the zero page, so this means not set")
	(STA.ZP (hi-add :vtable))
	(LDA.ABY (1- (resolve '(:object-table . :verb-lo))))
	(STA.ZP (lo-add :vtable))
	(dc "Now look for the entry that corresponds to our verb")
	(label :scan-vtable)
	(LDY 0)
	(label :next)
	(LDA.IZY :vtable)
	(BEQ :nope)
	(CMP.AB '(:parser . :words))
	(BEQ :found)
	;;TODO EARLY RETURN IF THE VERBS ARE SORTED
	;;BY WORD ID WE CAN QUIT EARLY IF WE GO PAST IT
	(dc "Skip to next, three bytes per entry")
	(INY)
	(INY)
	(INY)
	(BNE :next)
	(label :nope)
	(dc "Once more, with the generic vtable?")
	(DEX)
	(BEQ :definitely-nope)
	(LDA (lo :generic-vtable))
	(STA.ZP (lo-add :vtable))
	(LDA (hi :generic-vtable))
	(STA.ZP (hi-add :vtable))
	(BNE :scan-vtable)
	(label :definitely-nope)
	(respond-raw "You can't do that to it!")
	(RTS)
	(label :duplicate-found)
	(respond-raw *be-more-specific*)
	(RTS)	
	(label :found)
	(dc "Call the VM with the address of the handler")
	(INY)
	(LDA.IZY :vtable)
	(STA.ZP (lo-add :vm-pc))
	(INY)
	(LDA.IZY :vtable)
	(STA.ZP (hi-add :vm-pc))
        (TXA)
	(CMP 1 "Generic handler?")
	(BNE :vm-handler)
	(LDY.ZP :object-id "Generic handler might quite like to know what object it is")
	(JMP.IND :vm-pc "Generic handler is 6502")
	(label :vm-handler)
	(JMP :vm-go "Execute VM code and return to grandparent")
	(label :not-found)
	(respond-raw "I don't see that.")
	(RTS)))))

(defun test-render-input ()
  (label :test-render-input)
  (with-namespace :test-render-input
    ;;this simulates the user having already entered the text on the bottom
    ;;row and pressing return. The existing text in the 4 lines above is
    ;;scrolled up and the text is posted on line 4 (not five, which remains empty)
    (JSR :scroll)
    (sta16.zp (scradd (live-row 3) 0) '(:typeset . :raster))
    (alias :pos :D4)
    (alias :str :A4)
    (label :typeset-is nil)
    (LDY 0)
    (STY.ZP '(:typeset . :shift))
    (STY.ZP '(:typeset . :prev-width))
    (sta16.zp '(:parser . :input) :str)
    (label :next)
    (STY.ZP :pos)
    (LDA.IZY :str)
    (BNE :not-space)
    (LDA (lo '(:present . #\ )))
    (STA.ZP (lo-add '(:typeset . :char)))
    (LDA (hi '(:present . #\ )))
    (STA.ZP (hi-add '(:typeset . :char)))
    (JMP :emit)
    (label :not-space)
    (TAX)
    (LDA.ABX (1- (resolve ':1ch-lo)))
    (CLC)
    (dc "Add the offset of the font")
    (ADC.ZP (lo-add :font))
    (STA.ZP (lo-add '(:typeset . :char)))	
    (LDA.ABX (1- (resolve ':1ch-hi)))
    (ADC.ZP (hi-add :font))
    (STA.ZP (hi-add '(:typeset . :char)))
    (label :emit)
    (JSR :typeset)
    (LDY.ZP :pos)
    (INY)
    (CPY *max-input-length*)
    (BNE :next)
    (RTS)

    ;; We shouldn't need this table really, once the compressed
    ;; string table contains all the letters.
    
    (let ((lo (list :1ch-lo))
	  (hi (list :1ch-hi)))
      (loop for c across "ABCDEFGHIJKLMNOPQRSTUVWXYZ" do
	   (let ((offset (- (resolve (cons :present c))
			    (resolve '(:font . :present)))))
	     (push (lo offset) lo)
	     (push (hi offset) hi)))
      (apply #'db (nreverse lo))
      (apply #'db (nreverse hi)))))

;;TODO Move this into slime-interface.lisp

(defun enter-input (str &key (break-on 'brk) (print t))
  ;;really need to make better 6502 emulator to avoid need
  ;;for copying the buffer
  (let ((buffer (cl-6502:get-range 0)))
    ;;we must clear the buffer for new input
    (loop for i from 0 to *max-input-length* do
	 (setf (aref buffer (+ i (resolve '(:parser . :input)))) 0))
    
    (loop for c across str
       for i from 0 to *max-input-length* do       
	 (setf (aref buffer (+ i (resolve '(:parser . :input))))
	       (to-alphabet-pos c)))
    (setf (cl-6502:get-range 0) buffer))
  (monitor-setpc :test-input)
  (monitor-run :break-on break-on :print (and print
					      (eq break-on 'brk)))

  (when print
    (unless (eq break-on 'brk)
      (monitor-go)))
  (setmem-copy (monitor-buffer))
  (when print
    (dump-state-base64)))

(defun gogogo ()
  (princ "q to quit")
  (terpri)
  (tagbody
   :top
     (let ((line (read-line)))
       (if (equalp line "q")
	   (return-from gogogo))
       (enter-input line :print nil)
       (go :top))))

(defun restore-game (str &key (break-on 'brk))
  (restore-state-base64 str)
  (monitor-setpc :restore-game)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer))
  (enter-input "LOOK")
  (values))
  
