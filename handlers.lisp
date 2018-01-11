;; Actions and handler that should be common across
;; games, e.g. INVENTORY, EXAMINE etc

;; Test-game stuff also in here

(defparameter *be-more-specific* "You'll have to be more specific...")

(defun generic-generic-handlers ()
  (with-location :generic

    (action '(TAKE)
      (with-namespace :take
	(JSR :find-object-index-from-input)
	(BCS :duplicate-found)
	(BEQ :not-found)
	(dc "Now set the place to inventory")
	(LDA 1)
	(CMP.ABY (1- (resolve '(:object-table . :places))))
	(BNE :take-it)
	(respond "You already have that.")
	(RTS)
	(label :take-it)
	(STA.ABY (1- (resolve '(:object-table . :places))))
	(respond "You took it!")
	(RTS)
	(label :duplicate-found)
	;;There is an 'edge' case here where you are already
	;;carrying the RED SWORD and you try to TAKE the BLUE SWORD
	;;by typing TAKE SWORD. The computer is going to earn
	;;it's reputation as a pedant here.
	(respond *be-more-specific*)
	(RTS)
	(label :not-found)
	(respond "Take what?")))

    (action '(DROP)
      (with-namespace :drop
	(JSR :find-object-index-from-input)
	(BCS :duplicate-found)
	(BEQ :not-found)
	(dc "Check we have this object about our person")
	(LDA 1)
	(CMP.ABY (1- (resolve '(:object-table . :places))))
	(BNE :not-found)
	(label :drop-it)
	(dc "Now set its place to the current place")
	(LDA.ZP :current-place)
	(STA.ABY (1- (resolve '(:object-table . :places))))
	(respond "You dropped it!")
	(RTS)
	(label :duplicate-found)
	(respond *be-more-specific*)
	(RTS)
	(label :not-found)
	(respond "Drop what?")))

    (defword :INVENTORY :I)

    (action '(LOOK)
      (with-namespace :look
	(respond "You take a look around and see...")
	(LDA.ZP :current-place)
	(JMP '(:inventory . :scan-objects))))

    (action '(INVENTORY)
      (with-namespace :inventory
	(respond "You have...")
	(LDA 1 "Inventory is Place 1")
	(label :scan-objects)
	(dc "Reset matching object count to 0")
	(LDX 0)
	(STX.AB :object-count)
	(LDY (objects-count))
	(label :next-object)
	(dc "List the object in the place in A")
	(dc "Look in one-based object places table")
	(CMP.ABY (1- (resolve '(:object-table . :places))))
	(BNE :object-not-here)
	(dc "Save A and Y")
	(PHA)
	(TYA)
	(PHA)
	(dc "Now print the object name")
	;;See below, this self-modifying code needs refactoring
	(LDA.ABY (1- (resolve '(:object-table . :name-hi))))
	(STA.AB :name-hi)
	(LDA.ABY (1- (resolve '(:object-table . :name-lo))))
	(STA.AB :name-lo)
	(JSR '(:print-message . 1))
	(DB :name-lo 0)
	(DB :name-hi 0)
	(dc "Restore Y and A")
	(PLA)
	(TAY)
	(PLA)
	(INC.AB :object-count)
	(label :object-not-here)
	(DEY)
	(BNE :next-object)
	(LDX.AB :object-count)
	(BNE :not-empty)
	(respond "Nothing.")
	(label :not-empty)
	(RTS)
	(dc "Temporary counter when scanning objects")
	(db :object-count 0)))

    (action '(EXAMINE)
      (with-namespace :examine
	(JSR :find-object-index-from-input)
	(BCS :duplicate-found)
	(BEQ :not-found)
	(dc "Now print the description")
	;; TODO provide an entry point for print-message which
	;; does not use self-modifying code
	(LDA.ABY (1- (resolve '(:object-table . :description-hi))))
	(STA.AB :description-hi)
	(LDA.ABY (1- (resolve '(:object-table . :description-lo))))
	(STA.AB :description-lo)
	(LDA.ABY (1- (resolve '(:object-table . :description-lines))))
	(JSR :print-message)
	(DB :description-lo 0)
	(DB :description-hi 0)
	(RTS)
	(label :duplicate-found)
	(respond *be-more-specific*)
	(RTS)
	(label :not-found)
	(respond "Examine what?")))

    ))

(defun test-render-input ()
  (label :test-render-input)
  (with-namespace :test-render-input
    ;;this simulates the user having already entered the text on the bottom
    ;;row and pressing return. The existing text in the 4 lines above is
    ;;scrolled up and the text is posted on line 4 (not five, which remains empty)
    (call-memcpy (scradd (live-row 1) 0)
		 (scradd (live-row 0) 0)
		 (* 4 +screen-width-bytes+ *line-height*))
    (call-memset 0 (scradd (live-row 3) 0)
		 (* 1 +screen-width-bytes+ *line-height*))
    (sta16.zp (scradd (live-row 3) 0) '(:typeset . :raster))
    (alias :tmp-raster :A3)
    (alias :pos :D4)
    (alias :str :A2)
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
  
(defun enter-input (str &key (break-on 'brk))
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
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer))
  ;;(vicky)
  (values))

(defun restore-game (str)
  (restore-state-base64 str)
  (monitor-setpc :restore-game)
  (monitor-run)
  (setmem-copy (monitor-buffer))
  (enter-input "LOOK")
  (values))
