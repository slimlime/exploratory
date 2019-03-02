;; Hybrid approach. Binary search on table of 4 character words
;; any collisions resolved with a binary tree.

(defparameter *word-id-count* nil)
(defparameter *word->meaning* nil)
(defparameter *meaning->word* nil)
(defparameter *word-collisions* nil)
(defparameter *max-input-length* 35)
;;this following parameter is boring because it just suppresses the
;;defword assertion that a word not already be in the table.
;;and so needs to be set at the end of the build pass.
(defparameter *word-table-built* nil)
(defparameter *binary-search-table* nil) ;;for debugging
(defparameter *max-input-words* 8)

(defun reset-parser ()
  (setf *word-table-built* nil)
  (setf *word->meaning* (make-hash-table :test 'equal))
  (setf *meaning->word* (make-hash-table))
  (setf *word-id-count* 1)
  (setf *word-collisions* nil)
  (setf *binary-search-table* nil))

;;TODO defword should be idempotent
(defun defword (word &rest synonyms)
  ;;don't do anything once the table has already been built
  (unless *word-table-built*
    (unless (stringp word) (setf word (symbol-name word)))
    (assert (null (gethash word *word->meaning*)) nil (format nil "~a already in dictionary" word))
    (setf (gethash word *word->meaning*) *word-id-count*)
    (setf (gethash *word-id-count* *meaning->word*) word) 
    (dolist (synonym synonyms)
      (unless (stringp synonym) (setf synonym (symbol-name synonym)))
      (assert (null (gethash synonym *word->meaning*)))
      (setf (gethash synonym *word->meaning*) *word-id-count*))
    (incf *word-id-count*)))

(defun word-id (str &optional (assert t))
  (let ((id (gethash (symbol-name str) *word->meaning*)))
    (when (and assert *compiler-final-pass*)
      (assert id nil (format nil "~a is not defined" str)))
    (nil->0 id)))
     
(defun to-alphabet-pos (char)
  (let ((code (1+ (- (char-code (char-upcase char))
		     (char-code #\A)))))
    (when (or (> code 26)
	      (< code 0))
      (setf code 0))
    code))
    
;;Define an string where the only characters allowed are
;;SPC = 0
;;  A = 1
;;  B = 2
(defun dis (label string)
  (add-hint (1+ (length string))
	    (format nil "DIS ~s" string))
  (when label (label label))
  (loop for c across string do
       (push-byte (to-alphabet-pos c)))
  (push-byte 0))

;;Binary tree parser to disambiguate words which collide
;;when hashed. Expects A0 to contain the input buffer and
;;Y to contain the offset.
(defun binary-parser (wordlist)
  (label :binary-parser)
  (with-namespace :binary-parser
    (alias :word :A0)
    (unless wordlist
      (RTS)
      (return-from binary-parser))
    (let ((words (coerce wordlist 'vector))
	  (label-number 1))
      (setf words (sort words #'string<))
      (dc (format nil "~a" words))
      (labels ((range-or-word (i j)
		 (if (= i j)
		     (format nil "~a" (elt words i))
		     (format nil "~a...~a"
			     (elt words i)
			     (elt words j))))
	       (letter (i k)
		 (let ((w (elt words i)))
		   (if (>= k (length w))
		       #\ 
		       (elt w k))))
	       (split (i j k)
		 (let ((pos nil)
		       (diff 99999))
		   (loop for p from (1+ i) to j do
			(when (not (eq (letter (1- p) k)
				       (letter p k)))
			  (let ((d (abs (- (- p i)
					   (1+ (- j p))))))
			    (when (< d diff)
			      (setf diff d)
			      (setf pos (1- p))))))
		   pos))
	       (generate (i j k y)
		 (if (= i j)
		     (let ((id (gethash (elt words i) *word->meaning*))) 
		       (LDA id (format nil "~a (~a)"
				       (elt words i)
				       (gethash id *meaning->word*)))
		       (RTS))
		     (let ((split (split i j k)))
		       (if (null split)
			   (generate i j (1+ k) y)
			   (progn
			     (dc (format nil "~a or ~a?"
					 (range-or-word i split)
					 (range-or-word (1+ split) j)))
			     (unless (= k y)
			       (dotimes (_ (- k y))
				 (INY))
			       (LDA.IZY :word)
			       (setf y k))
			     (CMP (1+ (to-alphabet-pos (letter split k)))
				  (format nil "[~a] >= ~a" k
					  (code-char 
					   (1+ (char-code (letter split k))))))
			     (let ((label (write-to-string (incf label-number))))
			       (BCS label)
			       (generate i split k y)
			       (label label)
			       (generate (1+ split) j k y))))))))
	(LDA.IZY :word)
	(generate 0 (1- (length words)) 0 0)))))

(defun char-or-space (string index)
  (if (>= index (length string))
      0
      (to-alphabet-pos (char string index))))

(defun collides-p (a b)
  (and (>= (length a) 4)
       (>= (length b) 4)
       (equal (subseq a 0 4)
	      (subseq b 0 4))
       (not (= (gethash a *word->meaning*)
	       (gethash b *word->meaning*)))))

(defun parser ()
  (db :word-count 0)
  (label :parse)
  (with-namespace :parser
    (alias :word-start :D0)
    (alias :delta :D1)
    
    (alias :inp :A0)
    (alias :index :D3) ;;binary search array index

    (sta16.zp :input :inp)
    (label :parse-direct)
    (LDX 0 "X is our word pointer")
    (STX.AB :word-count)
    (dc "Clear the parsed words buffer")
    (dotimes (i *max-input-words*)
      (STX.AB (+ (resolve :words) i)))
    (LDY #xff)
    (label :trim)
    (INY)
    (LDA.IZY :inp)
    (BEQ :trim)
    (label :next-word)
    (CPY *max-input-length*)
    (BGE :done)
    (dc "Now start a binary search")
    (dc "but first save word index in case")
    (dc "we need to resolve a collision")
    (STY.ZP :word-start)
    (LDA (ash 1 7) "Search depth")
    (STA.ZP :delta)
    (label :next-entry)
    (LDY.ZP :word-start)
    (STA.ZP :index)
    (TAX)
    (LDA.IZY :inp)
    (CMP.ABX (1- (resolve :tbl1)) "tbl1 - 1")
    (BNE :not-match)
    
    (INY)
    (LDA.IZY :inp)
    (CMP.ABX (1- (resolve :tbl2)) "tbl2 - 1")
    (BNE :not-match)
    (dc "Might be a one letter word")
    (CMP 0 "Check for space")
    (BEQ :found)
    
    (INY)
    (LDA.IZY :inp)
    (CMP.ABX (1- (resolve :tbl3)) "tbl3 - 1")

    (BNE :not-match)
    (dc "Might be a two letter word")
    (CMP 0 "Check for space")
    (BEQ :found)

    (INY)
    (LDA.IZY :inp)
    (CMP.ABX (1- (resolve :tbl4)) "tbl4 - 1")
    (BEQ :found)
    
    (label :not-match)
    (BGE :gt)
    (dc "Less than...")
    (LSR.ZP :delta)
    (BEQ :not-found)
    (LDA.ZP :index)
    (SEC)
    (SBC.ZP :delta)
    (dc "Always jump... A != 0 since 128 - 64 ... - 1 = 1")
    (BNE :next-entry)
    (label :gt)
    (dc "Greater than...")
    (LSR.ZP :delta)
    (BEQ :not-found)
    (LDA.ZP :index)
    (CLC)
    (ADC.ZP :delta)
    (BNE :next-entry)
    (label :found)
    (dc "The word is in our dictionary")
    (dc "Let us look up its meaning")
    (LDX.ZP :index)
    (LDA.ABX (1- (resolve :word-meanings)) "Look up word meaning, 1 based")
    (BNE :store-result)
    
    (dc "Colliding word, lets have a go with")
    (dc "the binary parser to resolve it")
    (LDY.ZP :word-start)
    (JSR :binary-parser)
    (dc "Since the binary parser isn't too fussy about")
    (dc "where it stops, rewind back to the beginning")
    (dc "of the word before we look for the next space")
    (LDY.ZP :word-start)
    (JMP :store-result) ;;TODO, we can use SKIP trick for this
			;;e.g. (skip-to :not-found) which could assert if not 2 bytes
    (label :not-found)
    (LDA 0)
    (label :store-result)
    (LDX.AB :word-count)
    (STA.ABX :words)
    (dc "Advance to the next space")
    (label :seek-space)
    (LDA.IZY :inp)
    (BEQ :consume-space)
    (INY)
    (BNE :seek-space)
    (label :consume-space)
    (INY)
    (LDA.IZY :inp)
    (BEQ :consume-space)
    (label :found-next)
    (INX)
    (STX.AB :word-count)
    (CPX *max-input-words*)
    (BNE :next-word)
    (label :done)
    (RTS)

    ;;TODO Ensure that only real collisions are added, e.g. CHADRIC and CHADRIX
    ;;are synonymous, but CHEETOWS and CHEEZOWS are not.
    (dc "The parsed word meanings get put here")
    (dbs :words *max-input-words*)
    (dc "The user input buffer")
    (dc "Terminated with many zeroes for convenience.")
    (dbs :input (+ *max-input-words* *max-input-length*))
    
    (let ((strings nil)
	  (collisions (make-hash-table :test 'equal))
	  (string-count nil))
      
      (let ((all-strings nil))
	(maphash (λ (k v)
		     (declare (ignorable v))
		     (push k all-strings))
		 *word->meaning*)
	(setf all-strings (sort all-strings #'string<))
	(dolist (string all-strings)
	  (if (collides-p string (car strings))
	      (progn
		(setf (gethash string collisions) t)
		(setf (gethash (car strings) collisions) t))
	      (push string strings)))
	(setf string-count (length strings))
	(assert (< string-count 256) nil "Too many words")
	(dotimes (_ (- 255 string-count))
	  (push "ZZZZ" strings))
	(setf strings (nreverse strings))
	(assert (= (length strings) 255) nil "Dodgy array length"))

      (setf *binary-search-table* strings)
            
      (dc "Word tables, by character position")
	  
      (apply #'db :tbl1 (mapcar (λ (s) (char-or-space s 0)) strings))
      (apply #'db :tbl2 (mapcar (λ (s) (char-or-space s 1)) strings))
      (apply #'db :tbl3 (mapcar (λ (s) (char-or-space s 2)) strings))
      (apply #'db :tbl4 (mapcar (λ (s) (char-or-space s 3)) strings))
      
      (dc "Table of word meanings")

      (let ((meanings nil))
	(dotimes (_ string-count)
	  (let ((string (pop strings)))
	    (push (if (gethash string collisions)
		      0
		      (gethash string *word->meaning*))
		  meanings)))
	(apply 'db :word-meanings (nreverse meanings)))
	  
      (dc "Binary tree parser")

      (setf *word-collisions* nil)

      (maphash (λ (k v) (declare (ignore v))
			 (push k *word-collisions*))
	       collisions)
      
      (binary-parser *word-collisions*))))
  
(defun dump-words ()
  (let ((i 0)
	(sorted nil))
    (maphash (λ (k v)
		 (push (cons k v) sorted)
		 (format t "~a=~a " k v)
		 (when (zerop (mod (incf i) 5))
		   (terpri)))
	     *word->meaning*)
    (setf sorted (sort sorted #'string< :key #'car))
    (print sorted))
  (values))

(defun build-parse-word-test (pass)
  (funcall pass)
  (setf *word-table-built* t)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defun parse-word-test (word)
  (reset-compiler)
  (reset-strings)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)

	   (label :start)
	   
	   (sta16.zp :word '(:parser . :inp))
	   
	   (JSR '(:parser . :parse-direct))

	   (BRK)
	   
	   (dis :word word)
	   (db nil 0 0 0 0 0)
	   
	   (parser)
	   
	   (label :end)))
    
    (build-parse-word-test #'pass)
    
    ;;(dump-words)
    
    (monitor-reset #x600)
    (monitor-run :print nil)
    
    (aref (monitor-buffer) (resolve '(:parser . :words)))))

(defun test-parse-word (word meaning &key (debug nil))
  (when debug (format t "Checking ~a~%" word))
  (unless    
      (= (parse-word-test word) meaning)
    (let ((msg (format debug "The word ~a parsed to the wrong value" word)))
      (if debug
	  (terpri)
	  (assert nil nil msg)))))
  
;; Test all words in the list

(reset-parser)

(defword :PRESS :PUSH :PROD :POKE)
(defword :GET :PICK :TAKE)
(defword :OPEN)
(defword :CLOSE :SHUT :SLAM)
(defword :SAY :SPEAK :TELL :ASK)
(defword :UP :CLIMB)
(defword :DOWN :DESCEND)
(defword :OPERATE :USE)
(defword :CYLINDER)
(defword :DOOR)
(defword :DOG)
(defword :PORSCHE :CAR)
(defword :STAIRS)
(defword :MAXINE :SISTER :GIRL :WOMAN :LADY)
(defword :TABLE)
(defword :EXAMINE :LOOK :INSPECT)
(defword :IN :ENTER)
(defword :CHAIR :SEAT)
(defword :CHEEZOWS)
(defword :COLA)
(defword :COKE) ;;I don't know what that is...
(defword :WOTSITS :CHEETOS)
(defword :DICE)
(defword :FIGURES)
(defword :OPENER :REMOTE :CONTROL)
(defword :KEYS :KEY)
(defword :WINDOW)
(defword :WHEEL :TIRE :TYRE)
(defword :STEED :HORSE :STALLION)
(defword :SWORD :WEAPON)
(defword :SHIELD)
(defword :LIGHT :BULB)
(defword :N :NORTH)
(defword :S :SOUTH)
(defword :E :EAST)
(defword :W :WEST)
(defword :CHAD :CHADRIC :CHADRIX :IMPERATOR :LORD)
(defword :I :INVENTORY)
(defword :DROP)
(defword :WITH :USING)

(maphash (λ (k v) (test-parse-word k v :debug nil)) *word->meaning*)

(test-parse-word " OPEN" (gethash "OPEN" *word->meaning*))

(flet ((check (item)
	 (find item *word-collisions* :test 'equal)))

  (assert (check "OPEN"))
  (assert (check "OPENER"))
  (assert (check "CHEETOS"))
  (assert (check "CHEEZOWS"))

  ;; The following words should not be considered collisions
  ;; as they resolve to the same meaning.

  (assert (null (check "CHADRIX")))
  (assert (null (check "CHAD")))
  (assert (null (check "CHADRIC"))))

(defun parse-words-tester (input &key (break-on 'brk))
  (reset-compiler)
  (reset-strings)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (label :start)
	   (JSR :parse)
	   (BRK)
	   (parser)
	   (label :end)))
    
    (build-parse-word-test #'pass))

  ;; install the string into the input buffer

  (loop for c across input
        for i from 0 to *max-input-length* do       
       (setf (aref *compiler-buffer* (+ i (resolve '(:parser . :input))))
	     (to-alphabet-pos c)))
  
  (monitor-reset #x600)
  (monitor-run :print nil :break-on break-on)
  
  (coerce (subseq (monitor-buffer)
		  (resolve '(:parser . :words))
		  (+ (resolve '(:parser . :words)) *max-input-words*))
	  'list))

(defun test-parse-input (count input expected)
  ;;(format t "Testing ~a~%" input)
  (let ((words (parse-words-tester input)))
    ;;(print words)
    (assert (= count (aref (monitor-buffer) (resolve :word-count))))
    (loop
       for e in expected
       for r in words do
	 (unless (eq '? e)
	   (assert (equal r (gethash (symbol-name e) *word->meaning*)))))
    ;;we must insist the parser stops parsing at the end of the string
    (loop for i from (length expected) to 2 do
	 (assert (equal 0 (elt words i)) nil
		 (format nil "Sentence [~a] parsed to [~a] which has phantom extra words at pos ~a"
			 input words i)))))

(test-parse-input 0 "" '())
(test-parse-input 1 "CHAIR" '(CHAIR))
(test-parse-input 2 "OPEN DOOR" '(OPEN DOOR))
(test-parse-input 2 "PUSH CYLINDER" '(PUSH CYLINDER))
(test-parse-input 2 "PRESS CYLINDER" '(PUSH CYLINDER))
(test-parse-input 3 "OPEN FRABJOUS DOOR" '(OPEN ? DOOR))
(test-parse-input 3 "OPEN DOOR CLOSE" '(OPEN DOOR CLOSE))
(test-parse-input 2 " OPEN DOOR" '(OPEN DOOR))
(test-parse-input 2 "PUSH  CYLINDER" '(PUSH CYLINDER))
(test-parse-input 2 "PRESS   CYLINDER" '(PUSH CYLINDER))
(test-parse-input 3 "    OPEN  FRABJOUS  DOOR  " '(OPEN ? DOOR))
(test-parse-input 3 "OPEN     DOOR      CLOSE    " '(OPEN DOOR CLOSE))
(test-parse-input 4 "OPEN DOOR WITH CYLINDER" '(OPEN DOOR WITH CYLINDER))
(test-parse-input 6 "OPEN DOOR WITH CYLINDER IN HORSE"
		  '(OPEN DOOR WITH CYLINDER IN HORSE))

