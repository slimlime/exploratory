;; Hybrid approach. Put all words in a hash-table. Fill the hash-table
;; with the word meanings. For colliding hashes, use a discriminating
;; binary search.

(defparameter *word-id-count* nil)
(defparameter *word-ids* nil)
(defparameter *id-meanings* nil)
(defparameter *word-hash-table* (make-array 256))
(defparameter *hash-fudge-factors* nil)
(defparameter *word-collisions* nil)

(defparameter *max-input-length* 40)
(defparameter *max-words* 4)

(defun reset-parser ()
  (setf *word-ids* (make-hash-table :test 'equal))
  (setf *id-meanings* (make-hash-table))
  (setf *word-id-count* 1)
  (setf *word-hash-table* (make-array 256))
  (setf *hash-fudge-factors* nil))
  
(defun defword (word &rest synonyms)
  (setf word (symbol-name word))
  (assert (null (gethash word *word-ids*)))
  (setf (gethash word *word-ids*) *word-id-count*)
  (setf (gethash *word-id-count* *id-meanings*) word)
  (dolist (synonym synonyms)
    (setf synonym (symbol-name synonym))
    (assert (null (gethash synonym *word-ids*)))
    (setf (gethash synonym *word-ids*) *word-id-count*))
  (incf *word-id-count*))

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
(defun binary-parser ()
  (label :binary-parser)
  (with-namespace :binary-parser
    (alias :word :A0)
    (let ((words (coerce *word-collisions* 'vector))
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
		     (let* ((id (gethash (elt words i) *word-ids*))
			    (prim (gethash id *id-meanings*)))
			 (LDA id (format nil "~a" prim))
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

(reset-parser)
  
(defword :PRESS :PUSH :PROD :POKE)
(defword :GET :PICK :TAKE)
(defword :OPEN)
(defword :CLOSE :SHUT)
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

(defun hash (word a b c d)
  (flet ((g (i) (to-alphabet-pos (elt word i))))
    (let ((len (length word))
	  (h (+ (g 0) a)))
      (when (> len 1) (setf h (logxor (ash h 1) (+ (g 1) b))))
      (when (> len 2) (setf h (logxor (ash h 1) (+ (g 2) c))))
      (when (> len 3) (setf h (logxor (ash h 1) (+ (g 3) d))))
      (when (> len 4) (setf h (logxor h (- 128 (g 4)))))
      (logand #xff h))))

(defun build-table (a b c d)
  (let ((hashes (make-hash-table))
	(collisions (make-hash-table :test 'equal)))
    (maphash #'(lambda (k v)
		 (declare (ignorable v))
		 (let* ((h (hash k a b c d))
			(existing (gethash h hashes)))
		     ;;It's not a collision if the word maps to the same
		     ;;word id, for example, CHADRIX, CHADRIC, which
		     ;;is going to be quite common for words with 4 letters
		     ;;the same!
		   (if (and existing (/= (gethash existing *word-ids*)
					 (gethash k *word-ids*)))
		       (progn
			 (setf (gethash existing collisions) t)
			 (setf (gethash k collisions) t))
		       (setf (gethash h hashes) k))))
	     *word-ids*)
    (let ((collision-list nil))
      (maphash #'(lambda (k v)
		   (declare (ignorable v))
		   (push k collision-list))
	       collisions)
      (values hashes collision-list (hash-table-count collisions)))))

(defun count-collisions (a b c d)
  (multiple-value-bind (hashes collisions collision-count)
      (build-table a b c d)
    (declare (ignorable hashes collisions))
    collision-count))

(defun build-hash-table (&optional (quick t))
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((max 0)
	(min 9999)
	(r nil)
	(low (if quick 0 -16))
	(hi (if quick 0 16)))
    (loop for a from low to hi do
	 (loop for b from low to hi do
	      (loop for c from low to hi do
		   (loop for d from low to 255 do
			(let ((l (count-collisions a b c d)))
			  (when (> l max)
			    (setf max l))
			  (when (< l min)
			    (setf min l)
			    (setf r (list a b c d))))))))
    (format t "Collisions ~a~% (worst was ~a) Fudge Factors ~a~%" min max r)
    (multiple-value-bind (hashes collisions collision-count)
	(apply 'build-table r)
      (declare (ignorable collision-count))
      (format t "Collisions:~a" collisions)
      (setf *word-collisions* collisions)
      ;;fill the word meaning table with values
      (maphash #'(lambda (k v)
		   (setf (aref *word-hash-table* k)
			 (gethash v *word-ids*)))
	       hashes)
      ;;clear the colliding ones, just in case
      (loop for w in collisions do
	   (setf (aref *word-hash-table* (apply 'hash w r)) 0))
      (setf *hash-fudge-factors* (coerce r 'vector)))))

(defun test-hashing (w collision-p)
  (let ((hash-id (aref *word-hash-table* (apply 'hash w *hash-fudge-factors*)))
	(word-id (gethash w *word-ids*)))
    (if collision-p
	(assert (= hash-id 0))
	(assert (= hash-id word-id)))))

(defun parser ()
  (label :parse)
  (with-namespace :parser
    (alias :pos :D0)
    (alias :tmp :D1)
    (alias :word-start :D2)
    (alias :word-index :D3)
    (alias :inp :A0)
    ;;store our input address so we can use zp indexing
    (sta16.zp :input :inp)
    (label :parse-direct)
    (LDX 0 "X is our word pointer")
    (dc "Clear the parsed words buffer")
    (dotimes (i *max-words*)
      (STX.AB (+ (resolve :words) i)))
    (LDY #xff)
    (label :trim)
    (INY)
    (LDA.IZY :inp)
    (BEQ :trim)
    (label :next)
    (STY.ZP :word-start "Store position in-case of collision")
    ;;Now try to build up a hash for the word. Stop when
    ;;we get to a space or end of line etc.
    ;;Buffer is zero terminated so we can't overrun
    (dc "Character 0")
    (LDA.IZY :inp)
    (BEQ :end)
    (CLC)
    (ADC (to-ubyte (aref *hash-fudge-factors* 0)) "Fudge 0")
    (STA.ZP :tmp)
    (dc "Character 1")
    (INY)
    (LDA.IZY :inp)
    (BEQ :end)
    (CLC)
    (ADC (to-ubyte (aref *hash-fudge-factors* 1)) "Fudge 1")
    (ASL.ZP :tmp)
    (EOR.ZP :tmp)
    (STA.ZP :tmp)
    (dc "Character 2")
    (INY)
    (LDA.IZY :inp)
    (BEQ :end)
    (CLC)
    (ADC (to-ubyte (aref *hash-fudge-factors* 2)) "Fudge 2")
    (ASL.ZP :tmp)
    (EOR.ZP :tmp)
    (STA.ZP :tmp)
    (dc "Character 3")
    (INY)
    (LDA.IZY :inp)
    (BEQ :end)
    (CLC)
    (ADC (to-ubyte (aref *hash-fudge-factors* 3)) "Fudge 3")
    (ASL.ZP :tmp)
    (EOR.ZP :tmp)
    (STA.ZP :tmp)
    (INY)
    (LDA.IZY :inp)
    (BEQ :end)
    (dc "128 - 4 character is xored into the hash")
    (LDA 128)
    (SEC)
    (SBC.IZY :inp)
    (EOR.ZP :tmp)
    (STA.ZP :tmp)
    (label :end)
    (LDX.ZP :tmp)
    (dc "Look up the word meaning")
    (LDA.ABX :word-meanings)
    (BNE :store-result)
    (dc "Colliding word, lets have a go with")
    (dc "the binary parser to resolve it")
    (LDY.ZP :word-start)
    (JSR :binary-parser)
    (dc "Since the binary parser isn't too fussy about")
    (dc "where it stops, rewind back to the beginning")
    (dc "of the word before we look for the next space")
    (LDY.ZP :word-start)
    (label :store-result)
    (LDX.ZP :word-index)
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
    (TXA)
    (STA.ZP :word-index)
    (CMP *max-words*)
    (BNE :next)
    (RTS)
    
    (dc "The parsed word meanings get put here")
    (dbs :words *max-words*)
    (dc "The user input buffer")
    (dc "Terminated with many zeroes for convenience.")
    (dbs :input (+ *max-words* *max-input-length*))
    (dc "Table mapping hash values to word meanings")
    (apply 'db :word-meanings (coerce *word-hash-table* 'list))))

(defun dump-words ()
  (let ((i 0))
    (maphash #'(lambda (k v)
		 (format t "~a=#x~x (#x~x) " k v
			 (apply 'hash k
				(coerce *hash-fudge-factors* 'list)))
		 (when (zerop (mod (incf i) 5))
		   (terpri)))
	     *word-ids*))
  (print "Collisions:")
  (print *word-collisions*)
  (values))

(defun build-hash-test (pass)
  (funcall pass)
  (build-symbol-table)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defun hash-test (word)
  (reset-compiler)
  (reset-symbol-table)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)

	   (sta16.zp :word '(:parser . :inp))
	   
	   (JSR '(:parser . :parse-direct))

	   (BRK)
	   
	   (dis :word word)
	   (db nil 0 0 0 0 0)
	   
	   (parser)
	   (binary-parser)
	   
	   (label :end)))
    
    (build-hash-test #'pass))

  (dump-words)
  
  (monitor-reset #x600)
  (monitor-run)
  
  (aref (monitor-buffer) (resolve '(:parser . :words))))

(defun test-hash-word (word id &optional (hash-only nil))
  (assert (= (hash-test word)
	     (if (and hash-only (find word *word-collisions*))
		 0 id))
	  nil
	  (format nil "The word ~a hashed to the wrong value" word)))
  
;; Test all words in the list

(build-hash-table t)

(maphash #'(lambda (k v) (test-hash-word k v)) *word-ids*)

(defun parse-words-tester (input)
  (reset-compiler)
  (reset-symbol-table)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (label :start)
	   (JSR :parse)
	   (BRK)
	   (parser)
	   (binary-parser)
	   (label :end)))
    
    (build-hash-test #'pass))

  ;; install the string into the input buffer

  (loop for c across input
        for i from 0 to *max-input-length* do       
       (setf (aref *compiler-buffer* (+ i (resolve '(:parser . :input))))
	     (to-alphabet-pos c)))
  
  (monitor-reset #x600)
  (monitor-run)
  
  (coerce (subseq (monitor-buffer)
		  (resolve '(:parser . :words))
		  (+ -1 (resolve '(:parser . :words)) *max-words*))
	  'list))


(defun test-parse-input (input expected)
  (format t "Testing ~a~%" input)
  (loop
     for e in expected
     for r in (parse-words-tester input) do
       (unless (eq '? e)
	 (assert (equal r (gethash (symbol-name e) *word-ids*))))))

(test-parse-input "OPEN DOOR" '(:OPEN :DOOR))
(test-parse-input "PUSH CYLINDER" '(:PUSH :CYLINDER))
(test-parse-input "PRESS CYLINDER" '(:PUSH :CYLINDER))
(test-parse-input "OPEN FRABJOUS DOOR" '(:OPEN ? :DOOR))
(test-parse-input "OPEN DOOR CLOSE" '(:OPEN :DOOR :CLOSE))
(test-parse-input " OPEN DOOR" '(:OPEN :DOOR))
(test-parse-input "PUSH  CYLINDER" '(:PUSH :CYLINDER))
(test-parse-input "PRESS   CYLINDER" '(:PUSH :CYLINDER))
(test-parse-input "    OPEN  FRABJOUS  DOOR  " '(:OPEN ? :DOOR))
(test-parse-input "OPEN     DOOR      CLOSE    " '(:OPEN :DOOR :CLOSE))

