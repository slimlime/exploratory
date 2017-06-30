;; So we want to be able to do the following

;; A lookup between words and their meaning ids

(defparameter *word-id-count* nil)
(defparameter *word-ids* nil)
(defparameter *id-meanings* nil)

(defun reset-parser ()
  (setf *word-ids* (make-hash-table :test 'equal))
  (setf *id-meanings* (make-hash-table))
  (setf *word-id-count* 1))

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

(defun parser ()
  (label :parse-word)
  (with-namespace :parser
    (alias :word :A0)
    (let ((words (make-array 0 :fill-pointer 0 :adjustable t))
	  (label-number 1))
      (maphash #'(lambda (k v)
		   (declare (ignorable v))
		   (vector-push-extend k words))
	       *word-ids*)
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
		       0
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
			 (label prim)
			 (LDA id (format nil "~a" prim))
			 (RTS))
		     (let ((split (split i j k)))
		       (if (null split)
			   (generate i j (1+ k) y)
			   (progn
			     (dc (format nil ";~a or ~a?"
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

(defun build-parse-test (pass)
  (funcall pass)
  (build-symbol-table)
  (funcall pass)
  (setf *compiler-final-pass* t)
  (funcall pass))

(defparameter *max-input-length* 40)
(defparameter *max-words* 4)

(defun parse-test (word)
  (reset-compiler)
  (reset-symbol-table)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)

	   (sta16.zp :word '(:parser . :word))

	   (LDY 0)
	   
	   (JSR :parse-word)
	   (STA.AB :output)
	   (BRK)
	   
	   (dis :word word)

	   (db :output 0)
	   
	   (parser)
	   
	   (label :end)))
    
    (build #'pass))
  
  (monitor-reset #x600)
  (monitor-run)
  
  (aref (monitor-buffer) (resolve :output)))

(reset-parser)
  
(defword :PRESS :PUSH)
(defword :GET :PICK :TAKE)
(defword :OPEN)
(defword :CLOSE :SHUT)
(defword :SAY :SPEAK :TELL)
(defword :UP :CLIMB)
(defword :DOWN :DESCEND)
(defword :OPERATE :USE)
(defword :CYLINDER)
(defword :DOOR)

(defun test-parse-word (word id)
  (assert (= (parse-test word) id)))

;TODO ensure we don't match DOOR to DESCEND

;(assert (= (parse-test "DOOR") 0))

;; Test all words in the list

(maphash #'(lambda (k v) (test-parse-word k v)) *word-ids*)

;; This function will take an input buffer and parse
;; the words out of it.

(defun parse-words ()
  (label :parse-input)
  (with-namespace :parse-input
    (alias :pos :D0)
    (LDX 0 "X is our word pointer")
    (dc "Clear the parsed words buffer")
    (dotimes (i *max-words*)
      (STX.AB (+ (resolve :words) i)))
    ;;the input address, but we could make this absolute
    ;;as we are only ever going to read input from one place
    (sta16.zp :input '(:parser . :word))
    (dc "Start at the beginning of the input")
    (LDY #xff)
    (label :next)
    (INY)
    (TYA)
    (CMP *max-input-length* "Input buffer end?")
    (BEQ :done)
    (dc "Skip spaces to the next word")
    (LDA.IZY '(:parser . :word))
    (BEQ :next)
    (dc "We have found a word?")
    (STY.ZP :pos "Save the position")
    (JSR :parse-word)
    (STA.ABX :words)
    (INX)
    (TXA)
    (CMP *max-words* "Reached our word limit?")
    (BEQ :done)
    (LDY.ZP :pos "Restore pointer")
    (dc "Find the end of the word")
    (label :next1)
    (INY)
    (TYA)
    (CMP *max-input-length* "Input buffer end?")
    (BEQ :done)
    (LDA.IZY '(:parser . :word))
    (BNE :next1)
    (BEQ :next)
    (label :done)
    (RTS)

    (dc "The parsed words are put here")
    (dbs :words *max-words*)
    (dc "The user input buffer")
    (dbs :input *max-input-length*)))

(defun parse-words-tester (input)
  (reset-compiler)
  (reset-symbol-table)
  
  (flet ((pass ()
	   (zeropage)	     
	   (org #x600)
	   (CLD)
	   (label :start)
	   (JSR :parse-input)
	   (BRK)
	   (parser)
	   (parse-words)
	   (label :end)))
    
    (build #'pass))

  ;; install the string into the input buffer

  (loop for c across input
        for i from 0 to *max-input-length* do       
       (setf (aref *compiler-buffer* (+ i (resolve '(:parse-input . :input))))
	     (to-alphabet-pos c)))
  
  (monitor-reset #x600)
  (monitor-run)
  
  (coerce (subseq (monitor-buffer)
		  (resolve '(:parse-input . :words))
		  (+ -1 (resolve '(:parse-input . :words)) *max-words*))
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
