; 8-bit Tunstall table

; For our game we want to call the render routine for each character
; We want the table to be as compact as possible, so we split it into
; three groups, 1,2 and 3 characters


(defun count-words (table str start len strlen)
  (unless (> (+ start len) strlen)
    (let* ((substr (subseq str start (+ len start)))
	   (val (gethash substr table)))
      (setf (gethash substr table)
	    (if val (1+ val) 1)))))
 
(defun word-freq (str maxlen)
  (let ((strlen (length str))
	(table (make-hash-table :test 'equal)))
    (loop for start from 0 to (1- (length str)) do
	 (loop for size from 2 to maxlen do
	      (count-words table str start size strlen)))
    table))

(defun filter-table (table top reset)
  (let ((lst nil)
	(len 0))
    (maphash 
     #'(lambda (k v)
	 (when (or (> v 1) 
		   (= 1 (length k)))
	   (push (list k (* v (length k))) lst)
	   (incf len)))
     table)
    (let ((arr (make-array len)))
      (loop for i from 0 to (1- len) do
	   (setf (aref arr i)
		 (append (pop lst) (list 0 0))))
      (setf top (if (> top (length arr)) (length arr) top))
      (setf arr (subseq (sort arr #'> :key #'second) 0 top))
      (when reset
	(loop for i from 0 to (1- top) do
	     (setf (second (aref arr i)) 0)))
      arr)))

(defun lenc (c)
  (if (stringp c)
      (length c)
      1))

(defun tunstall (str max-word-size)
  (let ((letters (make-hash-table)))
    (loop for i from 0 to (1- (length str)) do
	 (setf (gethash (aref str i) letters) t))
    ;Now build a symbol table
    (let ((symbols (make-array 256 :initial-element "?"))
	  (index -1))
      (maphash #'(lambda (k v)
		   (declare (ignore v))
		   (setf (aref symbols (incf index)) (string k))) letters)
      (let ((words (sort (filter-table (word-freq str max-word-size) (- 255 index) nil)
			 #'< :key #'(lambda (s) (/ (fourth s) (lenc (first s)))))))
	(loop for i from 0 to (1- (length words)) do
	     (setf (aref symbols (+ 1 index i))
		   (first (aref words i)))))
      (sort symbols #'> :key #'length))))

(defun encode-tunstall (str words emit)
  (let ((strend (1- (length str))))
    (loop for i from 0 to strend do
	 (loop for j from 0 to 255 do
	      (let ((word (aref words j)))
		(when (and (<= (+ i -1 (length word)) strend)
			   (equal word (subseq str i (+ i (length word)))))
		  (funcall emit j)
		  (incf i (1- (length word)))
		  (return)))))))

(defun dtunstall (str)
  (let ((table (tunstall str 3))
	(one-chars nil)
	(two-chars nil)
	(three-chars nil))
    (loop for i from 0 to 255 do
	 (let ((sym (aref table i)))
	   (when (and (null three-chars)
		      (< (length sym) 3))
	     (setf three-chars i))
	   (when (and (null two-chars)
		      (< (length sym) 2))
	     (setf two-chars (- i three-chars)))))
    (setf one-chars (- 256 two-chars three-chars))
    
    (label :rstr-nxt)
    (INY)
    (BNE :rstr-cont)
    (INC.ZP :str-add)
    (BNE :rstr-cont)
    (INC.ZP (1+ :str-add))
    (BNE :rstr-cont)
    (dc "You've bust the last page if you ended up here")
    (RTS)
    (dc "Render the string at STR-ADD")
    (label :rstr)
    (STY.IMM #x00)
    (label :rstr-cont)
    (LDA.INY :str-add)
    (CMP.IMM three-chars)
    (BCS :2char)
    (dc "Times by three, then index into the three char table")
    (STA.ZP :scratch)
    (ASL)
    (CLC)
    (ADC.ZP :scratch)
    (TAX)
    (LDA.ABX :3char-tbl)
    (
    (emit)
    (INX)
    (LDA.ABX :3char-tbl)
    (emit)
    (INX)
    (LDA.ABX :3char-tbl)
    (emit)
    (CLC)
    (label :2char)
    (CMP.IMM (+ three-chars two-chars))
    (BCS :1char)
    (ASL)
    (TAX)
    (LDA.ABX :2char-tbl)
    (emit)
    (INX)
    (LDA.ABX :2char-tbl)
    (emit)
    
    (label :1char)

    (label :emit)
    (dc "Emit one character by looking up address in jump table")
    (TAX)
    (LDA.ABX :1char-htbl)
    (PHA)
    (LDA.ABX :1char-ltble)
    (PHA)
    (RTS)

    


))
	    
