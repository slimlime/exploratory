;; Method 1 - Shannon Fano, Shannon Fano with dictionary
;; - Create table of most common >1 character words
;; with a max word size and a max table size
;; - Make a first pass over the input encoding by
;; greedily matching strings.
;;
;;    F1- match in order of weighted frequency
;;    F2- match in order of symbol length
;;
;; The first pass also creates a frequency table
;; for the single characters AND re-calculates
;; the occurences of the symbols as it finds them
;; in the encoding.
;; - Create a Shannon Fano code from all the symbols
;; Encode the string with the symbols
;;
;;    E1 - match in weighted frequency order
;;    E2 - match in symbol length order
;;    E3 - match in efficieny order (bits per character)
;;
;; Initial findings for F1/E3 were that it improved by 2%
;; over matching on the weighted frequency order
;; 
;; Word size- going over 8 seemed to make no difference
;; to the table. Going over 16 seemed to make it very
;; slow. On that basis 8 seems a good round number. 
;; 
;; Results
;;
;; word size 8          WEIGHTED PRE-SORT   LENGTH PRE-SORT
;; table size               64    256           64     256
;; 
;; sorted by efficiency 725034 666470        705712 642135
;; sorted by length     725034 666470        705712 642135 totes suspic
;; no sort              726219 668066        711118 666650

;; word size 0, e.g. plain shannon-fano
;;                             806402
;; word size 2          725578 675827 - the dictionary isn't that great!!
;; perhaps digraphs are the way to go

;; Digraphs only gave an encoded size only a tiny percent
;; above the same with larger words. Is having the dictionary
;; a poor idea?
;;
;; Method 1a
;; Shannon code characters and digraphs, with a single code
;; for all words over 2 characters then emit a byte.
;; I have strong suspicions this won't work as the best way
;; to encode the words would be via a shnanon code too, presumably
;; a byte would be less efficient.


;; Method 2 - Shannon Forest
;; Since digraphs are so awesome, why not make a restricted state
;; machine from the digraph table.
;; So we have multiple shannon trees, limited by some heuristic
;; *->etaionshrdlu etc This tree would have ALL symbols
;; t->hea*             This tree would have a subset, and a branch back to *
;; e-> e*              This tree would have a subset, and a branch back to *

;; Tunstall
;;
;; word size  2  643232
;;            3  632928 <- This is the best so far
;;            4  636744
;;            5  642800

(defun file2str (file)
  (with-open-file (stream file)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      str)))
  

(defun letter-freq (file)
  (let ((arr (make-array 256 :element-type 'integer :initial-element 0)))
    (loop for c across (file2str file) do
	 (incf (aref arr (char-code c))))
    arr))

(defun print-freq (frequencies)
  (loop for c from 32 to 126 for i from 1 do
       (format t "~c: ~d~a"
	       (code-char c) (aref frequencies c)
	       (if (zerop (rem i 8)) #\newline #\tab))))

(defun sort-freq (frequencies)
  (let ((arr (make-array 256)))
    (loop for c from 0 to 255 for i from 1 do
	 (setf (aref arr c) (list c (aref frequencies c) 0 -1)))
    (sort arr #'> :key #'second)))

(defun bin2str (n bits)
  (let ((r ""))
    (dotimes (i bits)      
      (setf r (format nil "~A~A" (if (zerop (logand 1 n)) "0" "1") r))
      (setf n (ash n -1)))
    r))

(defun print-codes (frequencies)
  (loop for c from 0 to 255 for i from 1 do
       (unless (zerop (second (aref frequencies c)))
	 (let ((x (first (aref frequencies c))))
	   (format t "~a: ~a~a"
		   (if (< x 32)
		       (format nil "x~x" c)
		       (format nil "  ~c" (code-char x)))
		   (bin2str (third (aref frequencies c))
			    (fourth (aref frequencies c)))
		   (if (zerop (rem i 4)) #\newline #\tab))))))

(defun calculate-size (frequencies)
  (let ((bits 0)
	(orig 0))
    (loop for c from 0 to 255 do
	 (setf orig (+ (second (aref frequencies c)) orig))
	 (setf bits (+ (* (fourth (aref frequencies c))
			  (second (aref frequencies c)))
		       bits)))
    (values orig (/ bits 8))))


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


(defun phash (table)
  (maphash #'(lambda (k v) (print k) (print v)) table))

;filter hash-table of words- all letters are included
;all words are included where there is > 1 occurrence

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

(defun find-string (str strlen pos table)
  (loop for i from 0 to (1- (length table)) do
       (let* ((item (aref table i))
	      (symlen (length (first item))))
	 (when (and
		(<= (+ pos symlen) strlen)
		(equal (subseq str pos (+ pos symlen))
		       (first item))
		(return item))))))


;Convert the string into a list of symbols or characters

(defun make-char-table ()
  (let ((arr (make-array 256)))
    (loop for i from 0 to 255 do
	 (setf (aref arr i) (list (code-char i) 0 0 0)))
    arr))

;; do a dry run of encoding, counting new frequencies for
;; the symbols
(defun first-pass (str table char-table)
  (loop for i from 0 to 255 do
       (setf (second (aref char-table i)) 0))
  (loop for i from 0 to (1- (length table)) do
       (setf (second (aref table i)) 0))
  (let ((strlen (length str)))
    (loop for i from 0 to (1- strlen) do
	 (let ((sym (find-string str strlen i table)))
	   (if sym
	       ; found a word, increment its count and skip along
	       (let ((symlen (length (first sym))))
		 (incf (second sym)) ;NOT weighted by symbol length
		 ;(push sym output)
		 (incf i (1- symlen)))
	       ; it was just a character, so increment its count
	       (let ((c (char str i)))
		 (incf (second (aref char-table (char-code c))))
		 ;(push c output)
		 ))))
    (remove-if #'(lambda (x) (zerop (second x)))
     (sort (concatenate 'vector table char-table) #'> :key #'second))))


(defun print-ctbl (char-table)
  (loop for c from 32 to 126 for i from 1 do
       (format t "~c: ~d~a"
	       (first (aref char-table c))
	       (second (aref char-table c))
	       (if (zerop (rem i 8)) #\newline #\tab))))


;; With the sorted frequencies we want to make a shannon fano code
;; So what we do is split the list at the mid-point, then do
;; the same recursively, appending a 1 or a 0 respectively
;; to the codes

(defun shannon-fano (frequencies bit start end)
  (unless (> start end)
    (let ((total 0)
	  (halftotal 0))
      (loop for c from start to end do
	   (when (zerop (second (aref frequencies c)))
	     (setf end (1- c))
	     (return nil))
	   (setf total (+ total (second (aref frequencies c))))
					; increment the number of bits needed
	   (incf (fourth (aref frequencies c)))
					; shift the previous bit over and add the next one
	   (setf (third (aref frequencies c))
		 (+ (* 2 (third (aref frequencies c))) bit)))
					; half the total is where we split the table
      (unless (= start end)
	(setf halftotal (/ total 2))
	(loop for c from start to end do
	     (setf total (- total (second (aref frequencies c))))
	     (when (<= total halftotal)
					; apply a one bit to one half of the table
	       (shannon-fano frequencies 1 start c)
					; and a zero bit to the other half
	       (shannon-fano frequencies 0 (1+ c) end)
	       (return nil)))))))

;; find words only, not characters. This searches in order. Perhaps it
;; should be greedy and select the one with the best compression ratio
(defun find-word (str strlen pos table table-size)
  (loop for i from 0 to (1- table-size) do
       (let ((item (aref table i)))
	 (when (stringp (first item))
	   (let ((symlen (length (first item))))
	     (when (and
		    (<= (+ pos symlen) strlen)
		    (equal (subseq str pos (+ pos symlen))
			   (first item))
		    (return item))))))))

(defun find-char (c table table-size)
  (loop for i from 0 to (1- table-size) do
       (let ((item (aref table i)))
	 (when (eq c (first item))
	   (return item)))))

(defun lenc (c)
  (if (stringp c)
      (length c)
      1))

;encode to a list of symbols
(defun encode (str table)
  (let ((output nil)
	(strlen (length str))
	(table-size (length table))
	(bits 0))
    (loop for i from 0 to (1- strlen) do
	 (let ((sym (find-word str strlen i table table-size)))
	   (if sym
	       ; found a word (probably would be better
	       ; to have split the table here, but meh
	       (let ((symlen (length (first sym))))
		 (push sym output)
		 (incf i (1- symlen))
		 (incf bits (fourth sym)))
	       ; it was just a character
	       (let ((c (find-char (char str i) table table-size)))
		 (incf bits (fourth c))
		 (push c output)))))
    (nreverse output)))

;dont go too high with word-len, 8 seems to be no different than 16

(defun make-table (str max-table-size word-size)
  (let ((tbl1 (filter-table (word-freq str word-size) max-table-size T)))
    (first-pass str
		(sort tbl1 #'> :key #'(lambda (s) (lenc (first s)))) 
					;sort first pass table by length
					;tbl1
		(make-char-table))))
       
(defun doit (file max-table-size word-size)
  (let* ((str (file2str file))
	 (table (make-table str max-table-size word-size)))
    (shannon-fano table 0 0 (1- (length table)))
  ;;table)) ;no-sort
;;sorting into the most efficient seems to give 2% since we do greedy
;;      (sort table #'> :key #'(lambda (s) (lenc (first s))))))
;;sorting into the most efficient seems to give 2% since we do greedy
    (sort table #'< :key #'(lambda (s) (/ (fourth s) (lenc (first s)))))))

;; word size 8          WEIGHTED PRE-SORT   LENGTH PRE-SORT
;; table size               64    256           64     256
;; 
;; sorted by efficiency 725034 666470        705712 642135
;; sorted by length     725034 666470        705712 642135 totes suspic
;; no sort              726219 668066        711118 666650

;; word size 0, e.g. plain shannon-fano
;;                             806402
;; word size 2          725578 675827 - the dictionary isn't that great!!
;; perhaps digraphs are the way to go

;; encode to a binary string

(defun encode2bin (encoded &key (maxsize 65536))
  (let ((vec (make-array maxsize
			 :initial-element 0
			 :element-type '(unsigned-byte 8)
			 :adjustable t))
	(charp 0)
	(bitp 128))
    (dolist (el encoded)
      (let ((bitlen (fourth el))
	    (pattern (third el)))
	(loop for j from (1- bitlen) downto 0 do
	     (when (logbitp j pattern)
	       (incf (aref vec charp) bitp))
	     (setf bitp (ash bitp -1))
	     (when (zerop bitp)
	       (incf charp)
	       (setf bitp 128)))))
    (subseq vec 0 (1+ charp))))

;; flatten the tree

(defun flatten (table)
  (let ((lst nil)
	(lbl 0))
    (defun flat2 (start end)
      (unless (> start end)
	(if (= start end)
	    (push `(emit ,(first (aref table start))) lst)
	    (progn
	      (let ((total 0)
		    (halftotal 0))
		(loop for c from start to end do
		     (incf total (second (aref table c))))
		(setf halftotal (/ total 2))
		(loop for c from start to end do
		     (decf total (second (aref table c)))
		     (when (<= total halftotal)
		       (push 'fetch lst)
		       (let ((bra (push (list 'branch '?) lst)))
			 (flat2 start c)
			 (setf (second (first bra)) (incf lbl))
			 (push `(L ,lbl) lst)
			 (flat2 (1+ c) end))
		       (return nil))))))))
    (flat2 0 (1- (length table)))
    (nreverse lst)))


;; Method 2 - Shallow forest of Shannon-Fano
;; The predicate for actually having a sub tree is
;; if the sum of the code lengths for all instances of C1 is less than
;; if the sum of the code lengths for the root tree. Is there a heuristic for
;; this? Perhaps if the frequencies for the top n are sufficiently different
;; than for the main distribution? I imagine it would be for a . where it almost
;; certainly is followed by a space or a closing quote

;; Method 3 - Tunstall
;; Tunstall is a very simple method that assigns symbols to fixed length codes
;; A byte is a fixed length, so let us create a table with 256 entries
;; All single characters have to appear. If we are clever then they
;; can appear in the position they would occupy in the ascii table...
;; I would call that Stable Tunstall.

(defun tunstall (str max-word-size)
  (let ((letters (make-hash-table)))
    (loop for i from 0 to (1- (length str)) do
	 (setf (gethash (aref str i) letters) t))
    ;Now build a symbol table
    (let ((symbols (make-array 256 :initial-element "?"))
	  (index -1))
      (maphash #'(lambda (k v) (setf (aref symbols (incf index)) (string k))) letters)
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

(defun encode-tunstall-size (str words)
  (let ((size 0))
	(encode-tunstall str words #'(lambda (x) (incf size)))
    size))

(defun encode-tunstall-freq (str words)
  (let ((freq (make-array 256)))
    (encode-tunstall str words 
		     #'(lambda (symbol) (incf (aref freq symbol))))
    (let ((i -1))
      (sort (map 'vector #'(lambda (symbol) (list (incf i) symbol 0 0))
			      freq)
	    #'> :key #'second))))


