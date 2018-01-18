(defparameter *handlers* nil)

(defun reset-dispatcher ()
  (setf *handlers* (make-hash-table)))

;;Push a sentence handler into the list of handlers for a location
;;Note that the location is NOT automatically applied to the handler address
(defun defsentence (words handler-address &optional (location :generic))
  ;;put all words in the table that aren't already in there
  ;;as it stands we won't be able to declare any synonyms
  ;;once they have been declared here, but perhaps that is yagni
  (dolist (word words)
    (unless (gethash (symbol-name word) *word->meaning*)
      (defword word)))

  (assert (null (find words (gethash location *handlers*)
		      :key #'car :test #'equal))
	  nil (format nil "The words ~a already appear with a handler." words))
  ;;if this throws and shouldn't has the handlers table been reset?
  (push (cons words handler-address)
	(gethash location *handlers*)))

(defun delegate-action ()
  "Bail on an action and call the generic handler"
  (JMP '(:dispatcher . :generic-only)))

(defun dispatcher ()
  ;;when we enter a location, this dispatch table should be set
  (zp-w :location-dispatch-table)
  (with-namespace :dispatcher
    (alias :dispatch-table :A0)
    (alias :handled :D0)
    (alias :dispatch-ptr :D1)
    (alias :tries :D2)
    ;;TODO Does it make sense for parse and dispatch to be
    ;;two separate functions? i.e. should we call parse
    ;;from here?
    (label :generic-only)
    (LDA 1)
    (BNE :try-generic)
    (label :dispatch nil)
    (LDA 2)
    (STA.ZP :tries)
    (dc "Choose the location based table")
    (cpy16.zp :location-dispatch-table :dispatch-table)
    (label :dispatch)
    (LDY 0)
    (LDA.IZY :dispatch-table)
    (dc "Is this the last entry in the dispatch table?")
    (BNE :compare-word)
    (DEC.ZP :tries)
    (dc "We only try 2 tables, then return")
    (BNE :try-generic)
    (dc "Call the handler of last resort, i.e. failure")
    (LDY 3)
    (BNE :matched-sentence)
    (label :try-generic)
    (dc "Choose the generic table")
    (sta16.zp :generic :dispatch-table)
    (LDY 0)
    (label :compare-word)
    (LDX 0)
    (label :next-word)
    (LDA.IZY :dispatch-table)
    ;;TODO store wildcard match so it can be used
    ;;by the handler.
    (BEQ :matched-word "0 always matches")
    (CMP.ABX '(:parser . :words))
    (BNE :next-input-word)
    (label :matched-word)
    (dc "Great work kid, now match another one.")
    (INY)
    (CPY 3)
    (BEQ :matched-sentence)
    (label :next-input-word)    
    (INX)
    (CPX *max-words*)
    (BNE :next-word)
    (dc "We exhausted the input words")
    (label :next-handler)
    (dc "Skip to next entry in dispatch table")
    (add16.zp 5 :dispatch-table)
    (dc "Assume the carry is clear after long add")
    (BCC :dispatch)
    (label :matched-sentence)
    
    (dc "We are assuming Y is 3 here, which is the")
    (dc "offset to the the dispatch address")
    (dc "Call the VM with the address of the handler")
    (LDA.IZY :dispatch-table)
    (STA.ZP (lo-add :vm-pc))
    (INY)
    (LDA.IZY :dispatch-table)
    (STA.ZP (hi-add :vm-pc))
    (JMP :vm-go "Execute VM code and return to caller")
    
    ;;now build the tables

    (maphash
     #'(lambda (location entry-list)
	 (dc (format nil "~a dispatch table" location))
	 (label location)
	 (dolist (entry (reverse entry-list))
	   (let ((words
		  ;;this just means that we always get a list of three elements
		  ;;todo, look at how to do this nicelier.
		  (coerce (subseq (coerce (append (car entry) '(? ? ?)) 'vector)
				  0 3)
			  'list))
		 (handler (cdr entry)))
	     (dc (format nil "~{~a ~} -> ~a" words (fmt-label handler t)))
	     (apply 'db nil (append
			     (mapcar #'(lambda (word)
					 (let* ((symb (symbol-name word))
					    (id (if (equal "?" symb) 0
						    (gethash symb *word->meaning*))))
					   (assert id nil "Unknown word ~a" word)
					   id))
				     words)
			     ;;rts jump requires address offset by one
			     (list (hi handler)
				   (lo handler))))))
	 (dc (format nil "Terminating byte for ~a" location))
	 (db nil 0))
    *handlers*)))

(defun dump-handlers ()
  (maphash #'(lambda (k v)
	       (format t "In ~a~%" k)
	       (dolist (entry v)
		 (format t "  ~a -> ~a~%"
			 (car entry)
			 (cdr entry))))
	   *handlers*))
