;;;; VM WIP
;;;; These are the common operations. Once we know everything
;;;; we need, we can implement the game logic using byte-code
;;;; for massive gainz in compactness.


(defun vm ()
  (with-namespace :vm
    (zp-w :pc)
    (label :go)
    (LDY 0)
    (LDA.IZY :pc)
    (dc "Get the instruction")
    (BEQ :done "RTS")
    (JSR :op)
    (inc16.zp :pc) 
    (dc "Always jump to next")
    (BNE :go)
    (label :op)
    (TAX)
    (LDA.ABX (- (resolve :op-hi) 2))
    (PHA)
    (LDA.ABX (- (resolve :op-lo) 2))
    (PHA)    
    (label :done)
    (RTS)

    (let ((vmops nil))
      (macrolet ((vmop (op)
		    `(progn
		       (label ,op)
		       (push ,op vmops))))
	(label :PR1)

	(label :PR2)

	(label :PR3)

	(label :WHEN-BIT)

	(label :UNLESS-BIT)

	(label :IF-BIT)

	(label :WHEN-OBJECT-PLACE)

	(label :UNLESS-OBJECT-PLACE)

	(label :IF-OBJECT-PLACE)



    (label :op-hi)
    (mapc #'(lambda (addr) (db nil (rts-jmp-hi addr)) opcodes))

    (label :op-lo)
    (mapc #'(lambda (addr) (db nil (rts-jmp-lo addr)) opcodes))
    
