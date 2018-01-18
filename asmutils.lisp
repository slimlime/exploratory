;;TODO rationalise the tests and make an automated build which calls them

(defun zeropage ()

  ;; Scratch Area
  ;;
  ;; This contains essentially 'local variables'
  ;; How local they are depends on the use-case
  ;; e.g. there are two co-operating functions
  ;; in the string rendering- they take care to
  ;; share the scratch area as they call each other
  ;; Self contained functions can just use them
  ;; as they see fit.

  ;; TODO have it execute some code on start up to initialize
  ;; values.
  
  (zp-w :A0)
  (zp-w :A1)
  (zp-w :A2)
  (zp-w :A3)
  (zp-w :A4)
  
  (zp-b :D0)
  (zp-b :D1)
  (zp-b :D2)
  (zp-b :D3)
  (zp-b :D4)
  (zp-b :D5)
  (zp-b :D6))

(defun inc16.zp (label)
  "Increment a zero-page word"
  (with-local-namespace
    (INC.ZP (lo-add label))
    (BNE :inc16-done)
    (INC.ZP (hi-add label))
    (label :inc16-done)))

(defun sub16.zp (value zp)
  "Subtract a 16 bit value from a zero-page word"
  (SEC)
  (LDA.ZP (lo-add zp))
  (SBC (lo value))
  (STA.ZP (lo-add zp))
  (LDA.ZP (hi-add zp))
  (SBC (hi value))
  (STA.ZP (hi-add zp)))

(defun sbc16.zp (value zp)
  "Subtract a 16 bit value from a zero-page word without setting the carry first"
  (LDA.ZP (lo-add zp))
  (SBC (lo value))
  (STA.ZP (lo-add zp))
  (LDA.ZP (hi-add zp))
  (SBC (hi value))
  (STA.ZP (hi-add zp)))

(defun add16.zp (value zp)
  (CLC)
  (LDA.ZP (lo-add zp))
  (ADC (lo value))
  (STA.ZP (lo-add zp))
  (LDA.ZP (hi-add zp))
  (ADC (hi value))
  (STA.ZP (hi-add zp)))

(defun adc16a.zp (zp)
  "16 bit add accumulator to zero page"
  (with-local-namespace
    (ADC.ZP (lo-add zp))
    (STA.ZP (lo-add zp))
    (BCC :no-carry)
    (INC.ZP (hi-add zp))
    (label :no-carry)))
  
(defun add16a.zp (zp)
  "16 bit add accumulator to zero page"
  (with-local-namespace
    (CLC)
    (ADC.ZP (lo-add zp))
    (STA.ZP (lo-add zp))
    (BCC :no-carry)
    (INC.ZP (hi-add zp))
    (label :no-carry)))
  
(defun cpy16.zp (from to)
  "Copy a zero page word to another using A"
  (LDA.ZP (lo-add from))
  (STA.ZP (lo-add to))
  (LDA.ZP (hi-add from))
  (STA.ZP (hi-add to)))

;;TODO Pretty sure we can work out whether this is ab or zp
;;out since the
;;parameters are fixed.

(defun cpy16.abzp (from to)
  "Copy a word to another using A"
  (LDA.AB (lo-add from))
  (STA.ZP (lo-add to))
  (LDA.AB (hi-add from))
  (STA.ZP (hi-add to)))

(defun sta16.zp (addr zpg)
  "Load an address into a zero page word"
  (LDA (lo addr))
  (STA.ZP (lo-add zpg))
  (LDA (hi addr))
  (STA.ZP (hi-add zpg)))

(defun call-memcpy (src dst len)
  (assert (> len 0))
  (assert (< len 65536))
  (with-namespace :memcpy
    (sta16.zp src :src)
    (sta16.zp dst :dst)
    (if (< len 256)
	(progn
	  (LDY 0)
	  (LDX len)
	  (JSR '(:memcpy . :copy-remainder)))
	(progn
	  (LDA (lo len))
	  (LDX (hi len))
	  (JSR :memcpy)))))

(defun memcpy ()
  (label :memcpy)
  (with-namespace :memcpy
    (alias :src :A0)
    (alias :dst :A1)
    (alias :remainder :D0)
    (STA.ZP :remainder)
    (LDY 0)
    (label :copy-page)
    (LDA.IZY :src)
    (STA.IZY :dst)
    (INY)
    (BNE :copy-page)
    (INC.ZP (hi-add :src))
    (INC.ZP (hi-add :dst))
    (DEX)
    (BNE :copy-page)
    (LDX.ZP :remainder)
    (BEQ :done)
    (label :copy-remainder)
    (LDA.IZY :src)
    (STA.IZY :dst)
    (INY)
    (DEX)
    (BNE :copy-remainder)
    (label :done)
    (RTS)))

(defun call-memset (value dst len)
  (assert (> len 0))
  (assert (< len 65536))
  (with-namespace :memset
    (alias :remainder :D0)
    (sta16.zp dst :dst)
    (if (< len 256)
	(progn
	  (LDY 0)
	  (LDX len)
	  (LDA value)
	  (JSR :memset-remainder))
	(progn
	  (LDA (lo len))
	  (STA.ZP :remainder)
	  (LDX (hi len))
	  (LDA value)
	  (JSR :memset)))))

(defun memset ()
  (label :memset)
  (with-namespace :memset
    (alias :dst :A0)
    (LDY 0)
    (label :memset-page)
    (STA.IZY :dst)
    (INY)
    (BNE :memset-page)
    (INC.ZP (hi-add :dst))
    (DEX)
    (BNE :memset-page)
    (LDX.ZP :remainder)
    (label :memset-remainder)
    (STA.IZY :dst)
    (INY)
    (DEX)
    (BNE :memset-remainder)
    (label :done)
    (RTS)))

;;Retrieve the parameter at the grandparent call-site
;;Store first byte in X, second in A
(defun deref-w ()
  (label :deref-w)
  (with-namespace :deref
    (zp-w :tmp)
    (TSX)
    (INX "Points to return address of this function")
    (INX "Now skip to return address of grandparent")
    (INX)
    (dc "Store the grandparent return address")
    (LDA.ABX #x100)
    (STA.ZP (lo-add :tmp))
    (LDA.ABX #x101)
    (STA.ZP (hi-add :tmp))
    (dc "Now we have the address of the parameter (-1)")
    (dc "Add two to it so we can skip it when parent returns")
    (CLC)
    (LDA 2)
    (ADC.ABX #x100)
    (STA.ABX #x100)
    (LDA 0)
    (ADC.ABX #x101)
    (STA.ABX #x101)
    (LDY 1 "Offset against -1 for return convention")
    (dc "Dereference the word at the parameter address")
    (LDA.IZY :tmp)
    (TAX)
    (INY)
    (LDA.IZY :tmp)
    (RTS)))

(defun mul10 ()
  "Multiply A by ten, if 0 <= A <= 25, using D0"
  (zp-b :D0)
  (ASL)
  (STA.ZP :D0)
  (ASL)
  (ASL)
  (CLC)
  (ADC.ZP :D0))

(defun test-deref-w ()
  (reset-compiler)

  (flet ((src ()
	   (org #x600)
	   
	   (label :start)
	   (zp-b :dummy1 0)
	   (zp-b :dummy2 0)
	   (zp-w :test)
	   
	   (LDA 0)
	   (STA.ZP :dummy1)

	   (sta16.zp 0 :test)
	   
	   (JSR :routine)
	   (DW nil #x1234)

	   ;;test that we get back here..

	   (LDA 5)
	   (STA.ZP :dummy1)
	   	   
	   (BRK)
	   (label :routine)

	   (JSR :deref-w)
	   (STX.ZP (lo-add :test))
	   (STA.ZP (hi-add :test))

	   ;;test that we get here
	   (LDA 6)
	   (STA.ZP :dummy2)
	   (RTS)

	   (deref-w)
	   
	   (label :end)))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))
  
  (monitor-reset #x600)
  (monitor-run :print nil)
  
  (let ((buf (monitor-buffer)))
    (assert (= 5 (aref buf 0)))
    (assert (= 6 (aref buf 1)))
    (assert (= #x34 (aref buf 2)))
    (assert (= #x12 (aref buf 3)))))

(defun test-mul10 ()
  (dotimes (v 26)
    (reset-compiler)
    ;;Note, can get away with one pass as there
    ;;are no labels etc.
    (ORG #x600)

    (LDA v)
    (MUL10)
    (BRK)
  
    (monitor-reset #x600)
    (monitor-run :print nil)

    (multiple-value-bind (buffer pc sp sr a x y)
	(funcall *monitor-get-state*)
      (declare (ignore buffer pc sp sr x y))
      (assert (= a (* v 10))))))

(defun test-inc16 (v)
  (reset-compiler)

  (flet ((src ()
	   (org #x600)

	   (zp-w :word)
	   (sta16.zp v :word)
	   (inc16.zp :word)
	   (BRK)

	   (inc16.zp :word)
	   (inc16.zp :word)

	   ))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))
  
  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (+ (aref buf (lo-add :word))
		 (* 256 (aref buf (hi-add :word))))
	       (1+ v)))))

(defun test-add16a (v a)
  (reset-compiler)

  (flet ((src ()
	   (org #x600)
	   (zp-w :word)
	   (label :start)
	   (sta16.zp v :word)
	   (LDA a)
	   (add16a.zp :word)
	   (label :end)
	   (BRK)))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))
  
  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (+ (aref buf (lo-add :word))
		 (* 256 (aref buf (hi-add :word))))
	    (+ v a)))))

(defun test-adc16a (v a c)
  (reset-compiler)

  (flet ((src ()
	   (org #x600)
	   (zp-w :word)
	   (label :start)
	   (sta16.zp v :word)
	   (LDA a)
	   (if (= 1 c)
	       (SEC)
	       (CLC))
	   (adc16a.zp :word)
	   (label :end)
	   (BRK)))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src))
  
  (monitor-reset #x600)
  (monitor-run :print nil)

  (let ((buf (monitor-buffer)))
    (assert (= (+ (aref buf (lo-add :word))
		 (* 256 (aref buf (hi-add :word))))
	    (+ v a c)))))

(defun test-nested-namespace ()
  ;; this should just not generate a failed to
  ;; resolve exception
  (reset-compiler)
  
  (flet ((src ()
	   (org #x600)
	   (zp-w :word)
	   (with-namespace :fred
	     (alias :localname :word)
	     ;; without nested namespace resolution
	     ;; localname wouldn't resolve.
	     (inc16.zp :localname))))
    (reset-compiler)
    (src)
    (setf *compiler-final-pass* t)
    (src)))

(test-nested-namespace)

(test-add16a 255 1)
(test-add16a 255 255)
(test-add16a 255 0)
(test-add16a 256 0)
(test-add16a 256 1)
(test-add16a 256 255)
(test-add16a 1 255)

(test-adc16a 255 1 0)
(test-adc16a 255 255 0)
(test-adc16a 255 0 0)
(test-adc16a 256 0 0)
(test-adc16a 256 1 0)
(test-adc16a 256 255 0)
(test-adc16a 1 255 0)

(test-adc16a 255 1 1)
(test-adc16a 255 255 1)
(test-adc16a 255 0 1)
(test-adc16a 256 0 1)
(test-adc16a 256 1 1)
(test-adc16a 256 255 1)
(test-adc16a 1 255 1)

(test-adc16a 254 1 1)
(test-adc16a 254 255 1)
(test-adc16a 254 0 1)
(test-adc16a 256 0 1)
(test-adc16a 256 1 1)
(test-adc16a 256 254 1)
(test-adc16a 1 254 1)

(test-inc16 255)
(test-inc16 0)
(test-inc16 1)


