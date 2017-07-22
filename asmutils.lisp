(defun inc16.zp (label)
  "Increment a zero-page word"
  (INC.ZP (lo-add label))
  ;;Once local namespaces are really working then
  ;;we can use a label here
  (BNE 2)
  (INC.ZP (hi-add label)))

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

(defun mul10 ()
  "Multiply A by ten, if 0 <= A <= 25, using D0"
  (zp-b :D0)
  (ASL)
  (STA.ZP :D0)
  (ASL)
  (ASL)
  (CLC)
  (ADC.ZP :D0))

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
