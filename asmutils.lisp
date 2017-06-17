;todo nested namespaces?
;todo local function instance namespaces
;the difficulty is identifying the instance
;of the macro next time round. If we assume
;that all instances are seen in the first
;pass we could keep a counter. 

(defun inc16.zp (label)
  "Increment a zero-page word"
  (INC.ZP (lo-add label))
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

(defun sta16.zp (addr zpg)
  "Load an address into a zero page word"
  (LDA (lo addr))
  (STA.ZP (lo-add zpg))
  (LDA (hi addr))
  (STA.ZP (hi-add zpg)))


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
