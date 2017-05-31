## 31/5/2017 8 bit multiplication by 10

Let's say you have a bunch of characters that are 10 pixels high and you want to do a multiplication to look up the address of the bytes where you are keeping them. Well, the first step, I thought was to see if we can do multiplication *at all* on the 6502. Here is a new instruction, MUL10, which does it.

Having said that it will be pretty useless as we need a 16 bit address if we are to have any hope of rendering it to the screen.

Anyway, here is a function to multiply by 10, well actually a function that will emit some 6502 machine code to multiply by ten, followed by a function which does it, runs it through an emulator and checks that it actually works.

~~~~

(defun MUL10 ()
  "Multiply A by ten, if 0 <= A <= 25"
  (zp-b :scratch)
  (ASL)
  (STA.ZP :scratch)
  (ASL)
  (ASL)
  (CLC)
  (ADC.ZP :scratch))

(defun test-MUL10 ()
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

~~~~

Note the scratch reservation with zp-b, scratch is going to be a byte that may change at any time. Except in an interrupt, where we should probably take care not to use it. Because it is lisp and the compiler labels are variables, we can do this instead, though it is Yaggers atm.

~~~~

(let ((*scratch-label* :iscratch))
	;dangerous interrupt code here
	...
	(MUL10)
	...)
	
(defun MUL10 ()
  "Multiply A by ten, if 0 <= A <= 25"
  (zp-b *scratch-label*)
  (ASL)
  (STA.ZP *scratch-label*)
  
~~~~

## 30/5/2017 Label namespaces, VICKY and YAGNI

It would be nice to use the same generic names for labels when writing assembly language, e.g. next, done, start, end. To this end, I added namespaces. Using the macro with-namespace instructs the assembler to try to resolve all labels in the specified namespace. If it can't find one, it tries the global namespace. The label directive will apply the label to the currently active namespace unless told otherwise by an optional namespace parameter. This is useful for 'exposing' a label in the middle of a namespace block. Of course, it isn't really exposing anything, since it the label is available everywhere with its qualifier. The qualifier is added by consing it to the label; I will probably change this to something nicer if it gets in the way. If it doesn't get in the way then I won't, as it is YAGNI, which from now on will be referred to as yaggers.

~~~~
 (with-namespace :decode-string

      (zp-b :sym 0)
      
      (label :next)
      (INC.ZP (lo-add :a-global-string-add))
      (BNE :decode-string)
      (INC.ZP (hi-add :a-global-string-add))
      (BEQ :done)
      (label :decode-string nil) ; in the global namespace
      (LDY.IMM #x0)
      (LDA.IZY :decode-string-add)
      (BEQ :done)
      (CMP.IMM first-three-char)
      (BCC :2char)
      (STA.ZP :sym)
~~~~

### VICKY

VICKY (I was going to go for V.I.C.K.Y. Perhaps there's a movie in there somewhere about a regular schoolgirl who is actually a robot) is a view of an mmap'ed section of memory, rendered to the screen in the washed out palette of the VIC-II fromm the C64. This is giving the 6502 code something to target. For now, it will just render the hi-res bitmap mode, 320x200 which I plan on using for the adventure game.

Look, here is a test rendering I dumped in using a 'medieval' style font. Sadly I think that at 12 pixels high, it will be too big, there being room for only 15 lines (the original, 8-bit break on which it was based is even bigger). Note the horrific horizontal spacing- variable width rendering of fonts is next on the agenda. For some of you, the brown and yellow colour scheme will bring back memories redolent of languorous summers spent indoors waiting for tapes to load. I had a Spectrum, so I feel nothing. Perhaps resentment, not greatly dulled by time.

![Alt text](/capture.png)

UPDATE - As you can see three additional fonts have been added, 10 pixels high. There are some flaws. But, meh, fix it when it needs fixing. It will be okay at the sanding stage.

## 27/5/2017 Roundtrip encoding strings

Compressed strings can be added to the program with the assembler command dcs, e.g.

~~~~
(dcs :my-string "This is a string")
~~~~

After one full pass, all strings have been processed and a symbol table built. After a second pass, the strings can be encoded into literal bytes. There is a validation run which checks (in Lisp) that in theory the strings can all be decoded, but it would be nice to check that the 6502 routine for decoding actually does what it says on the tin.

~~~~
  (compile-string-test :test-str)

  (dolist (str *compiled-strings*)

    (compile-string-test (car str))
    (monitor-reset #x600)
    (monitor-run :print nil)

    (let ((*compiler-buffer* (monitor-buffer)))
      (let ((output 
	     (map 'string #'code-char
		  (subseq *compiler-buffer*
			  (resolve :str-buffer)
			  (position 0 *compiler-buffer* :start (resolve :str-buffer))))))
	(format t "~a~%" output)
	(assert (equal output (cdr str)))))))
~~~~

This function compiles a string test program to 6502 once, and as a side-effect grabs the list of strings that were included. The test program contains a routine that simply emits encoded strings into a buffer. It then compiles and executes (using a monitor attached to a suitable emulator) the 6502 machine code, once for every string in the program. The buffer is then checked against the actual string to see if it matches.

As far as 'unit' tests go, this is as perfect as it gets; for the simple reason that the decoder will *never* be expected to handle any string that hasn't already been tested. It will encounter only the strings that have been explicitly built into the program.
