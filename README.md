# 6502 Exploratory Code

## Assembler

A 6502 assembler for Lisp. For simplicity the addressing modes are explicit rather than based on syntax. Syntax is dull. All lisp constructs are allowed- each 6502 instruction maps to a lisp function each of which emits bytes into a buffer.

Compilation requires (at least) two passes. The same code executes the same way for each pass, except that a flag can be set to ensure that all labels have been resolved. 

Perhaps a third pass can be added e.g. to convert long branches to jmps. This is under the user's control, as the assembler is a library of functions rather than a monolith.

~~~~
    (org #x0000)

    (db :variable 0)
    (db :another-variable 1)

    (org #x0600)
    
    (label :start)
    (BCC :the-future)
    (CLD)
    (CLD)
    (label :the-future)
    (CLD)
    (STA.ZP :another-variable)
    (STA.AB :a-non-zpg-variable)
    (JMP :start)
    (RTS)
    (ds nil "X")
    (LDA.IMM (lo :start))
    (PHA)
    (LDA.IMM (hi :start))
    (PHA)
    (RTS)
    
    (ds nil "Tetradic Chronisms")
    (db :a-non-zpg-variable #x55))
~~~~

## Disassembler

Maybe it will be useful.

~~~~

(disassemble-6502 buffer #x600 32)

START     0600 9002    BCC $0604
          0602 D8      CLD
          0603 D8      CLD
THE-FUTUR 0604 D8      CLD
          0605 8501    STA $01
          0607 8D2A06  STA $062A
          060A 4C0006  JMP $0600
          060D 60      RTS
          060E 58      CLI
          060F 00      BRK
          0610 A900    LDA #$00
          0612 48      PHA
          0613 A906    LDA #$06
          0615 48      PHA
          0616 60      RTS
          0617 54
          0618 6574    ADC $74
          061A 72
          061B 61
          061C 64
          061D 6963    ADC #$63
          061F 204368  JSR $6843
~~~~

## Hexdump

The hexdump function is definitely useful

~~~~

(hexdump buffer #x600 100)

0600 9002 D8D8 D885 018D 2A06 4C00 0660 5800 ........*.L..`X.
0610 A900 48A9 0648 6054 6574 7261 6469 6320 ..H..H`Tetradic 
0620 4368 726F 6E69 736D 7300 5500 0000 0000 Chronisms.U.....
0630 0000 0000 0000 0000 0000 0000 0000 0000 ................
~~~~
