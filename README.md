# 6502 Exploratory Code

## Assembler

A 6502 assembler for Lisp. For simplicity the addressing modes are explicit rather than based on syntax. Syntax is dull. All lisp constructs are allowed- each 6502 instruction maps to a Lisp function each of which emits bytes into a buffer.

Compilation requires (at least) two passes. The same code executes the same way for each pass, except that a flag can be set to ensure that all labels have been resolved. 

Perhaps a third pass can be added e.g. to convert long branches to jmps. This is under the user's control, as the assembler is a library of functions rather than a monolith. Just keep looping over and over again, if you feel it helps. Of course, using some macrology and lambdas and whatnot, this could all be done in one pass, using delayed evaluation. But where would the environment be? What values would the variables take? I'm sure it could be done in Scheme with continuations. But then, you couldn't take advantage of the output of the first run. This is why two or more simple but inefficient passes are better than one incredibly complex but inefficient pass.

- Zeropage addresses can be reserved. They could be reserved in one place by doing org #x0000 and db, etc, but this way you can reserve them in the place that is interested in them as merrily as you like. Until you have used them all, and your compilation will fail.

~~~~

    (org #x0600)

    (reserve-zp-b :variable 0)
    (reserve-zp-b :another-variable 1)
    (reserve-zp-b :lbl1 0)

    (label :start)
    
    (ORA.IZX :lbl1)
    (ORA.IZY :lbl1)
    (ROR)
    (LSR)
    (dc "This is a comment")
    (ROL)
    (ASL)
    (BCC :the-future)
    (JMP :over-some-text)
    (DS nil "Scrozzbot")
    (label :over-some-text)
    (CLD)
    (CLD)
    (label :the-future)
    (CLD)
    (STA.ZP :another-variable)
    (STA.AB :a-non-zpg-variable)
    (JMP :start)
    (RTS)
    (dw :words #x1234 #x5678 #xABCD)
    (db :bytes #x01 #x02 #x03)
    (BRK)
    (LDA.IMM (lo :start))
    (PHA)
    (LDA.IMM (hi :start))
    (PHA)
    (RTS)
    
    (ds nil "Tetradic Chronisms")
    (db :a-non-zpg-variable #x55)
    (NOP)
    (label :end)
    
~~~~

I wouldn't read to much into that particular listing, it's mostly nonsense.

## Disassembler

I did think this was going to be YAGNI, but the output looks nice and not everything in life has to be necessary. Maybe it will be useful. Print it on fan-fold for the full effect.

- Labels are in the left column
- Hints can be supplied by the compiler so db, dw, ds etc can be recovered. User can apply hints with the function apply-hint Open for extension, open for modification.
- I'm not adding anything else to this except maybe labels in the right hand column
- Ok, I added comments to the hint section

~~~~
(disassemble-6502 :start :end)

START     0600 0102    ORA ($02,X)
          0602 1102    ORA ($02),Y
          0604 6A      ROR
          0605 4A      LSR
          ;This is a comment
          0606 2A      ROL
          0607 0A      ASL
          0608 900F    BCC $0619
          060A 4C1706  JMP $0617
          060D 53      DS "Scrozzbot"
OVER-SOME 0617 D8      CLD
          0618 D8      CLD
THE-FUTUR 0619 D8      CLD
          061A 8501    STA $01
          061C 8D4706  STA $0647
          061F 4C0006  JMP $0600
          0622 60      RTS
WORDS     0623 34      DW $1234, $5678, $ABCD
BYTES     0629 01      DB $01, $02, $03
          062C 00      BRK
          062D A900    LDA #$00
          062F 48      PHA
          0630 A906    LDA #$06
          0632 48      PHA
          0633 60      RTS
          0634 54      DS "Tetradic Chronisms"
A-NON-ZPG 0647 55      DB $55
          0648 EA      NOP
END       0649 00      BRK

(disassemble-6502 0 10)

VARIABLE  0000 00      DB $00
ANOTHER-V 0001 01      DB $01
LBL1      0002 00      DB $00
          0003 00      BRK
~~~~

## Hexdump

The hexdump function is definitely useful. I used this to check the output of the assembler against that of Skilldrick who has made an excellent page at https://skilldrick.github.io/easy6502/

~~~~

(hexdump buffer #x600 100)

0600 9002 D8D8 D885 018D 2A06 4C00 0660 5800 ........*.L..`X.
0610 A900 48A9 0648 6054 6574 7261 6469 6320 ..H..H`Tetradic 
0620 4368 726F 6E69 736D 7300 5500 0000 0000 Chronisms.U.....
0630 0000 0000 0000 0000 0000 0000 0000 0000 ................
~~~~

## Monitor

Combine the hexdump and the disassembly plus an emulator of your choice and you get a monitor. Some exciting watches have been added to the zero page, and $60C, which is a buffer being filled by a string. I did think that adding comments was mere whimsy, but it is actually very helpful when you are debugging. I haven't output labels in the argument column yet. It's quite exciting, because you only see the hexadecimal, you never know quite where you will end up.

~~~~

CL-USER> (monitor-step)
-- Stack ----------------------------------------------------
01F1 D306 0A06 0000 0000 0000 0000 0000 0000 ................
-- Watches --------------------------------------------------
0000 0B59 150A 0000 0000 0000 0000 0000 0000 .Y..............
060C 5468 6973 2069 7320 7468 6520 2020 2020 This is the     
-- PC -------------------------------------------------------
06A4 E600 60E6 02D0 04E6 00F0 4BA0 00B1 02F0 ..`.......K.....
06B4 45C9 5790 1E85 01AA BDA4 0620 F106 A601 E.W........ ....
-------------------------------------------------------------
                          SV BDIZC
       PC:06A4 SP:1F0  SR:00110101 A:20 X:01 Y:0B
-------------------------------------------------------------
          06A4 E600    INC $00
          06A6 60      RTS
RSTR-NXT  06A7 E602    INC $02
          06A9 D004    BNE $06AF
          06AB E600    INC $00

~~~~

## Shannon-Fano

When I was a teenager I used to while away the long hot summers chasing ever better compression ratios for storing games on 720K floppies. Then I learned about the pigeon-hole principle and my dreams were shattered. Also, it turned out that a couple of chaps called Lempel and Ziv had already figured this stuff out.

For this adventure game we will need to compress text. I decided that Huffman, with a bit of bang in the form of multiple character symbols would do. The text will be compressed at compilation stage, then a tree of 6502 branch instructions and a couple of RORs will decompress.

~~~~

e 1500  00
t 1000  01    
a 700   10
i 32    110
o 12    111

~~~~~

So the algorithm is as follows

- Split the symbols in half, based on frequency
- Half will get 0, the other half will get 1
- Repeat, for each half, splitting furiously until we can split no more

Except it turns out this algorithm is called Shannon-Fano and not Huffman. Huffman starts at the least frequent symbols and joins them together, Shannon at the most frequent. Shannon-Fano is less efficient than Huffman but its cooler name more than makes up for it.

## Tunstall

Clearly still bitten by the bug of implementing jet-age compression algorithms in a sixties language compiling to a seventies chip for the purposes of building an eighties text adventure, I tried one more routine. Tunstall, a fixed length code, with a table of words. A byte is a fixed length code, so I tried that.

~~~~

(tunstall "Clearly still bitten.... " 3)

$00 "ge " $01 "ent" $02 "ing" $03 "of " $04 "he " $05 " co" $06 " a " $07 "ven"
$08 " th" $09 "es " $0A "a s" $0B "age" $0C "ies" $0D "the" $0E "n a" $0F "tie"
$10 " of" $11 "ng " $12 "e c" $13 "omp" $14 "com" $15 " bu" $16 "nti" $17 "it"
...
$F7 "s"   $F8 "?"   $F9 "?"   $FA "a"   $FB "?"   $FC "?"   $FD "?"   $FE "l"

~~~~

Tunstall gets the best results for maximum word size of three, no surprise really because it is the forerunner to LZW. LZ as in Lempel-Ziv. As I said, those guys had it all figured out (along with Welch in this instance). And so Tunstall is the winner as it is so simple.

## String Decoding

Here is a listing to decode the strings. Can you imagine having to have typed this out in the eighties? All we are doing here is pushing the characters into a buffer, but in the next phase, the emit functions will be rendering variable width fonts to a really lo-res screen mode. Probably one of the C-64 screen modes.

Metaprogramming is a lot of fun. We can change the strings and new tables will be generated autoomatically. Here the string decoder and the emit functions have been separated in the 'compiler' but are together in the output. Why don't we do this for modern programs? Ah yes, that's right, we have OO, JIT and SOLID design principles so it is entirely acceptable to have a monolithic compiler.

~~~~

CL-USER> (disassemble-6502 :start :str-buffer)
          ;Decode a string into a buffer
START     0600 A912    LDA #$12
          0602 8502    STA $02
          0604 A90A    LDA #$0A
          0606 8503    STA $03
          0608 20AF06  JSR $06AF
          060B 00      BRK
STR-BUFFE 060C 20      DS "                "
; No value
CL-USER> (disassemble-6502 :start :end)
          ;Emit a string into a buffer
START     0600 A912    LDA #$12
          0602 8502    STA $02
          0604 A90A    LDA #$0A
          0606 8503    STA $03
          0608 20AF06  JSR $06AF
          060B 00      BRK
STR-BUFFE 060C 20      DS "                "
          ;Each character must have its own labelled function
          ;e.g. to render to the screen.
          ;In this test program we will just
          ;write it to memory at an index
EMIT-     061D A900    LDA #$00
          061F 4C9F06  JMP $069F
EMIT-     0622 A920    LDA #$20
          0624 4C9F06  JMP $069F
EMIT-,    0627 A92C    LDA #$2C
          0629 4C9F06  JMP $069F
EMIT-T    062C A954    LDA #$54
          062E 4C9F06  JMP $069F
EMIT-a    0631 A961    LDA #$61
          0633 4C9F06  JMP $069F
EMIT-b    0636 A962    LDA #$62
          0638 4C9F06  JMP $069F
EMIT-c    063B A963    LDA #$63
          063D 4C9F06  JMP $069F
EMIT-d    0640 A964    LDA #$64
          0642 4C9F06  JMP $069F
EMIT-e    0645 A965    LDA #$65
          0647 4C9F06  JMP $069F
EMIT-f    064A A966    LDA #$66
          064C 4C9F06  JMP $069F
EMIT-g    064F A967    LDA #$67
          0651 4C9F06  JMP $069F
EMIT-h    0654 A968    LDA #$68
          0656 4C9F06  JMP $069F
EMIT-i    0659 A969    LDA #$69
          065B 4C9F06  JMP $069F
EMIT-l    065E A96C    LDA #$6C
          0660 4C9F06  JMP $069F
EMIT-m    0663 A96D    LDA #$6D
          0665 4C9F06  JMP $069F
EMIT-n    0668 A96E    LDA #$6E
          066A 4C9F06  JMP $069F
EMIT-o    066D A96F    LDA #$6F
          066F 4C9F06  JMP $069F
EMIT-p    0672 A970    LDA #$70
          0674 4C9F06  JMP $069F
EMIT-q    0677 A971    LDA #$71
          0679 4C9F06  JMP $069F
EMIT-r    067C A972    LDA #$72
          067E 4C9F06  JMP $069F
EMIT-s    0681 A973    LDA #$73
          0683 4C9F06  JMP $069F
EMIT-t    0686 A974    LDA #$74
          0688 4C9F06  JMP $069F
EMIT-u    068B A975    LDA #$75
          068D 4C9F06  JMP $069F
EMIT-v    0690 A976    LDA #$76
          0692 4C9F06  JMP $069F
EMIT-w    0695 A977    LDA #$77
          0697 4C9F06  JMP $069F
EMIT-y    069A A979    LDA #$79
          069C 4C9F06  JMP $069F
EMIT      069F A400    LDY $00
          06A1 990C06  STA $060C,Y
          06A4 E600    INC $00
          06A6 60      RTS
RSTR-NXT  06A7 E602    INC $02
          06A9 D004    BNE $06AF
          06AB E600    INC $00
          06AD F04B    BEQ $06FA
          ;Render the string at RSTR-ADD
RSTR      06AF A000    LDY #$00
          06B1 B102    LDA ($02),Y
          06B3 F045    BEQ $06FA
          06B5 C957    CMP #$57
          06B7 901E    BCC $06D7
          06B9 8501    STA $01
          06BB AA      TAX
          ;Get the first character from the three character table
          ;note that the address to :3ch-0 is offset, so that we
          ;can use the value in A without subtracting
          06BC BDA406  LDA $06A4,Y
          06BF 20F106  JSR $06F1
          06C2 A601    LDX $01
          06C4 BD4D07  LDA $074D,Y
          06C7 20F106  JSR $06F1
          06CA A601    LDX $01
          06CC BDF607  LDA $07F6,Y
          ;The third character might be EOS
          06CF F029    BEQ $06FA
          06D1 20F106  JSR $06F1
          06D4 4CA706  JMP $06A7
2CHAR     06D7 C91A    CMP #$1A
          06D9 B010    BCS $06EB
          06DB 8501    STA $01
          06DD AA      TAX
          06DE BDDC08  LDA $08DC,Y
          06E1 20F106  JSR $06F1
          06E4 A601    LDX $01
          06E6 BD1909  LDA $0919,Y
          ;The second character might be EOS
          06E9 F00F    BEQ $06FA
1CHAR     06EB 20F106  JSR $06F1
          06EE 4CA706  JMP $06A7
          ;Look up address of rendering function in jump table
RSTR-EMIT 06F1 AA      TAX
          06F2 BD8A09  LDA $098A,Y
          06F5 48      PHA
          06F6 BD7009  LDA $0970,Y
          06F9 48      PHA
RSTR-DONE 06FA 60      RTS
          ;Three character string table split into three
          ;so that each character can be retrieved by indexing
          ;without multiplication
3CH-0     06FB 01      DB $01, $15, $0B, $0C, $15, $14, $13, $01, $0C, $05, $08, $01, $04, $0F, $01, $0C, $01, $11, $04, $0F, $0F, $0F, $01, $15, $0A, $14, $06, $14, $01, $01, $10, $05, $01, $0D, $0D, $01, $01, $18, $07, $14, $0C, $04, $03, $01, $14, $0D, $08, $0B, $14, $0B, $0D, $0A, $05, $16, $08, $0D, $07, $04, $04, $05, $08, $0B, $0C, $01, $01, $04, $11, $08, $04, $13, $14, $01, $08, $01, $14, $13, $10, $0A, $01, $14, $17, $04, $06, $04, $10, $15, $10, $14, $0B, $0C, $02, $13, $08, $13, $01, $08, $19, $14, $0C, $08, $08, $08, $01, $08, $0C, $04, $0D, $0B, $03, $0F, $0F, $04, $0D, $14, $19, $10, $14, $08, $04, $07, $0B, $18, $01, $13, $0D, $08, $08, $01, $07, $04, $13, $08, $0A, $09, $11, $12, $13, $0C, $13, $01, $15, $04, $16, $06, $15, $0C, $11, $0F, $14, $09, $16, $08, $01, $18, $04, $13, $06, $10, $0D, $0E, $11, $0D, $0A, $0C, $0D, $19, $08, $16, $0F
3CH-1     07A4 15      DB $15, $0B, $08, $0F, $13, $15, $0C, $14, $14, $0D, $01, $0C, $05, $0A, $15, $0F, $11, $04, $14, $0A, $07, $01, $05, $04, $01, $01, $0B, $01, $04, $10, $09, $08, $04, $08, $0D, $05, $18, $0C, $01, $14, $0D, $0F, $0B, $0C, $15, $08, $01, $01, $01, $0C, $01, $14, $16, $0C, $07, $07, $01, $01, $01, $16, $01, $0C, $06, $14, $04, $11, $11, $04, $13, $01, $01, $17, $01, $11, $10, $10, $0A, $13, $14, $08, $04, $01, $10, $13, $0F, $0B, $01, $14, $0C, $13, $01, $0C, $01, $07, $04, $19, $01, $01, $04, $14, $01, $01, $0F, $01, $0D, $0F, $15, $08, $0B, $10, $04, $0D, $19, $02, $14, $18, $08, $07, $13, $01, $08, $01, $13, $08, $08, $06, $12, $15, $01, $0E, $04, $0F, $14, $0C, $13, $16, $08, $13, $14, $04, $01, $06, $0C, $15, $16, $13, $08, $0A, $01, $01, $04, $01, $06, $0B, $0D, $08, $10, $0E, $0D, $11, $0C, $19, $15, $0D, $08, $01, $13, $0F, $06
3CH-2     084D 0B      DB $0B, $08, $01, $0A, $0C, $13, $0F, $15, $01, $08, $14, $14, $0D, $01, $04, $01, $04, $14, $14, $14, $01, $15, $16, $05, $15, $0C, $01, $15, $01, $09, $01, $01, $0F, $01, $01, $08, $0C, $0D, $11, $01, $0D, $07, $0C, $0F, $01, $00, $15, $04, $04, $14, $05, $00, $0C, $0D, $00, $01, $04, $14, $05, $0F, $0C, $06, $0B, $10, $11, $11, $08, $13, $01, $0C, $17, $04, $11, $13, $01, $0A, $13, $04, $08, $06, $13, $15, $0F, $0C, $07, $0C, $04, $02, $13, $07, $15, $04, $00, $01, $13, $01, $18, $0F, $05, $15, $18, $05, $10, $04, $15, $04, $00, $19, $08, $18, $0D, $19, $14, $01, $08, $01, $07, $01, $08, $0C, $00, $13, $08, $12, $0F, $10, $16, $08, $15, $00, $0E, $0A, $01, $13, $10, $0C, $01, $14, $15, $06, $11, $15, $13, $16, $04, $08, $04, $15, $10, $15, $0D, $06, $10, $0C, $0D, $07, $0E, $11, $19, $0C, $0D, $01, $0B, $08, $13, $08, $00, $06, $0B
          ;Two character string table
2CH-0     08F6 01      DB $01, $08, $15, $0C, $0B, $14, $14, $13, $0F, $01, $01, $15, $0D, $01, $07, $0C, $01, $0C, $04, $0B, $04, $01, $05, $0A, $0F, $13, $05, $04, $08, $08, $0D, $0C, $11, $14, $03, $06, $0B, $04, $16, $0A, $0F, $15, $04, $10, $14, $18, $0D, $05, $09, $0F, $14, $10, $04, $01, $0D, $08, $15, $07, $19, $06, $01
2CH-1     0933 15      DB $15, $01, $0B, $0F, $08, $01, $15, $0C, $0A, $14, $04, $13, $08, $0C, $01, $14, $05, $0D, $14, $0C, $05, $11, $0D, $14, $07, $08, $16, $0F, $00, $07, $0D, $13, $04, $14, $0B, $10, $01, $01, $0C, $01, $01, $04, $13, $01, $00, $0C, $01, $08, $01, $06, $08, $09, $0D, $18, $19, $0F, $01, $00, $01, $0B, $10
          ;Addresses for the character jump table in two tables
          ;hi-byte and lo-byte, less one, for use by rts
1CH-LO    0970 1C      DB $1C, $21, $26, $2B, $30, $35, $3A, $3F, $44, $49, $4E, $53, $58, $5D, $62, $67, $6C, $71, $76, $7B, $80, $85, $8A, $8F, $94, $99
1CH-HI    098A 06      DB $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
          09A4 81      DCS 'This is a bunch of strings'
          09AD F0      DCS 'which appear in the program'
          09B7 C3      DCS 'They will be analysed in the'
          09C1 DC      DCS 'first pass of the compiler'
          09CA 15      DCS 'to build a string table'
          09D3 66      DCS 'in the second pass, the '
          09DC 5C      DCS 'string table will be built'
          09E5 80      DCS 'and the strings actually encoded'
          09F2 A8      DCS 'a third pass is now required'
          09FC 2C      DCS 'as the string table is variable'
          0A07 D3      DCS 'length and so are the strings'
STR       0A12 81      DCS 'This is the test string'
END       0A1C 00      BRK

~~~~





