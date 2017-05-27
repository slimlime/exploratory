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

CL-USER> (disassemble-6502 :start :end)
          ;Emit a string into a buffer
START     0600 A928    LDA #$28
          0602 8502    STA $02
          0604 A90A    LDA #$0A
          0606 8503    STA $03
          0608 20C506  JSR $06C5             ;RSTR
          060B 00      BRK
STR-BUFFE 060C 20      DS "                                      "
          ;Each character must have its own labelled function
          ;e.g. to render to the screen.
          ;In this test program we will just
          ;write it to memory at an index
EMIT-null    0633 A900    LDA #$00
          0635 4CB506  JMP $06B5             ;EMIT
EMIT-     0638 A920    LDA #$20
          063A 4CB506  JMP $06B5             ;EMIT
EMIT-,    063D A92C    LDA #$2C
          063F 4CB506  JMP $06B5             ;EMIT
EMIT-T    0642 A954    LDA #$54
          0644 4CB506  JMP $06B5             ;EMIT
EMIT-a    0647 A961    LDA #$61
          0649 4CB506  JMP $06B5             ;EMIT
EMIT-b    064C A962    LDA #$62
          064E 4CB506  JMP $06B5             ;EMIT
EMIT-c    0651 A963    LDA #$63
          0653 4CB506  JMP $06B5             ;EMIT
EMIT-d    0656 A964    LDA #$64
          0658 4CB506  JMP $06B5             ;EMIT
EMIT-e    065B A965    LDA #$65
          065D 4CB506  JMP $06B5             ;EMIT
EMIT-f    0660 A966    LDA #$66
          0662 4CB506  JMP $06B5             ;EMIT
EMIT-g    0665 A967    LDA #$67
          0667 4CB506  JMP $06B5             ;EMIT
EMIT-h    066A A968    LDA #$68
          066C 4CB506  JMP $06B5             ;EMIT
EMIT-i    066F A969    LDA #$69
          0671 4CB506  JMP $06B5             ;EMIT
EMIT-l    0674 A96C    LDA #$6C
          0676 4CB506  JMP $06B5             ;EMIT
EMIT-m    0679 A96D    LDA #$6D
          067B 4CB506  JMP $06B5             ;EMIT
EMIT-n    067E A96E    LDA #$6E
          0680 4CB506  JMP $06B5             ;EMIT
EMIT-o    0683 A96F    LDA #$6F
          0685 4CB506  JMP $06B5             ;EMIT
EMIT-p    0688 A970    LDA #$70
          068A 4CB506  JMP $06B5             ;EMIT
EMIT-q    068D A971    LDA #$71
          068F 4CB506  JMP $06B5             ;EMIT
EMIT-r    0692 A972    LDA #$72
          0694 4CB506  JMP $06B5             ;EMIT
EMIT-s    0697 A973    LDA #$73
          0699 4CB506  JMP $06B5             ;EMIT
EMIT-t    069C A974    LDA #$74
          069E 4CB506  JMP $06B5             ;EMIT
EMIT-u    06A1 A975    LDA #$75
          06A3 4CB506  JMP $06B5             ;EMIT
EMIT-v    06A6 A976    LDA #$76
          06A8 4CB506  JMP $06B5             ;EMIT
EMIT-w    06AB A977    LDA #$77
          06AD 4CB506  JMP $06B5             ;EMIT
EMIT-y    06B0 A979    LDA #$79
          06B2 4CB506  JMP $06B5             ;EMIT
EMIT      06B5 A400    LDY $00
          06B7 990C06  STA $060C,Y           ;STR-BUFFER
          06BA E600    INC $00
          06BC 60      RTS
RSTR-NXT  06BD E602    INC $02
          06BF D004    BNE $06C5             ;RSTR
          06C1 E603    INC $03
          06C3 F04B    BEQ $0710             ;RSTR-DONE
          ;Render the string at RSTR-ADD
RSTR      06C5 A000    LDY #$00
          06C7 B102    LDA ($02),Y
          06C9 F045    BEQ $0710             ;RSTR-DONE
          06CB C957    CMP #$57
          06CD 901E    BCC $06ED             ;2CHAR
          06CF 8501    STA $01
          06D1 AA      TAX
          ;Get the first character from the three character table
          ;note that the address to :3ch-0 is offset, so that we
          ;can use the value in A without subtracting
          06D2 BDBA06  LDA $06BA,Y
          06D5 200707  JSR $0707             ;RSTR-EMIT
          06D8 A601    LDX $01
          06DA BD6307  LDA $0763,Y
          06DD 200707  JSR $0707             ;RSTR-EMIT
          06E0 A601    LDX $01
          06E2 BD0C08  LDA $080C,Y
          ;The third character might be EOS
          06E5 F029    BEQ $0710             ;RSTR-DONE
          06E7 200707  JSR $0707             ;RSTR-EMIT
          06EA 4CBD06  JMP $06BD             ;RSTR-NXT
2CHAR     06ED C91A    CMP #$1A
          06EF 9010    BCC $0701             ;1CHAR
          06F1 8501    STA $01
          06F3 AA      TAX
          06F4 BDF208  LDA $08F2,Y
          06F7 200707  JSR $0707             ;RSTR-EMIT
          06FA A601    LDX $01
          06FC BD2F09  LDA $092F,Y
          ;The second character might be EOS
          06FF F00F    BEQ $0710             ;RSTR-DONE
1CHAR     0701 200707  JSR $0707             ;RSTR-EMIT
          0704 4CBD06  JMP $06BD             ;RSTR-NXT
          ;Look up address of rendering function in jump table
RSTR-EMIT 0707 AA      TAX
          0708 BDA009  LDA $09A0,Y           ;1CH-HI
          070B 48      PHA
          070C BD8609  LDA $0986,Y           ;1CH-LO
          070F 48      PHA
RSTR-DONE 0710 60      RTS
          ;Three character string table split into three
          ;so that each character can be retrieved by indexing
          ;without multiplication
3CH-0     0711 01      DB $01, $15, $0B, $0C, $15, $14, $13, $01, $0C, $05, $08, $01, $04, $0F, $01, $0C, $01, $11, $04, $0F, $0F, $0F, $01, $15, $0A, $14, $06, $14, $01, $01, $10, $05, $01, $0D, $0D, $01, $01, $18, $07, $14, $0C, $04, $03, $01, $14, $0D, $08, $0B, $14, $0B, $0D, $0A, $05, $16, $08, $0D, $07, $04, $04, $05, $08, $0B, $0C, $01, $01, $04, $11, $08, $04, $13, $14, $01, $08, $01, $14, $13, $10, $0A, $01, $14, $17, $04, $06, $04, $10, $15, $10, $14, $0B, $0C, $02, $13, $08, $13, $01, $08, $19, $14, $0C, $08, $08, $08, $01, $08, $0C, $04, $0D, $0B, $03, $0F, $0F, $04, $0D, $14, $19, $10, $14, $08, $04, $07, $0B, $18, $01, $13, $0D, $08, $08, $01, $07, $04, $13, $08, $0A, $09, $11, $12, $13, $0C, $13, $01, $15, $04, $16, $06, $15, $0C, $11, $0F, $14, $09, $16, $08, $01, $18, $04, $13, $06, $10, $0D, $0E, $11, $0D, $0A, $0C, $0D, $19, $08, $16, $0F
3CH-1     07BA 15      DB $15, $0B, $08, $0F, $13, $15, $0C, $14, $14, $0D, $01, $0C, $05, $0A, $15, $0F, $11, $04, $14, $0A, $07, $01, $05, $04, $01, $01, $0B, $01, $04, $10, $09, $08, $04, $08, $0D, $05, $18, $0C, $01, $14, $0D, $0F, $0B, $0C, $15, $08, $01, $01, $01, $0C, $01, $14, $16, $0C, $07, $07, $01, $01, $01, $16, $01, $0C, $06, $14, $04, $11, $11, $04, $13, $01, $01, $17, $01, $11, $10, $10, $0A, $13, $14, $08, $04, $01, $10, $13, $0F, $0B, $01, $14, $0C, $13, $01, $0C, $01, $07, $04, $19, $01, $01, $04, $14, $01, $01, $0F, $01, $0D, $0F, $15, $08, $0B, $10, $04, $0D, $19, $02, $14, $18, $08, $07, $13, $01, $08, $01, $13, $08, $08, $06, $12, $15, $01, $0E, $04, $0F, $14, $0C, $13, $16, $08, $13, $14, $04, $01, $06, $0C, $15, $16, $13, $08, $0A, $01, $01, $04, $01, $06, $0B, $0D, $08, $10, $0E, $0D, $11, $0C, $19, $15, $0D, $08, $01, $13, $0F, $06
3CH-2     0863 0B      DB $0B, $08, $01, $0A, $0C, $13, $0F, $15, $01, $08, $14, $14, $0D, $01, $04, $01, $04, $14, $14, $14, $01, $15, $16, $05, $15, $0C, $01, $15, $01, $09, $01, $01, $0F, $01, $01, $08, $0C, $0D, $11, $01, $0D, $07, $0C, $0F, $01, $00, $15, $04, $04, $14, $05, $00, $0C, $0D, $00, $01, $04, $14, $05, $0F, $0C, $06, $0B, $10, $11, $11, $08, $13, $01, $0C, $17, $04, $11, $13, $01, $0A, $13, $04, $08, $06, $13, $15, $0F, $0C, $07, $0C, $04, $02, $13, $07, $15, $04, $00, $01, $13, $01, $18, $0F, $05, $15, $18, $05, $10, $04, $15, $04, $00, $19, $08, $18, $0D, $19, $14, $01, $08, $01, $07, $01, $08, $0C, $00, $13, $08, $12, $0F, $10, $16, $08, $15, $00, $0E, $0A, $01, $13, $10, $0C, $01, $14, $15, $06, $11, $15, $13, $16, $04, $08, $04, $15, $10, $15, $0D, $06, $10, $0C, $0D, $07, $0E, $11, $19, $0C, $0D, $01, $0B, $08, $13, $08, $00, $06, $0B
          ;Two character string table
2CH-0     090C 01      DB $01, $08, $15, $0C, $0B, $14, $14, $13, $0F, $01, $01, $15, $0D, $01, $07, $0C, $01, $0C, $04, $0B, $04, $01, $05, $0A, $0F, $13, $05, $04, $08, $08, $0D, $0C, $11, $14, $03, $06, $0B, $04, $16, $0A, $0F, $15, $04, $10, $14, $18, $0D, $05, $09, $0F, $14, $10, $04, $01, $0D, $08, $15, $07, $19, $06, $01
2CH-1     0949 15      DB $15, $01, $0B, $0F, $08, $01, $15, $0C, $0A, $14, $04, $13, $08, $0C, $01, $14, $05, $0D, $14, $0C, $05, $11, $0D, $14, $07, $08, $16, $0F, $00, $07, $0D, $13, $04, $14, $0B, $10, $01, $01, $0C, $01, $01, $04, $13, $01, $00, $0C, $01, $08, $01, $06, $08, $09, $0D, $18, $19, $0F, $01, $00, $01, $0B, $10
          ;Addresses for the character jump table in two tables
          ;hi-byte and lo-byte, less one, for use by rts
1CH-LO    0986 32      DB $32, $37, $3C, $41, $46, $4B, $50, $55, $5A, $5F, $64, $69, $6E, $73, $78, $7D, $82, $87, $8C, $91, $96, $9B, $A0, $A5, $AA, $AF
1CH-HI    09A0 06      DB $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06
          09BA 81      DCS 'This is a bunch of strings'
          09C3 F0      DCS 'which appear in the program'
          09CD C3      DCS 'They will be analysed in the'
          09D7 DC      DCS 'first pass of the compiler'
          09E0 15      DCS 'to build a string table'
          09E9 66      DCS 'in the second pass, the '
          09F2 5C      DCS 'string table will be built'
          09FB 80      DCS 'and the strings actually encoded'
          0A08 A8      DCS 'a third pass is now required'
          0A12 2C      DCS 'as the string table is variable'
          0A1D D3      DCS 'length and so are the strings'
STR       0A28 81      DCS 'This is the test string'
END       0A32 00      BRK

~~~~





