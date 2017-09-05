Game listing from a build taken 5/8/17

~~~~
               START 0601 A900    LDA #$00
                     0603 8500    STA $00    ;DST
                     0605 A970    LDA #$70
                     0607 8501    STA $01    ;DST + 1
                     0609 A9E8    LDA #$E8
                     060B 850A    STA $0A
                     060D A203    LDX #$03
                     060F A910    LDA #$10
                     0611 205218  JSR $1852  ;MEMSET 
                     0614 A995    LDA #$95   ;LO : FONT:PRESENT
                     0616 851D    STA $1D    ;FONT
                     0618 A92C    LDA #$2C   ;HI : FONT:PRESENT
                     061A 851E    STA $1E    ;FONT + 1
                     061C 200D1C  JSR $1C0D  ;NAVIGATE 
                     061F 34..    DW $0634
                TEST
               TEST2 0621 A9EE    LDA #$EE   ;LO : DISPATCHER:DUNGEON-CELL
                     0623 851F    STA $1F    ;LOCATION-DISPATCH-TABLE
                     0625 A922    LDA #$22   ;HI : DISPATCHER:DUNGEON-CELL
                     0627 8520    STA $20    ;LOCATION-DISPATCH-TABLE + 1
                     0629 00      BRK
          TEST-INPUT 062A 20B61C  JSR $1CB6  ;TEST-RENDER-INPUT 
                     062D 209C1D  JSR $1D9C  ;PARSE 
                     0630 20A122  JSR $22A1  ;DISPATCH 
                     0633 00      BRK
        DUNGEON-CELL
  DUNGEON-CELL:TITLE 0634 DD..    DW $2BDD
GEON-CELL:TFLEUR-COL 0636 0D..    DB $0D
   DUNGEON-CELL:TEXT 0637 23..    DW $2B23
GEON-CELL:MFLEUR-COL 0639 06..    DB $06
   DUNGEON-CELL:IMGW 063A 0D..    DB $0D
                     063B 43..    DW $0643
   DUNGEON-CELL:IMGH 063D 68..    DB $68
                     063E 71..    DW $0A71
UNGEON-CELL:DISPATCH 0640 EE..    DW $22EE
  DUNGEON-CELL:PLACE 0642 02..    DB $02
                     ;Image DUNGEON-CELL 104x104 (/home/dan/Downloads/cellardoor.bmp)
 DUNGEON-CELL:PIXELS 0643 15..    /home/dan/Downloads/cellardoor.bmp pixels (1070)
DUNGEON-CELL:COLOURS 0A71 01..    /home/dan/Downloads/cellardoor.bmp colours (122)
                     ;ON EXAMINE SLIME 
N-CELL:EXAMINE-SLIME 0AEB 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0AEE F3..    DW $2AF3   ;Millions of sad eyes peer out from the
slime.
                     0AF0 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0AF2 1005    BPL $0AF9  ;$0AF0:ENDIF 
                     0AF4 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0AF7 E2..    DW $2AE2   ;They seem to be staring at the floor.
         $0AF0:ENDIF 0AF9 60      RTS
                     ;ON EXAMINE WALL 
ON-CELL:EXAMINE-WALL 0AFA 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0AFD CD..    DW $2ACD   ;The wall oozes with a repellant green slime.
                     0AFF 2418    BIT $18
                     0B01 3008    BMI $0B0B  ;$0AFF:ENDIF 
                     0B03 38      SEC
                     0B04 6618    ROR $18
                     0B06 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B09 BA..    DW $2ABA   ;Eugh! The slime is looking at you!
         $0AFF:ENDIF 0B0B 60      RTS
                     ;ON EXAMINE FLOOR 
N-CELL:EXAMINE-FLOOR 0B0C 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B0F AD..    DW $2AAD   ;There is a crack in the floor.
                     0B11 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0B13 1005    BPL $0B1A  ;$0B11:ENDIF 
                     0B15 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B18 97..    DW $2A97   ;Perhaps it bears further examination?
         $0B11:ENDIF 0B1A 60      RTS
                     ;ON EXAMINE CRACK 
N-CELL:EXAMINE-CRACK 0B1B 2417    BIT $17    ;IF SLIME-LICKED
                     0B1D 1017    BPL $0B36  ;$0B1B:ELSE 
                     0B1F 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0B21 100B    BPL $0B2E  ;$0B1F:ELSE 
                     0B23 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B26 80..    DW $2A80   ;A glint of metal shines back at you... A key!
                     0B28 38      SEC
                     0B29 6616    ROR $16
                     0B2B 4C330B  JMP $0B33  ;$0B1F:ENDIF 
          $0B1F:ELSE 0B2E 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0B31 3F..    DW $2A3F   ;A crack in the floor, just like any other.
One might hide a small key-like object here.
Like, for example, a key.
         $0B1F:ENDIF 0B33 4C3B0B  JMP $0B3B  ;$0B1B:ENDIF 
          $0B1B:ELSE 0B36 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0B39 1E..    DW $2A1E   ;The Veil of Maia prevents you from seeing
anything interesting.
         $0B1B:ENDIF 0B3B 60      RTS
                     ;ON TAKE KEY 
UNGEON-CELL:TAKE-KEY 0B3C 2416    BIT $16    ;IF CRACK-EXAMINED
                     0B3E 1016    BPL $0B56  ;$0B3C:ELSE 
                     0B40 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0B42 100A    BPL $0B4E  ;$0B40:ELSE 
                     0B44 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B47 14..    DW $2A14   ;You take the shiny key.
                     0B49 461A    LSR $1A
                     0B4B 4C530B  JMP $0B53  ;$0B40:ENDIF 
          $0B40:ELSE 0B4E 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0B51 F3..    DW $29F3   ;It's in your pocket... Perhaps the dungeon
air is getting to you?
         $0B40:ENDIF 0B53 4C5B0B  JMP $0B5B  ;$0B3C:ENDIF 
          $0B3C:ELSE 0B56 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B59 DC..    DW $29DC   ;What key? Do you know something I don't?
         $0B3C:ENDIF 0B5B 60      RTS
                     ;ON EXAMINE DOOR 
ON-CELL:EXAMINE-DOOR 0B5C 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0B5F B4..    DW $29B4   ;The door is a grim iron affair with a tiny
barred window and a keyhole.
                     0B61 2415    BIT $15    ;IF DOOR-OPEN
                     0B63 1008    BPL $0B6D  ;$0B61:ELSE 
         $0B43:ENDIF 0B65 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B68 AD..    DW $29AD   ;The door is open.
                     0B6A 4C720B  JMP $0B72  ;$0B61:ENDIF 
          $0B61:ELSE 0B6D 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B70 A5..    DW $29A5   ;The door is closed.
         $0B61:ENDIF 0B72 60      RTS
                     ;ON EXAMINE WINDOW 
-CELL:EXAMINE-WINDOW 0B73 2415    BIT $15
                     0B75 3019    BMI $0B90  ;$0B73:ENDIF $0B7C:ENDIF 
                     0B77 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0B7A 95..    DW $2995   ;A goblin appears at the window.
                     0B7C 2414    BIT $14    ;IF SLOP-FLUNG
                     0B7E 1008    BPL $0B88  ;$0B7C:ELSE 
                     0B80 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0B83 56..    DW $2956   ;He tells you to keep the noise down using a
stream of vowel-free goblin profanities.
KRRPKCHK DRGKPK!
                     0B85 4C900B  JMP $0B90  ;$0B73:ENDIF $0B7C:ENDIF 
          $0B7C:ELSE 0B88 38      SEC
                     0B89 6614    ROR $14
                     0B8B 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0B8E 2C..    DW $292C   ;He flings some inedible slop through the
bars. You hear a key rattling in the lock.
         $0B7C:ENDIF
         $0B73:ENDIF 0B90 60      RTS
                     ;ON EXAMINE SLOP 
ON-CELL:EXAMINE-SLOP 0B91 2414    BIT $14    ;IF SLOP-FLUNG
                     0B93 1005    BPL $0B9A  ;$0B91:ENDIF 
          $0B88:ELSE 0B95 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0B98 F6..    DW $28F6   ;A balanced soup of entrails, small
amphibians and mandibles. Ooh! Garlic
croutons!
         $0B91:ENDIF 0B9A 60      RTS
                     ;ON EAT FOOD 
UNGEON-CELL:EAT-FOOD 0B9B 2414    BIT $14    ;IF SLOP-FLUNG
                     0B9D 1005    BPL $0BA4  ;$0B9B:ENDIF 
                     0B9F 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BA2 DE..    DW $28DE   ;I would hardly call the goblin's slop food.
         $0B9B:ENDIF 0BA4 60      RTS
                     ;ON EAT SLOP 
UNGEON-CELL:EAT-SLOP 0BA5 2414    BIT $14    ;IF SLOP-FLUNG
                     0BA7 1005    BPL $0BAE  ;$0BA5:ENDIF 
         $0B9F:ENDIF 0BA9 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BAC D0..    DW $28D0   ;The gods look away in shame.
         $0BA5:ENDIF 0BAE 60      RTS
                     ;ON EXAMINE KEY 
EON-CELL:EXAMINE-KEY 0BAF 2416    BIT $16    ;IF CRACK-EXAMINED
                     0BB1 1008    BPL $0BBB  ;$0BAF:ELSE 
                     0BB3 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BB6 C8..    DW $28C8   ;It's a key, man.
                     0BB8 4CC00B  JMP $0BC0  ;$0BAF:ENDIF 
          $0BAF:ELSE 0BBB 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BBE C2..    DW $28C2   ;What key?
         $0BAF:ENDIF 0BC0 60      RTS
                     ;ON EXAMINE KEYHOLE 
CELL:EXAMINE-KEYHOLE 0BC1 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BC4 B7..    DW $28B7   ;It's a keyhole, man.
                     0BC6 60      RTS
                     ;ON TAKE CRACK 
GEON-CELL:TAKE-CRACK 0BC7 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BCA AE..    DW $28AE   ;Inadvisable.
                     0BCC 60      RTS
                     ;ON LICK CRACK 
          $0BC0:ELSE
GEON-CELL:LICK-CRACK 0BCD 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BD0 D0..    DW $28D0   ;The gods look away in shame.
         $0BC0:ENDIF 0BD2 60      RTS
                     ;ON ATTACK SLIME 
ON-CELL:ATTACK-SLIME 0BD3 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0BD6 8F..    DW $288F   ;Your hand is stayed by the slime's gaze of
infinite sadness.
                     0BD8 60      RTS
                     ;ON UNLOCK DOOR FINGER 
L:UNLOCK-DOOR-FINGER 0BD9 2419    BIT $19    ;IF DOOR-LOCKED
                     0BDB 100D    BPL $0BEA  ;$0BD9:ELSE 
                     0BDD 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0BE0 6C..    DW $286C   ;Wise guy, eh? The lock doesn't budge. Your
finger is now sore.
                     0BE2 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BE5 5B..    DW $285B   ;You hear the faint sound of snickering...
                     0BE7 4CEF0B  JMP $0BEF  ;$0BD9:ENDIF 
          $0BD9:ELSE 0BEA 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0BED 42..    DW $2842   ;You put your finger in the keyhole of an
unlocked door.
         $0BD9:ENDIF 0BEF 60      RTS
                     ;ON UNLOCK DOOR 
                     ;ON USE KEY DOOR 
EON-CELL:UNLOCK-DOOR
ON-CELL:USE-KEY-DOOR 0BF0 2419    BIT $19    ;IF DOOR-LOCKED
                     0BF2 1022    BPL $0C16  ;$0BF0:ELSE 
                     0BF4 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0BF6 1008    BPL $0C00  ;$0BF4:ELSE 
                     0BF8 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0BFB 35..    DW $2835   ;With what? Your finger?
          $0BEB:ELSE 0BFD 4C130C  JMP $0C13  ;$0BF4:ENDIF $0C00:ENDIF 
          $0BF4:ELSE 0C00 2414    BIT $14    ;IF SLOP-FLUNG
         $0BEB:ENDIF 0C02 100A    BPL $0C0E  ;$0C00:ELSE 
                     0C04 4619    LSR $19
                     0C06 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C09 27..    DW $2827   ;The lock mechanism clicks...
                     0C0B 4C130C  JMP $0C13  ;$0BF4:ENDIF $0C00:ENDIF 
          $0C00:ELSE 0C0E 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0C11 03..    DW $2803   ;You rattle the key in the lock, but there is
a key stuck in the other side.
         $0C00:ENDIF
         $0BF4:ENDIF 0C13 4C1B0C  JMP $0C1B  ;$0BF0:ENDIF 
          $0BF0:ELSE 0C16 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C19 F7..    DW $27F7   ;The door is already unlocked.
         $0BF0:ENDIF 0C1B 60      RTS
                     ;ON CLOSE DOOR 
GEON-CELL:CLOSE-DOOR 0C1C 2415    BIT $15    ;IF DOOR-OPEN
                     0C1E 100C    BPL $0C2C  ;$0C1C:ENDIF 
                     0C20 4615    LSR $15
                     0C22 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
          $0C15:ELSE 0C25 EF..    DW $27EF   ;The door closes.
                     0C27 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
         $0C15:ENDIF
         $0C08:ENDIF 0C2A D0..    DW $28D0   ;The gods look away in shame.
         $0C1C:ENDIF 0C2C 60      RTS
                     ;ON LOCK DOOR 
          $0C03:ELSE
NGEON-CELL:LOCK-DOOR 0C2D 2419    BIT $19    ;IF DOOR-LOCKED
                     0C2F 1008    BPL $0C39  ;$0C2D:ELSE 
                     0C31 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C34 E4..    DW $27E4   ;The door is already locked.
                     0C36 4C430C  JMP $0C43  ;$0C2D:ENDIF 
          $0C2D:ELSE 0C39 38      SEC
                     0C3A 6619    ROR $19
                     0C3C 4615    LSR $15
                     0C3E 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     0C41 B9..    DW $27B9   ;The lock mechanism clicks shut. You really
have got it in for yourself haven't you?
         $0C2D:ENDIF 0C43 60      RTS
                     ;ON EXIT 
                     ;ON USE DOOR 
   DUNGEON-CELL:EXIT
UNGEON-CELL:USE-DOOR 0C44 2415    BIT $15    ;IF DOOR-OPEN
                     0C46 1008    BPL $0C50  ;$0C44:ELSE 
                     0C48 200D1C  JSR $1C0D  ;NAVIGATE 
                     0C4B A7..    DW $0CA7
                     0C4D 4C5A0C  JMP $0C5A  ;$0C44:ENDIF 
          $0C44:ELSE 0C50 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
          $0C46:ELSE 0C53 A6..    DW $27A6   ;Ouch! You walk into the closed door.
                     0C55 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C58 5B..    DW $285B   ;You hear the faint sound of snickering...
         $0C44:ENDIF 0C5A 60      RTS
                     ;ON LICK SLIME 
GEON-CELL:LICK-SLIME 0C5B 2417    BIT $17    ;IF SLIME-LICKED
                     0C5D 1008    BPL $0C67  ;$0C5B:ELSE 
         $0C46:ENDIF 0C5F 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0C62 68..    DW $2768   ;Nothing happens. Your third eye is already
open. But... you do feel a strange urge to
grow a beard and play the guitar.
                     0C64 4C740C  JMP $0C74  ;$0C5B:ENDIF 
          $0C5B:ELSE 0C67 38      SEC
                     0C68 6617    ROR $17
                     0C6A 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
          $0C60:ELSE 0C6D 62..    DW $2762   ;Far out!
                     0C6F 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0C72 20..    DW $2720   ;Myriad colours break in waves upon your
ears. Maia's cosmic tears rain down on you
in a shower of gold. The slime smiles.
         $0C5B:ENDIF 0C74 60      RTS
                     ;ON OPEN DOOR 
NGEON-CELL:OPEN-DOOR 0C75 2415    BIT $15    ;IF DOOR-OPEN
         $0C60:ENDIF 0C77 1008    BPL $0C81  ;$0C75:ELSE 
                     0C79 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C7C 15..    DW $2715   ;The door is already open.
                     0C7E 4CA60C  JMP $0CA6  ;$0C75:ENDIF $0C81:ENDIF 
          $0C75:ELSE 0C81 2419    BIT $19    ;IF DOOR-LOCKED
                     0C83 1014    BPL $0C99  ;$0C81:ELSE 
          $0C78:ELSE 0C85 241A    BIT $1A    ;IF KEY-IN-CRACK
                     0C87 1008    BPL $0C91  ;$0C85:ELSE 
                     0C89 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     0C8C DD..    DW $26DD   ;Have you been licking the slime? It's
hallucinogenic. The door, not atypically for
a dungeon, is locked.
                     0C8E 4C960C  JMP $0C96  ;$0C85:ENDIF 
          $0C85:ELSE 0C91 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C94 D6..    DW $26D6   ;The door is locked.
         $0C85:ENDIF 0C96 4CA60C  JMP $0CA6  ;$0C75:ENDIF $0C81:ENDIF 
          $0C81:ELSE 0C99 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     0C9C 62..    DW $2762   ;Far out!
                     0C9E 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
          $0C94:ELSE 0CA1 CD..    DW $26CD   ;The door creaks open.
                     0CA3 38      SEC
                     0CA4 6615    ROR $15
         $0C81:ENDIF
         $0C75:ENDIF 0CA6 60      RTS
            CORRIDOR
      CORRIDOR:TITLE 0CA7 C4..    DW $26C4
 CORRIDOR:TFLEUR-COL 0CA9 09..    DB $09
       CORRIDOR:TEXT 0CAA 38..    DW $2638
 CORRIDOR:MFLEUR-COL 0CAC 00..    DB $00
       CORRIDOR:IMGW 0CAD 0D..    DB $0D
                     0CAE B6..    DW $0CB6
       CORRIDOR:IMGH 0CB0 68..    DB $68
                     0CB1 7D..    DW $107D
          $0CA6:ELSE
   CORRIDOR:DISPATCH 0CB3 67..    DW $2367
      CORRIDOR:PLACE 0CB5 03..    DB $03
                     ;Image CORRIDOR 104x104 (/home/dan/Downloads/bedroom.bmp)
     CORRIDOR:PIXELS 0CB6 0A..    /home/dan/Downloads/bedroom.bmp pixels (967)
    CORRIDOR:COLOURS 107D 00..    /home/dan/Downloads/bedroom.bmp colours (157)
                     ;ON EXAMINE TORCHES 
                     ;ON EXAMINE TORCH 
IDOR:EXAMINE-TORCHES
RRIDOR:EXAMINE-TORCH 111A 38      SEC
                     111B 6611    ROR $11
                     111D 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     1120 00..    DW $2600   ;The flickering shadows make you think of
something profound, like a packet of
Cheezows caught in the wind.
                     1122 60      RTS
                     ;ON TAKE TORCH 
 CORRIDOR:TAKE-TORCH 1123 38      SEC
                     1124 6612    ROR $12
                     1126 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1129 F3..    DW $25F3   ;You take one of the torches.
                     112B 60      RTS
                     ;ON CHEEZOWS 
                     ;ON PACKET 
   CORRIDOR:CHEEZOWS
     CORRIDOR:PACKET 112C 2411    BIT $11    ;IF TORCHES-EXAMINED
                     112E 1008    BPL $1138  ;$112C:ELSE 
                     1130 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     1133 CF..    DW $25CF   ;There are no Cheezows! It was a metaphor
for your situation.
                     1135 4C3D11  JMP $113D  ;$112C:ENDIF 
          $112C:ELSE 1138 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     113B C2..    DW $25C2   ;What a strange thing to say.
         $112C:ENDIF 113D 60      RTS
                     ;ON EXAMINE METAPHOR 
DOR:EXAMINE-METAPHOR 113E 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1141 B1..    DW $25B1   ;That is really taking the biscuit.
                     1143 60      RTS
                     ;ON ENTER CELL 
 CORRIDOR:ENTER-CELL 1144 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1147 A7..    DW $25A7   ;You enter your cell.
                     1149 60      RTS
                     ;ON ENTER GREEN DOOR 
DOR:ENTER-GREEN-DOOR 114A 2413    BIT $13    ;IF GREEN-DOOR-OPEN
                     114C 1008    BPL $1156  ;$114A:ELSE 
                     114E 200D1C  JSR $1C0D  ;NAVIGATE 
                     1151 A5..    DW $11A5
                     1153 4C5B11  JMP $115B  ;$114A:ENDIF 
          $114A:ELSE 1156 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1159 9C..    DW $259C   ;The green door is closed.
         $114A:ENDIF 115B 60      RTS
                     ;ON ENTER CELL DOOR 
IDOR:ENTER-CELL-DOOR 115C 200D1C  JSR $1C0D  ;NAVIGATE 
                     115F 34..    DW $0634
                     1161 60      RTS
                     ;ON EXAMINE WALL 
ORRIDOR:EXAMINE-WALL 1162 2412    BIT $12    ;IF TORCH-CARRIED
                     1164 100D    BPL $1173  ;$1162:ELSE 
                     1166 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1169 62..    DW $2762   ;Far out!
                     116B 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     116E 5D..    DW $255D   ;Etched on the wall is a diagram. A triangle
sits inside a circle, surrounded by flames. A
spell perhaps?
                     1170 4C7811  JMP $1178  ;$1162:ENDIF 
          $1162:ELSE 1173 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     1176 40..    DW $2540   ;It is too dark at this end of the corridor to
see anything.
         $1162:ENDIF 1178 60      RTS
                     ;ON ENTER DOOR 
 CORRIDOR:ENTER-DOOR 1179 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     117C 2F..    DW $252F   ;You will need to be more specific.
                     117E 60      RTS
                     ;ON EXAMINE GREEN DOOR 
R:EXAMINE-GREEN-DOOR 117F 209917  JSR $1799  ;PRINT-MESSAGE:2 
         $1170:ENDIF 1182 15..    DW $2515   ;You hear mumbling and sighing from behind
the door.
                     1184 60      RTS
                     ;ON KNOCK GREEN DOOR 
DOR:KNOCK-GREEN-DOOR 1185 2413    BIT $13    ;IF GREEN-DOOR-OPEN
                     1187 1008    BPL $1191  ;$1185:ELSE 
                     1189 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     118C EE..    DW $24EE   ;Politely, you knock on the already open
green door, but theres is no answer.
                     118E 4C9E11  JMP $119E  ;$1185:ENDIF 
          $1185:ELSE 1191 38      SEC
                     1192 6613    ROR $13
                     1194 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1197 DC..    DW $24DC   ;You knock on the door and wait patiently.
                     1199 206417  JSR $1764  ;PRINT-MESSAGE:3 
                     119C AF..    DW $24AF   ;Presently, it swings open. There appears to
be some sort of lodging beyond the
threshold.
         $1185:ENDIF 119E 60      RTS
                     ;ON KNOCK DOOR 
 CORRIDOR:KNOCK-DOOR 119F 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     11A2 A8..    DW $24A8   ;Which one?
                     11A4 60      RTS
    FRAZBOLGS-CLOSET
AZBOLGS-CLOSET:TITLE 11A5 96..    DW $2496
GS-CLOSET:TFLEUR-COL 11A7 12..    DB $12
RAZBOLGS-CLOSET:TEXT 11A8 FA..    DW $23FA
GS-CLOSET:MFLEUR-COL 11AA 00..    DB $00
RAZBOLGS-CLOSET:IMGW 11AB 0D..    DB $0D
                     11AC B4..    DW $11B4
RAZBOLGS-CLOSET:IMGH 11AE 68..    DB $68
                     11AF 29..    DW $1629
OLGS-CLOSET:DISPATCH 11B1 AE..    DW $23AE
AZBOLGS-CLOSET:PLACE 11B3 04..    DB $04
                     ;Image FRAZBOLGS-CLOSET 104x104 (/home/dan/Downloads/porsche.bmp)
ZBOLGS-CLOSET:PIXELS 11B4 0B..    /home/dan/Downloads/porsche.bmp pixels (1141)
BOLGS-CLOSET:COLOURS 1629 01..    /home/dan/Downloads/porsche.bmp colours (151)
                     ;ON EXAMINE CHAIR 
CLOSET:EXAMINE-CHAIR 16C0 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     16C3 F3..    DW $23F3   ;It's broken.
                     16C5 60      RTS
                     ;ON EXIT 
RAZBOLGS-CLOSET:EXIT 16C6 200D1C  JSR $1C0D  ;NAVIGATE 
                     16C9 A7..    DW $0CA7
                     16CB 60      RTS
                     ;ON EXAMINE 
                     ;Load the second and third words into the name
                     ;and adjective positions
     GENERIC:EXAMINE 16CC AD371E  LDA $1E37
                     16CF F00A    BEQ $16DB  ;GENERIC:NO-ADJECTIVE 
                     16D1 850A    STA $0A
                     16D3 AD361E  LDA $1E36
                     16D6 850B    STA $0B
                     16D8 4CE216  JMP $16E2  ;GENERIC:LOADED-WORDS 
GENERIC:NO-ADJECTIVE 16DB 850B    STA $0B
                     16DD AD361E  LDA $1E36
                     16E0 850A    STA $0A
GENERIC:LOADED-WORDS 16E2 20561D  JSR $1D56  ;FIND-OBJECT-INDEX 
                     16E5 B014    BCS $16FB  ;GENERIC:DUPLICATE-FOUND 
                     16E7 F018    BEQ $1701  ;GENERIC:NOT-FOUND 
                     ;Now print the description
                     16E9 B9991D  LDA $1D99,Y ;FIND-OBJECT-INDEX:INITIAL-PLACES 
                     16EC 8DF916  STA $16F9  ;GENERIC:DESCRIPTION-HI 
                     16EF B99A1D  LDA $1D9A,Y ;FIND-OBJECT-INDEX:DESCRIPTION-HI 
                     16F2 8DF816  STA $16F8  ;GENERIC:DESCRIPTION-LO 
                     16F5 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
NERIC:DESCRIPTION-LO 16F8 00..    DB $00
NERIC:DESCRIPTION-HI 16F9 00..    DB $00
                     16FA 60      RTS
ERIC:DUPLICATE-FOUND 16FB 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     16FE A8..    DW $24A8   ;Which one?
                     1700 60      RTS
   GENERIC:NOT-FOUND 1701 20CE17  JSR $17CE  ;PRINT-MESSAGE:1 
                     1704 E5..    DW $23E5   ;I don't know what that is.
                     1706 60      RTS
                     ;ON TAKE KEY 
    GENERIC:TAKE-KEY 1707 241A    BIT $1A
                     1709 3005    BMI $1710  ;$1707:ENDIF 
                     170B 209917  JSR $1799  ;PRINT-MESSAGE:2 
                     170E C4..    DW $23C4   ;You already have the key! Perhaps the
dungeon air is getting to you.
         $1707:ENDIF 1710 60      RTS
                     ;Bit table
                     ;Bits for NIL
             DEREF-W 1711 BA      TSX
                     1712 E8      INX        ;Points to return address of this function
                     1713 E8      INX        ;Now skip to return address of grandparent
                     1714 E8      INX
                     ;Store the grandparent return address
                     1715 BD0001  LDA $0100,X
                     1718 8526    STA $26    ;TMP
                     171A BD0101  LDA $0101,X
                     171D 8527    STA $27    ;TMP + 1
                     ;Now we have the address of the parameter (-1)
                     ;Add two to it so we can skip it when parent returns
                     171F 18      CLC
                     1720 A902    LDA #$02
                     1722 7D0001  ADC $0100,X
                     1725 9D0001  STA $0100,X
                     1728 A900    LDA #$00
                     172A 7D0101  ADC $0101,X
                     172D 9D0101  STA $0101,X
                     1730 A001    LDY #$01   ;Offset against -1 for return convention
                     ;Dereference the word at the parameter address
                     1732 B126    LDA ($26),Y
                     1734 AA      TAX
                     1735 C8      INY
                     1736 B126    LDA ($26),Y
                     1738 60      RTS
         SCROLL-TEXT 1739 A988    LDA #$88
         $1731:ENDIF 173B 8500    STA $00    ;SRC
                     173D A998    LDA #$98
                     173F 8501    STA $01    ;SRC + 1
                     1741 A9D0    LDA #$D0
                     1743 8502    STA $02    ;DST
                     1745 A996    LDA #$96
                     1747 8503    STA $03    ;DST + 1
                     1749 A928    LDA #$28
                     174B A205    LDX #$05
                     174D 203318  JSR $1833  ;MEMCPY 
                     1750 A9F8    LDA #$F8
                     1752 8500    STA $00    ;DST
                     1754 A99B    LDA #$9B
                     1756 8501    STA $01    ;DST + 1
                     1758 A9B8    LDA #$B8
                     175A 850A    STA $0A
                     175C A201    LDX #$01
                     175E A900    LDA #$00
                     1760 205218  JSR $1852  ;MEMSET 
                     1763 60      RTS
     PRINT-MESSAGE:3 1764 A9F8    LDA #$F8
                     1766 8500    STA $00    ;SRC
                     1768 A99B    LDA #$9B
                     176A 8501    STA $01    ;SRC + 1
                     176C A9D0    LDA #$D0
                     176E 8502    STA $02    ;DST
                     1770 A996    LDA #$96
                     1772 8503    STA $03    ;DST + 1
                     1774 A9B8    LDA #$B8
                     1776 A201    LDX #$01
                     1778 203318  JSR $1833  ;MEMCPY 
                     177B A988    LDA #$88
                     177D 8500    STA $00    ;DST
                     177F A998    LDA #$98
                     1781 8501    STA $01    ;DST + 1
                     1783 A928    LDA #$28
                     1785 850A    STA $0A
                     1787 A205    LDX #$05
                     1789 A900    LDA #$00
                     178B 205218  JSR $1852  ;MEMSET 
                     178E A988    LDA #$88
                     1790 8502    STA $02    ;TYPESET:RASTER
                     1792 A998    LDA #$98
                     1794 8503    STA $03    ;TYPESET:RASTER + 1
                     1796 4C0018  JMP $1800  ;PRINT-MESSAGE:PRINT 
     PRINT-MESSAGE:2 1799 A940    LDA #$40
                     179B 8500    STA $00    ;SRC
                     179D A99A    LDA #$9A
                     179F 8501    STA $01    ;SRC + 1
                     17A1 A9D0    LDA #$D0
                     17A3 8502    STA $02    ;DST
                     17A5 A996    LDA #$96
                     17A7 8503    STA $03    ;DST + 1
                     17A9 A970    LDA #$70
                     17AB A203    LDX #$03
                     17AD 203318  JSR $1833  ;MEMCPY 
                     17B0 A940    LDA #$40
                     17B2 8500    STA $00    ;DST
                     17B4 A99A    LDA #$9A
                     17B6 8501    STA $01    ;DST + 1
                     17B8 A970    LDA #$70
                     17BA 850A    STA $0A
                     17BC A203    LDX #$03
                     17BE A900    LDA #$00
                     17C0 205218  JSR $1852  ;MEMSET 
                     17C3 A940    LDA #$40
                     17C5 8502    STA $02    ;TYPESET:RASTER
                     17C7 A99A    LDA #$9A
                     17C9 8503    STA $03    ;TYPESET:RASTER + 1
                     17CB 4C0018  JMP $1800  ;PRINT-MESSAGE:PRINT 
     PRINT-MESSAGE:1 17CE A988    LDA #$88
                     17D0 8500    STA $00    ;SRC
                     17D2 A998    LDA #$98
                     17D4 8501    STA $01    ;SRC + 1
                     17D6 A9D0    LDA #$D0
                     17D8 8502    STA $02    ;DST
                     17DA A996    LDA #$96
                     17DC 8503    STA $03    ;DST + 1
                     17DE A928    LDA #$28
                     17E0 A205    LDX #$05
                     17E2 203318  JSR $1833  ;MEMCPY 
                     17E5 A9F8    LDA #$F8
                     17E7 8500    STA $00    ;DST
                     17E9 A99B    LDA #$9B
                     17EB 8501    STA $01    ;DST + 1
                     17ED A9B8    LDA #$B8
                     17EF 850A    STA $0A
                     17F1 A201    LDX #$01
                     17F3 A900    LDA #$00
                     17F5 205218  JSR $1852  ;MEMSET 
                     17F8 A9F8    LDA #$F8
                     17FA 8502    STA $02    ;TYPESET:RASTER
                     17FC A99B    LDA #$9B
                     17FE 8503    STA $03    ;TYPESET:RASTER + 1
 PRINT-MESSAGE:PRINT 1800 201117  JSR $1711  ;DEREF-W 
                     1803 8604    STX $04    ;STR
                     1805 8505    STA $05    ;STR + 1
                     1807 A98A    LDA #$8A   ;LO : PROMPT
                     1809 8500    STA $00    ;TYPESET:CHAR
                     180B A92C    LDA #$2C   ;HI : PROMPT
                     180D 8501    STA $01    ;TYPESET:CHAR + 1
                     180F A502    LDA $02    ;TYPESET:RASTER
                     1811 8506    STA $06    ;TYPESET-CS:TMP-RASTER
                     1813 A503    LDA $03    ;TYPESET:RASTER + 1
                     1815 8507    STA $07    ;TYPESET-CS:TMP-RASTER + 1
                     1817 A900    LDA #$00
                     1819 850B    STA $0B
                     181B 850A    STA $0A
                     181D 204A1B  JSR $1B4A  ;TYPESET 
                     1820 A506    LDA $06    ;TYPESET-CS:TMP-RASTER
                     1822 8502    STA $02    ;TYPESET:RASTER
                     1824 A507    LDA $07    ;TYPESET-CS:TMP-RASTER + 1
                     1826 8503    STA $03    ;TYPESET:RASTER + 1
                     1828 A900    LDA #$00
                     182A 850B    STA $0B
                     182C A905    LDA #$05
                     182E 850A    STA $0A
                     1830 4C6D18  JMP $186D  ;TYPESET-CS-CONTINUE 
              MEMCPY 1833 850A    STA $0A
                     1835 A000    LDY #$00
    MEMCPY:COPY-PAGE 1837 B100    LDA ($00),Y
                     1839 9102    STA ($02),Y
                     183B C8      INY
                     183C D0F9    BNE $1837  ;MEMCPY:COPY-PAGE 
                     183E E601    INC $01    ;SRC + 1
                     1840 E603    INC $03    ;DST + 1
                     1842 CA      DEX
                     1843 D0F2    BNE $1837  ;MEMCPY:COPY-PAGE 
                     1845 A60A    LDX $0A
                     1847 F008    BEQ $1851  ;MEMCPY:DONE 
EMCPY:COPY-REMAINDER 1849 B100    LDA ($00),Y
                     184B 9102    STA ($02),Y
                     184D C8      INY
                     184E CA      DEX
                     184F D0F8    BNE $1849  ;MEMCPY:COPY-REMAINDER 
         MEMCPY:DONE 1851 60      RTS
              MEMSET 1852 A000    LDY #$00
  MEMSET:MEMSET-PAGE 1854 9100    STA ($00),Y
                     1856 C8      INY
                     1857 D0FB    BNE $1854  ;MEMSET:MEMSET-PAGE 
                     1859 E601    INC $01    ;DST + 1
                     185B CA      DEX
                     185C D0F6    BNE $1854  ;MEMSET:MEMSET-PAGE 
                     185E A60A    LDX $0A
SET:MEMSET-REMAINDER 1860 9100    STA ($00),Y
                     1862 C8      INY
                     1863 CA      DEX
                     1864 D0FA    BNE $1860  ;MEMSET:MEMSET-REMAINDER 
         MEMSET:DONE 1866 60      RTS
          TYPESET-CS 1867 A900    LDA #$00
                     1869 850A    STA $0A
                     186B 850B    STA $0B
 TYPESET-CS-CONTINUE 186D A502    LDA $02    ;TYPESET:RASTER
                     186F 8506    STA $06    ;TMP-RASTER
                     1871 A503    LDA $03    ;TYPESET:RASTER + 1
                     1873 8507    STA $07    ;TMP-RASTER + 1
                     1875 4C8018  JMP $1880  ;TYPESET-CS:FIRST TYPESET-CS-CONT-TEST 
     TYPESET-CS:NEXT 1878 E604    INC $04    ;STR
                     187A D004    BNE $1880  ;TYPESET-CS:FIRST TYPESET-CS-CONT-TEST 
                     187C E605    INC $05    ;STR + 1
                     187E F06F    BEQ $18EF  ;TYPESET-CS:DONE  ;Gone off the edge of the map
TYPESET-CS-CONT-TEST
    TYPESET-CS:FIRST 1880 A000    LDY #$00
                     1882 B104    LDA ($04),Y
                     1884 F069    BEQ $18EF  ;TYPESET-CS:DONE 
                     1886 C9A2    CMP #$A2
                     1888 901E    BCC $18A8  ;TYPESET-CS:2CHAR 
                     188A 850C    STA $0C
                     188C AA      TAX
                     ;Get the first character from the three character table
                     ;note that the address to :3ch-0 is offet, so that we
                     ;can use the value in A without subtracting
                     188D BD4E18  LDA $184E,X
                     1890 20C218  JSR $18C2  ;TYPESET-CS:EMIT 
                     1893 A60C    LDX $0C
                     1895 BDAC18  LDA $18AC,X
                     1898 20C218  JSR $18C2  ;TYPESET-CS:EMIT 
                     189B A60C    LDX $0C
                     189D BD0A19  LDA $190A,X
                     ;The third character might be EOS
                     18A0 F04D    BEQ $18EF  ;TYPESET-CS:DONE 
                     18A2 20C218  JSR $18C2  ;TYPESET-CS:EMIT 
                     18A5 4C7818  JMP $1878  ;TYPESET-CS:NEXT 
    TYPESET-CS:2CHAR 18A8 C939    CMP #$39
                     18AA 9010    BCC $18BC  ;TYPESET-CS:1CHAR 
                     18AC 850C    STA $0C
                     18AE AA      TAX
                     18AF BDD119  LDA $19D1,X
                     18B2 20C218  JSR $18C2  ;TYPESET-CS:EMIT 
                     18B5 A60C    LDX $0C
                     18B7 BD3A1A  LDA $1A3A,X
                     ;The second character might be EOS
                     18BA F033    BEQ $18EF  ;TYPESET-CS:DONE 
    TYPESET-CS:1CHAR 18BC 20C218  JSR $18C2  ;TYPESET-CS:EMIT 
                     18BF 4C7818  JMP $1878  ;TYPESET-CS:NEXT 
     TYPESET-CS:EMIT 18C2 C901    CMP #$01
                     18C4 F013    BEQ $18D9  ;TYPESET-CS:NEWLINE 
                     ;Look up address of character data
                     ;table is offset by two as EOS=0 NEWLINE=1
                     18C6 AA      TAX
                     18C7 BDDA1A  LDA $1ADA,X
                     18CA 18      CLC
                     ;Add the offset of the font
                     18CB 651D    ADC $1D    ;FONT
                     18CD 8500    STA $00    ;TYPESET:CHAR
                     18CF BD111B  LDA $1B11,X
                     18D2 651E    ADC $1E    ;FONT + 1
                     18D4 8501    STA $01    ;TYPESET:CHAR + 1
                     18D6 4C4A1B  JMP $1B4A  ;TYPESET 
                     ;Carry is clear from the cmp 1
  TYPESET-CS:NEWLINE 18D9 A9B7    LDA #$B7
                     18DB 6506    ADC $06    ;TMP-RASTER
                     18DD 8506    STA $06    ;TMP-RASTER
                     18DF 8502    STA $02    ;TYPESET:RASTER
                     18E1 A901    LDA #$01
                     18E3 6507    ADC $07    ;TMP-RASTER + 1
                     18E5 8507    STA $07    ;TMP-RASTER + 1
                     18E7 8503    STA $03    ;TYPESET:RASTER + 1
                     18E9 A900    LDA #$00
    $18E5:INC16-DONE 18EB 850A    STA $0A
                     18ED 850B    STA $0B
     TYPESET-CS:DONE 18EF 60      RTS
                     ;Three character string table split into three
                     ;so that each character can be retrieved by indexing
                     ;without multiplication
    TYPESET-CS:3CH-0 18F0 27..    DB $27, $32, $02, $28, $2E, $1A, $02, $02, $37, $02, $28, $02, $02, $28, $1E, $2E, $02, $2D, $23, $2E, $31, $24, $2E, $02, $02, $2A, $02, $2E, $2E, $2D, $2D, $20, $02, $30, $24, $20, $20, $27, $27, $24, $24, $30, $24, $30, $24, $02, $2B, $22, $24, $22, $24, $2B, $2B, $2E, $02, $23, $33, $32, $2D, $02, $02, $24, $24, $32, $32, $02, $24, $2B, $02, $24, $33, $02, $02, $28, $24, $24, $02, $20, $02, $28, $02, $20, $2E, $02, $24, $24, $2B, $04, $31, $20, $30, $31, $27, $2D
    TYPESET-CS:3CH-1 194E 24..    DB $24, $27, $32, $2D, $33, $27, $37, $23, $2E, $20, $31, $2E, $28, $2D, $2E, $2E, $28, $26, $2E, $25, $02, $02, $30, $2A, $2B, $24, $32, $33, $22, $23, $02, $32, $20, $02, $02, $2D, $2B, $28, $24, $07, $20, $24, $2D, $24, $02, $26, $2E, $2A, $30, $2A, $30, $28, $2B, $2D, $31, $07, $30, $27, $26, $27, $1E, $02, $02, $02, $2E, $2E, $23, $28, $35, $02, $2D, $31, $31, $2C, $02, $23, $20, $30, $21, $22, $35, $30, $30, $20, $31, $02, $2E, $31, $02, $30, $24, $2B, $20, $07
    TYPESET-CS:3CH-2 19AC 02..    DB $02, $24, $27, $26, $02, $24, $2E, $2E, $33, $02, $02, $25, $31, $02, $33, $30, $2D, $02, $2E, $02, $20, $23, $02, $24, $2E, $37, $2E, $30, $2A, $02, $32, $02, $2D, $28, $31, $23, $2B, $2D, $30, $00, $30, $20, $32, $02, $26, $2E, $22, $02, $02, $24, $24, $2D, $02, $02, $27, $00, $02, $28, $24, $20, $2E, $20, $25, $32, $02, $2D, $02, $2C, $20, $32, $23, $2E, $2B, $24, $28, $07, $32, $24, $24, $2A, $28, $31, $07, $2B, $02, $2E, $2E, $02, $2E, $02, $31, $28, $32, $00
                     ;Two character string table
    TYPESET-CS:2CH-0 1A0A 24..    DB $24, $27, $28, $02, $32, $02, $31, $2E, $07, $30, $02, $02, $02, $32, $30, $2D, $2D, $2E, $24, $20, $2A, $28, $20, $24, $02, $2E, $23, $20, $2B, $27, $2B, $05, $2D, $33, $1A, $23, $22, $2E, $37, $2E, $02, $20, $24, $24, $02, $2C, $02, $2B, $1E, $37, $02, $20, $02, $02, $24, $07, $25, $24, $26, $02, $27, $33, $02, $2D, $2A, $02, $32, $24, $28, $28, $02, $30, $2F, $31, $2E, $33, $07, $2B, $26, $2B, $24, $30, $24, $2E, $31, $31, $2E, $31, $35, $02, $30, $33, $23, $20, $22, $2D, $32, $24, $30, $32, $20, $2C, $02, $2D, $2D
    TYPESET-CS:2CH-1 1A73 02..    DB $02, $24, $2D, $32, $27, $20, $02, $33, $00, $24, $31, $28, $2E, $02, $02, $02, $26, $30, $30, $2D, $24, $31, $30, $2D, $23, $2E, $2E, $02, $28, $20, $2E, $02, $23, $02, $27, $02, $2A, $25, $2E, $2D, $37, $2B, $20, $31, $22, $24, $35, $2B, $2E, $02, $26, $32, $21, $25, $23, $02, $02, $24, $02, $2A, $28, $30, $27, $32, $02, $2B, $2E, $37, $22, $32, $2C, $28, $24, $24, $35, $2D, $07, $02, $24, $24, $2B, $2E, $07, $02, $32, $2E, $22, $07, $20, $2F, $20, $32, $07, $23, $27, $24, $28, $32, $31, $20, $28, $20, $24, $07, $2E
                     ;Addresses for the character data table in two tables
                     ;hi-byte and lo-byte
   TYPESET-CS:1CH-LO 1ADC 00..    DB $00, $0B, $16, $21, $2C, $37, $B0, $BB, $C6, $D1, $DC, $E7, $F2, $FD, $08, $13, $29, $34, $3F, $4A, $55, $60, $76, $81, $8C, $97, $A2, $AD, $C3, $CE, $D9, $E4, $EF, $FA, $05, $10, $1B, $26, $31, $3C, $47, $52, $5D, $68, $73, $7E, $94, $9F, $AA, $B5, $C0, $CB, $D6, $E1, $EC
   TYPESET-CS:1CH-HI 1B13 00..    DB $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
             TYPESET 1B4A A200    LDX #$00   ;Ensure we can use X indexed addressing later
                     1B4C A50B    LDA $0B
                     1B4E F034    BEQ $1B84  ;TYPESET:START 
                     ;Prior to this shift, bit 6 set if previous char admits to the right
                     1B50 0A      ASL        ;Now that flag is in bit 7
                     ;Bit 7 set iff prev char admits to right and current char admits to left
                     1B51 2100    AND ($00,X)
                     1B53 4980    EOR #$80
                     1B55 0A      ASL
                     ;Now the carry is set iff there is no kerning between the two characters
                     1B56 A50B    LDA $0B
                     1B58 290F    AND #$0F
                     1B5A 650A    ADC $0A
                     1B5C C908    CMP #$08
                     1B5E 9015    BCC $1B75  ;TYPESET:NOT-WRAPPED 
                     ;There is a case where we will have a char of width 9
                     ;this means we need to advance an extra byte
                     ;Doing this compare clears the carry when we don't
                     1B60 C910    CMP #$10
                     1B62 2907    AND #$07
                     1B64 850A    STA $0A
                     1B66 A502    LDA $02    ;RASTER
                     1B68 E967    SBC #$67
                     1B6A 8502    STA $02    ;RASTER
                     1B6C A503    LDA $03    ;RASTER + 1
                     1B6E E901    SBC #$01
                     1B70 8503    STA $03    ;RASTER + 1
                     1B72 4C841B  JMP $1B84  ;TYPESET:START 
                     ;Move the raster back up, and left 1 byte
 TYPESET:NOT-WRAPPED 1B75 850A    STA $0A
                     1B77 38      SEC
                     1B78 A502    LDA $02    ;RASTER
                     1B7A E969    SBC #$69
                     1B7C 8502    STA $02    ;RASTER
                     1B7E A503    LDA $03    ;RASTER + 1
                     1B80 E901    SBC #$01
                     1B82 8503    STA $03    ;RASTER + 1
       TYPESET:START 1B84 A00A    LDY #$0A   ;10 pixel character height
                     1B86 D00B    BNE $1B93  ;TYPESET:GO 
                     ;Move the raster to the next line
        TYPESET:NEXT 1B88 A502    LDA $02    ;RASTER
                     1B8A 18      CLC
                     1B8B 6927    ADC #$27   ;40 for screen, -1
                     1B8D 8502    STA $02    ;RASTER
                     1B8F 9002    BCC $1B93  ;TYPESET:GO 
                     1B91 E603    INC $03    ;RASTER + 1
          TYPESET:GO 1B93 B100    LDA ($00),Y ;Get the bit pattern
                     ;Just jump to the next screen byte and blat it
                     ;if the bit pattern is empty
                     1B95 F022    BEQ $1BB9  ;TYPESET:SECOND-HALF 
                     1B97 A60A    LDX $0A
                     1B99 D00A    BNE $1BA5  ;TYPESET:SHIFT-RIGHT 
                     ;If the shift is zero then we can write the byte
                     ;and then clear the next one with no extra work
                     1B9B 8102    STA ($02,X) ;We hope X is 0 here
                     1B9D E602    INC $02    ;RASTER
                     1B9F D002    BNE $1BA3  ;$1B9D:INC16-DONE 
                     1BA1 E603    INC $03    ;RASTER + 1
    $1B9D:INC16-DONE 1BA3 D020    BNE $1BC5  ;TYPESET:SKIP-CLEAR 
                     ;Shift the bit pattern across by the offset
                     ;and OR it with the screen
 TYPESET:SHIFT-RIGHT 1BA5 4A      LSR
                     1BA6 CA      DEX
                     1BA7 D0FC    BNE $1BA5  ;TYPESET:SHIFT-RIGHT 
                     1BA9 0102    ORA ($02,X) ;We hope X is 0 here
                     1BAB 8102    STA ($02,X)
                     ;Now shift the same character byte left by 8-offset
                     1BAD A908    LDA #$08
                     1BAF 38      SEC
                     1BB0 E50A    SBC $0A
                     1BB2 AA      TAX
                     ;Get the bit pattern again
                     1BB3 B100    LDA ($00),Y
  TYPESET:SHIFT-LEFT 1BB5 0A      ASL
                     1BB6 CA      DEX
                     1BB7 D0FC    BNE $1BB5  ;TYPESET:SHIFT-LEFT 
 TYPESET:SECOND-HALF 1BB9 E602    INC $02    ;RASTER
                     1BBB D002    BNE $1BBF  ;$1BB9:INC16-DONE 
                     1BBD E603    INC $03    ;RASTER + 1
                     ;don't clear the next character
    $1BB9:INC16-DONE 1BBF C900    CMP #$00
                     1BC1 F002    BEQ $1BC5  ;TYPESET:SKIP-CLEAR 
                     1BC3 8102    STA ($02,X) ;X better be 0 here
  TYPESET:SKIP-CLEAR 1BC5 88      DEY
                     1BC6 D0C0    BNE $1B88  ;TYPESET:NEXT 
                     ;Now, the first byte of the character data holds the
                     ;width of the character. Store it, so we can use it
                     ;before we draw the next character
                     1BC8 B100    LDA ($00),Y
                     1BCA 850B    STA $0B
                     1BCC 60      RTS
      MIDDLE-FLEURON 1BCD 850A    STA $0A
                     1BCF A958    LDA #$58
                     1BD1 8500    STA $00    ;RASTER
                     1BD3 A996    LDA #$96
                     1BD5 8501    STA $01    ;RASTER + 1
                     1BD7 4CE41B  JMP $1BE4  ;FLEURON:DRAW-FLEURON 
         TOP-FLEURON 1BDA 850A    STA $0A
                     1BDC A990    LDA #$90
                     1BDE 8500    STA $00    ;RASTER
                     1BE0 A981    LDA #$81
                     1BE2 8501    STA $01    ;RASTER + 1
FLEURON:DRAW-FLEURON 1BE4 A209    LDX #$09
    FLEURON:NEXT-ROW 1BE6 38      SEC
                     1BE7 A500    LDA $00    ;RASTER
                     1BE9 E928    SBC #$28
                     1BEB 8500    STA $00    ;RASTER
                     1BED A501    LDA $01    ;RASTER + 1
                     1BEF E900    SBC #$00
                     1BF1 8501    STA $01    ;RASTER + 1
                     1BF3 BD031C  LDA $1C03,X ;FLEURON:ROPE 
                     1BF6 A027    LDY #$27
 FLEURON:NEXT-COLUMN 1BF8 9100    STA ($00),Y
                     1BFA 88      DEY
                     1BFB C40A    CPY $0A
                     1BFD 10F9    BPL $1BF8  ;FLEURON:NEXT-COLUMN 
                     1BFF CA      DEX
                     1C00 10E4    BPL $1BE6  ;FLEURON:NEXT-ROW 
                     1C02 60      RTS
        FLEURON:ROPE 1C03 00..    DB $00, $00, $C3, $24, $99, $42, $99, $24, $C3, $00
            NAVIGATE 1C0D 201117  JSR $1711  ;DEREF-W 
                     1C10 8608    STX $08    ;LOC
                     1C12 8509    STA $09    ;LOC + 1
                     ;Clear the top half of the screen
                     1C14 A900    LDA #$00
                     1C16 8500    STA $00    ;DST
                     1C18 A980    LDA #$80
                     1C1A 8501    STA $01    ;DST + 1
                     1C1C A9D0    LDA #$D0
                     1C1E 850A    STA $0A
                     1C20 A216    LDX #$16
                     1C22 A900    LDA #$00
                     1C24 205218  JSR $1852  ;MEMSET 
                     ;Set the current place
                     1C27 A00E    LDY #$0E
                     1C29 B108    LDA ($08),Y
                     1C2B 8523    STA $23
                     ;Get the title string
                     1C2D A000    LDY #$00
                     1C2F B108    LDA ($08),Y
                     1C31 8504    STA $04    ;STR
                     1C33 C8      INY
                     1C34 B108    LDA ($08),Y
                     1C36 8505    STA $05    ;STR + 1
                     1C38 A900    LDA #$00
                     1C3A 8502    STA $02    ;TYPESET:RASTER
                     1C3C A980    LDA #$80
                     1C3E 8503    STA $03    ;TYPESET:RASTER + 1
                     1C40 206718  JSR $1867  ;TYPESET-CS 
                     ;Get the left column for the title fleuron
                     1C43 A002    LDY #$02
                     1C45 B108    LDA ($08),Y
                     1C47 20DA1B  JSR $1BDA  ;TOP-FLEURON 
                     ;Get the text
                     1C4A A003    LDY #$03
                     1C4C B108    LDA ($08),Y
                     1C4E 8504    STA $04    ;STR
                     1C50 C8      INY
                     1C51 B108    LDA ($08),Y
                     1C53 8505    STA $05    ;STR + 1
                     1C55 A9E0    LDA #$E0
                     1C57 8502    STA $02    ;TYPESET:RASTER
                     1C59 A981    LDA #$81
                     1C5B 8503    STA $03    ;TYPESET:RASTER + 1
                     1C5D 206718  JSR $1867  ;TYPESET-CS 
                     ;Get the left column for the middle fleuron
                     1C60 A005    LDY #$05
                     1C62 B108    LDA ($08),Y
                     1C64 20CD1B  JSR $1BCD  ;MIDDLE-FLEURON 
                     ;Get the image width in bytes
                     1C67 A006    LDY #$06
                     1C69 B108    LDA ($08),Y
                     1C6B 850B    STA $0B
                     ;Work out where to put the image
                     1C6D A928    LDA #$28
                     1C6F 38      SEC
                     1C70 E50B    SBC $0B
                     1C72 8502    STA $02    ;DEST
                     1C74 A980    LDA #$80
                     1C76 E900    SBC #$00
                     1C78 8503    STA $03    ;DEST + 1
                     ;Now the address of the pixels
                     1C7A C8      INY
                     1C7B B108    LDA ($08),Y
                     1C7D 8500    STA $00    ;DATA
                     1C7F C8      INY
                     1C80 B108    LDA ($08),Y
                     1C82 8501    STA $01    ;DATA + 1
                     ;Now the height of the image
                     1C84 C8      INY
                     1C85 B108    LDA ($08),Y
                     1C87 20EA2B  JSR $2BEA  ;DECOMPRESS 
                     ;Work out where to put the colour data image
                     1C8A A928    LDA #$28
                     1C8C 38      SEC
                     1C8D E50B    SBC $0B
                     1C8F 8502    STA $02    ;DEST
                     1C91 A970    LDA #$70
                     1C93 E900    SBC #$00
                     1C95 8503    STA $03    ;DEST + 1
                     ;Get the colour data for the image
                     1C97 A00A    LDY #$0A
                     1C99 B108    LDA ($08),Y
                     1C9B 8500    STA $00    ;DECOMPRESS:DATA
                     1C9D C8      INY
                     1C9E B108    LDA ($08),Y
                     1CA0 8501    STA $01    ;DECOMPRESS:DATA + 1
                     ;Set the location dispatch table while we are here
                     1CA2 C8      INY
                     1CA3 B108    LDA ($08),Y
                     1CA5 851F    STA $1F    ;LOCATION-DISPATCH-TABLE
                     1CA7 C8      INY
                     1CA8 B108    LDA ($08),Y
                     1CAA 8520    STA $20    ;LOCATION-DISPATCH-TABLE + 1
                     ;Now the height again and divide by 8; colour attributes
                     ;are in blocks of 8x8
                     1CAC A009    LDY #$09
                     1CAE B108    LDA ($08),Y
                     1CB0 4A      LSR
                     1CB1 4A      LSR
                     1CB2 4A      LSR
                     ;Tail jump to decompress the colours and return
                     1CB3 4CEA2B  JMP $2BEA  ;DECOMPRESS 
   TEST-RENDER-INPUT 1CB6 A988    LDA #$88
                     1CB8 8500    STA $00    ;SRC
                     1CBA A998    LDA #$98
                     1CBC 8501    STA $01    ;SRC + 1
                     1CBE A9D0    LDA #$D0
                     1CC0 8502    STA $02    ;DST
                     1CC2 A996    LDA #$96
                     1CC4 8503    STA $03    ;DST + 1
                     1CC6 A9E0    LDA #$E0
                     1CC8 A206    LDX #$06
                     1CCA 203318  JSR $1833  ;MEMCPY 
                     1CCD A9F8    LDA #$F8
                     1CCF 8500    STA $00    ;DST
                     1CD1 A99B    LDA #$9B
                     1CD3 8501    STA $01    ;DST + 1
                     1CD5 A9B8    LDA #$B8
                     1CD7 850A    STA $0A
                     1CD9 A201    LDX #$01
                     1CDB A900    LDA #$00
                     1CDD 205218  JSR $1852  ;MEMSET 
                     1CE0 A9F8    LDA #$F8
                     1CE2 8502    STA $02    ;TYPESET:RASTER
                     1CE4 A99B    LDA #$9B
                     1CE6 8503    STA $03    ;TYPESET:RASTER + 1
          TYPESET-IS 1CE8 A000    LDY #$00
                     1CEA 840A    STY $0A
                     1CEC 840B    STY $0B
                     1CEE A939    LDA #$39   ;LO : PARSER:INPUT
                     1CF0 8504    STA $04    ;STR
                     1CF2 A91E    LDA #$1E   ;HI : PARSER:INPUT
                     1CF4 8505    STA $05    ;STR + 1
ST-RENDER-INPUT:NEXT 1CF6 840E    STY $0E
                     1CF8 B104    LDA ($04),Y
                     1CFA D00B    BNE $1D07  ;TEST-RENDER-INPUT:NOT-SPACE 
                     1CFC A995    LDA #$95   ;LO : PRESENT: 
                     1CFE 8500    STA $00    ;TYPESET:CHAR
                     1D00 A92C    LDA #$2C   ;HI : PRESENT: 
                     1D02 8501    STA $01    ;TYPESET:CHAR + 1
                     1D04 4C171D  JMP $1D17  ;TEST-RENDER-INPUT:EMIT 
NDER-INPUT:NOT-SPACE 1D07 AA      TAX
                     1D08 BD211D  LDA $1D21,X
                     1D0B 18      CLC
                     ;Add the offset of the font
                     1D0C 651D    ADC $1D    ;FONT
                     1D0E 8500    STA $00    ;TYPESET:CHAR
                     1D10 BD3B1D  LDA $1D3B,X
                     1D13 651E    ADC $1E    ;FONT + 1
                     1D15 8501    STA $01    ;TYPESET:CHAR + 1
ST-RENDER-INPUT:EMIT 1D17 204A1B  JSR $1B4A  ;TYPESET 
                     1D1A A40E    LDY $0E
                     1D1C C8      INY
                     1D1D C028    CPY #$28
                     1D1F D0D5    BNE $1CF6  ;TEST-RENDER-INPUT:NEXT 
                     1D21 60      RTS
-RENDER-INPUT:1CH-LO 1D22 BB..    DB $BB, $C6, $D1, $DC, $E7, $F2, $FD, $08, $13, $1E, $29, $34, $3F, $4A, $55, $60, $6B, $76, $81, $8C, $97, $A2, $AD, $B8, $C3, $CE
-RENDER-INPUT:1CH-HI 1D3C 00..    DB $00, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
                     ;Linear search for the name
   FIND-OBJECT-INDEX 1D56 A000    LDY #$00
                     1D58 840C    STY $0C
JECT-INDEX:NEXT-NAME 1D5A A50A    LDA $0A
ECT-INDEX:NEXT-NAME1 1D5C C8      INY
                     ;Check in one-based name table
                     1D5D D9951D  CMP $1D95,Y
                     1D60 F006    BEQ $1D68  ;FIND-OBJECT-INDEX:FOUND-NAME 
                     1D62 10F8    BPL $1D5C  ;FIND-OBJECT-INDEX:NEXT-NAME1 
JECT-INDEX:NOT-FOUND 1D64 18      CLC        ;Definitely not a duplicate if we are here
                     1D65 A40C    LDY $0C
                     1D67 60      RTS
                     ;Check adjective
ECT-INDEX:FOUND-NAME 1D68 A50B    LDA $0B
                     1D6A F005    BEQ $1D71  ;FIND-OBJECT-INDEX:ADJECTIVE-MATCHES  ;Zero adjective always matches...
                     1D6C D9961D  CMP $1D96,Y ;FIND-OBJECT-INDEX:NAMES 
                     1D6F D0E9    BNE $1D5A  ;FIND-OBJECT-INDEX:NEXT-NAME 
                     ;Now check it is in our place
EX:ADJECTIVE-MATCHES 1D71 B9971D  LDA $1D97,Y ;FIND-OBJECT-INDEX:ADJECTIVES 
                     1D74 F0E4    BEQ $1D5A  ;FIND-OBJECT-INDEX:NEXT-NAME  ;Object is elsewhere
                     1D76 C901    CMP #$01
                     1D78 F006    BEQ $1D80  ;FIND-OBJECT-INDEX:FOUND  ;Object is in our inventory
                     1D7A C523    CMP $23
                     1D7C F002    BEQ $1D80  ;FIND-OBJECT-INDEX:FOUND  ;Object is in our current location
                     1D7E D0DA    BNE $1D5A  ;FIND-OBJECT-INDEX:NEXT-NAME 
                     ;If we already have found one then return
D-OBJECT-INDEX:FOUND 1D80 A50C    LDA $0C
                     1D82 D004    BNE $1D88  ;FIND-OBJECT-INDEX:ALREADY-FOUND 
                     1D84 840C    STY $0C
                     1D86 F0D2    BEQ $1D5A  ;FIND-OBJECT-INDEX:NEXT-NAME 
                     ;Carry AND not-zero, i.e. duplicate AND found
-INDEX:ALREADY-FOUND 1D88 38      SEC
                     1D89 60      RTS
        INIT-OBJECTS 1D8A A001    LDY #$01
ECT-INDEX:COPY-PLACE 1D8C B9981D  LDA $1D98,Y ;FIND-OBJECT-INDEX:PLACES 
                     1D8F 99971D  STA $1D97,Y ;FIND-OBJECT-INDEX:ADJECTIVES 
                     1D92 88      DEY
                     1D93 D0F7    BNE $1D8C  ;FIND-OBJECT-INDEX:COPY-PLACE 
                     1D95 60      RTS
D-OBJECT-INDEX:NAMES 1D96 07..    DB $07
ECT-INDEX:ADJECTIVES 1D97 08..    DB $08
-OBJECT-INDEX:PLACES 1D98 02..    DB $02
INDEX:INITIAL-PLACES 1D99 02..    DB $02
INDEX:DESCRIPTION-HI 1D9A 2B..    DB $2B
INDEX:DESCRIPTION-LO 1D9B 0C..    DB $0C
               PARSE 1D9C A939    LDA #$39   ;LO : INPUT
                     1D9E 8500    STA $00    ;INP
                     1DA0 A91E    LDA #$1E   ;HI : INPUT
                     1DA2 8501    STA $01    ;INP + 1
 PARSER:PARSE-DIRECT 1DA4 A200    LDX #$00   ;X is our word pointer
                     1DA6 860C    STX $0C
                     ;Clear the parsed words buffer
                     1DA8 8E351E  STX $1E35  ;PARSER:WORDS 
                     1DAB 8E361E  STX $1E36
                     1DAE 8E371E  STX $1E37
                     1DB1 8E381E  STX $1E38
                     1DB4 A0FF    LDY #$FF
         PARSER:TRIM 1DB6 C8      INY
                     1DB7 B100    LDA ($00),Y
                     1DB9 F0FB    BEQ $1DB6  ;PARSER:TRIM 
    PARSER:NEXT-WORD 1DBB C028    CPY #$28
                     1DBD 1075    BPL $1E34  ;PARSER:DONE 
                     ;Now start a binary search
                     ;but first save word index in case
                     ;we need to resolve a collision
                     1DBF 840A    STY $0A
                     1DC1 A980    LDA #$80   ;Search depth
                     1DC3 850B    STA $0B
   PARSER:NEXT-ENTRY 1DC5 A40A    LDY $0A
                     1DC7 850D    STA $0D
                     1DC9 AA      TAX
                     1DCA B100    LDA ($00),Y
                     1DCC DD641E  CMP $1E64,X ;tbl1 - 1
                     1DCF D020    BNE $1DF1  ;PARSER:NOT-MATCH 
                     1DD1 C8      INY
                     1DD2 B100    LDA ($00),Y
                     1DD4 DD631F  CMP $1F63,X ;tbl2 - 1
                     1DD7 D018    BNE $1DF1  ;PARSER:NOT-MATCH 
                     ;Might be a one letter word
                     1DD9 C900    CMP #$00   ;Check for space
                     1DDB F02C    BEQ $1E09  ;PARSER:FOUND 
                     1DDD C8      INY
                     1DDE B100    LDA ($00),Y
                     1DE0 DD6220  CMP $2062,X ;tbl3 - 1
                     1DE3 D00C    BNE $1DF1  ;PARSER:NOT-MATCH 
                     ;Might be a two letter word
                     1DE5 C900    CMP #$00   ;Check for space
                     1DE7 F020    BEQ $1E09  ;PARSER:FOUND 
                     1DE9 C8      INY
                     1DEA B100    LDA ($00),Y
                     1DEC DD6121  CMP $2161,X ;tbl4 - 1
                     1DEF F018    BEQ $1E09  ;PARSER:FOUND 
    PARSER:NOT-MATCH 1DF1 100B    BPL $1DFE  ;PARSER:GT 
                     ;Less than...
                     1DF3 460B    LSR $0B
                     1DF5 F023    BEQ $1E1A  ;PARSER:NOT-FOUND 
                     1DF7 A50D    LDA $0D
                     1DF9 38      SEC
                     1DFA E50B    SBC $0B
                     ;Always jump... A != 0 since 128 - 64 ... - 1 = 1
                     1DFC D0C7    BNE $1DC5  ;PARSER:NEXT-ENTRY 
                     ;Greater than...
           PARSER:GT 1DFE 460B    LSR $0B
                     1E00 F018    BEQ $1E1A  ;PARSER:NOT-FOUND 
                     1E02 A50D    LDA $0D
                     1E04 18      CLC
                     1E05 650B    ADC $0B
                     1E07 D0BC    BNE $1DC5  ;PARSER:NEXT-ENTRY 
                     ;The word is in our dictionary
                     ;Let us look up its meaning
        PARSER:FOUND 1E09 A60D    LDX $0D
                     1E0B BD6022  LDA $2260,X ;Look up word meaning, 1 based
                     1E0E D00C    BNE $1E1C  ;PARSER:STORE-RESULT 
                     ;Colliding word, lets have a go with
                     ;the binary parser to resolve it
                     1E10 A40A    LDY $0A
                     1E12 208E22  JSR $228E  ;PARSER:BINARY-PARSER 
                     ;Since the binary parser isn't too fussy about
                     ;where it stops, rewind back to the beginning
                     ;of the word before we look for the next space
                     1E15 A40A    LDY $0A
                     1E17 4C1C1E  JMP $1E1C  ;PARSER:STORE-RESULT 
    PARSER:NOT-FOUND 1E1A A900    LDA #$00
 PARSER:STORE-RESULT 1E1C A60C    LDX $0C
                     1E1E 9D351E  STA $1E35,X ;PARSER:WORDS 
                     ;Advance to the next space
   PARSER:SEEK-SPACE 1E21 B100    LDA ($00),Y
                     1E23 F003    BEQ $1E28  ;PARSER:CONSUME-SPACE 
                     1E25 C8      INY
                     1E26 D0F9    BNE $1E21  ;PARSER:SEEK-SPACE 
PARSER:CONSUME-SPACE 1E28 C8      INY
                     1E29 B100    LDA ($00),Y
                     1E2B F0FB    BEQ $1E28  ;PARSER:CONSUME-SPACE 
   PARSER:FOUND-NEXT 1E2D E8      INX
                     1E2E 860C    STX $0C
                     1E30 E004    CPX #$04
                     1E32 D087    BNE $1DBB  ;PARSER:NEXT-WORD 
         PARSER:DONE 1E34 60      RTS
                     ;The parsed word meanings get put here
        PARSER:WORDS 1E35 00..    DBS (4)
                     ;The user input buffer
                     ;Terminated with many zeroes for convenience.
        PARSER:INPUT 1E39 00..    DBS (44)
                     ;Word tables, by character position
         PARSER:TBL1 1E65 01..    DB $01, $02, $03, $03, $03, $03, $03, $03, $04, $05, $05, $05, $05, $06, $06, $06, $07, $07, $07, $07, $08, $08, $0B, $0B, $0B, $0B, $0C, $0C, $0C, $0D, $0F, $0F, $0F, $10, $10, $10, $13, $13, $14, $14, $14, $15, $15, $17, $17, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A
         PARSER:TBL2 1F64 14..    DB $14, $12, $05, $08, $08, $0C, $0C, $12, $0F, $01, $0E, $18, $18, $09, $0C, $0F, $05, $0F, $12, $12, $09, $0F, $05, $05, $09, $0E, $09, $0F, $0F, $05, $0F, $10, $15, $01, $09, $15, $0C, $0C, $01, $01, $0F, $0E, $13, $01, $09, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A
         PARSER:TBL3 2063 14..    DB $14, $0F, $0C, $01, $05, $05, $0F, $01, $0F, $14, $14, $01, $09, $0E, $0F, $0F, $14, $00, $01, $05, $14, $0F, $19, $19, $0C, $0F, $03, $03, $0F, $14, $1A, $05, $14, $03, $03, $0E, $09, $0F, $0B, $13, $12, $0C, $05, $0C, $0E, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A
         PARSER:TBL4 2162 01..    DB $01, $0E, $0C, $09, $05, $01, $13, $03, $12, $00, $05, $0D, $14, $07, $0F, $04, $00, $00, $02, $05, $00, $0B, $00, $08, $0C, $03, $0B, $0B, $0B, $01, $05, $0E, $00, $0B, $0B, $03, $0D, $10, $05, $14, $03, $0F, $00, $0C, $04, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A, $1A
                     ;Table of word meanings
PARSER:WORD-MEANINGS 2261 06..    DB $06, $08, $1E, $21, $1A, $06, $15, $0B, $0D, $03, $1D, $01, $05, $13, $0A, $10, $02, $05, $02, $1F, $06, $07, $0C, $11, $06, $20, $03, $16, $01, $1C, $04, $17, $05, $1B, $02, $06, $04, $0F, $02, $03, $00, $12, $14, $09, $0E
                     ;Binary tree parser
                     ;#(TORCH TORCHES)
PARSER:BINARY-PARSER 228E B100    LDA ($00),Y
                     ;TORCH or TORCHES?
                     2290 C8      INY
                     2291 C8      INY
                     2292 C8      INY
                     2293 C8      INY
                     2294 C8      INY
                     2295 B100    LDA ($00),Y
                     2297 C901    CMP #$01   ;[5] >= !
                     2299 B003    BCS $229E  ;BINARY-PARSER:2 
                     229B A919    LDA #$19   ;TORCH (TORCH)
                     229D 60      RTS
     BINARY-PARSER:2 229E A918    LDA #$18   ;TORCHES (TORCHES)
                     22A0 60      RTS
            DISPATCH 22A1 A902    LDA #$02
                     22A3 850C    STA $0C
                     ;Choose the location based table
                     22A5 A51F    LDA $1F    ;LOCATION-DISPATCH-TABLE
                     22A7 8500    STA $00    ;DISPATCH-TABLE
                     22A9 A520    LDA $20    ;LOCATION-DISPATCH-TABLE + 1
                     22AB 8501    STA $01    ;DISPATCH-TABLE + 1
 DISPATCHER:DISPATCH 22AD A000    LDY #$00
                     22AF B100    LDA ($00),Y
                     ;Is this the last entry in the dispatch table?
                     22B1 D00F    BNE $22C2  ;DISPATCHER:COMPARE-WORD 
                     22B3 C60C    DEC $0C
                     ;We only try 2 tables, then return
                     22B5 D001    BNE $22B8  ;DISPATCHER:TRY-GENERIC 
                     22B7 60      RTS
                     ;Choose the generic table
SPATCHER:TRY-GENERIC 22B8 A9B9    LDA #$B9   ;LO : GENERIC
                     22BA 8500    STA $00    ;DISPATCH-TABLE
                     22BC A923    LDA #$23   ;HI : GENERIC
                     22BE 8501    STA $01    ;DISPATCH-TABLE + 1
                     22C0 A000    LDY #$00
PATCHER:COMPARE-WORD 22C2 A200    LDX #$00
DISPATCHER:NEXT-WORD 22C4 B100    LDA ($00),Y
                     22C6 F005    BEQ $22CD  ;DISPATCHER:MATCHED-WORD  ;0 always matches
                     22C8 DD351E  CMP $1E35,X ;PARSER:WORDS 
                     22CB D005    BNE $22D2  ;DISPATCHER:NEXT-INPUT-WORD 
                     ;Great work kid, now match another one.
PATCHER:MATCHED-WORD 22CD C8      INY
                     22CE C003    CPY #$03
                     22D0 F014    BEQ $22E6  ;DISPATCHER:MATCHED-SENTENCE 
CHER:NEXT-INPUT-WORD 22D2 E8      INX
                     22D3 E004    CPX #$04
                     22D5 D0ED    BNE $22C4  ;DISPATCHER:NEXT-WORD 
                     ;We exhausted the input words
                     ;Skip to next entry in dispatch table
PATCHER:NEXT-HANDLER 22D7 18      CLC
                     22D8 A500    LDA $00    ;DISPATCH-TABLE
                     22DA 6905    ADC #$05
                     22DC 8500    STA $00    ;DISPATCH-TABLE
                     22DE A501    LDA $01    ;DISPATCH-TABLE + 1
                     22E0 6900    ADC #$00
                     22E2 8501    STA $01    ;DISPATCH-TABLE + 1
                     ;Assume the carry is clear after long add
                     22E4 90C7    BCC $22AD  ;DISPATCHER:DISPATCH 
                     ;Push the dispatch address on the stack
HER:MATCHED-SENTENCE 22E6 B100    LDA ($00),Y
                     22E8 48      PHA
                     22E9 C8      INY
                     22EA B100    LDA ($00),Y
                     22EC 48      PHA
                     ;Call the handler, which will return to the top-level caller
                     22ED 60      RTS
                     ;DUNGEON-CELL dispatch table
                     ;EXAMINE SLIME ?  -> DUNGEON-CELL:EXAMINE-SLIME
PATCHER:DUNGEON-CELL 22EE 01..    DB $01, $04, $00, $0A, $EA
                     ;EXAMINE WALL ?  -> DUNGEON-CELL:EXAMINE-WALL
                     22F3 01..    DB $01, $09, $00, $0A, $F9
                     ;EXAMINE FLOOR ?  -> DUNGEON-CELL:EXAMINE-FLOOR
                     22F8 01..    DB $01, $0A, $00, $0B, $0B
                     ;EXAMINE CRACK ?  -> DUNGEON-CELL:EXAMINE-CRACK
                     22FD 01..    DB $01, $0B, $00, $0B, $1A
                     ;TAKE KEY ?  -> DUNGEON-CELL:TAKE-KEY
                     2302 02..    DB $02, $0C, $00, $0B, $3B
                     ;EXAMINE DOOR ?  -> DUNGEON-CELL:EXAMINE-DOOR
                     2307 01..    DB $01, $0D, $00, $0B, $5B
                     ;EXAMINE WINDOW ?  -> DUNGEON-CELL:EXAMINE-WINDOW
                     230C 01..    DB $01, $0E, $00, $0B, $72
                     ;EXAMINE SLOP ?  -> DUNGEON-CELL:EXAMINE-SLOP
                     2311 01..    DB $01, $0F, $00, $0B, $90
                     ;EAT FOOD ?  -> DUNGEON-CELL:EAT-FOOD
                     2316 03..    DB $03, $10, $00, $0B, $9A
                     ;EAT SLOP ?  -> DUNGEON-CELL:EAT-SLOP
                     231B 03..    DB $03, $0F, $00, $0B, $A4
                     ;EXAMINE KEY ?  -> DUNGEON-CELL:EXAMINE-KEY
                     2320 01..    DB $01, $0C, $00, $0B, $AE
                     ;EXAMINE KEYHOLE ?  -> DUNGEON-CELL:EXAMINE-KEYHOLE
                     2325 01..    DB $01, $11, $00, $0B, $C0
                     ;TAKE CRACK ?  -> DUNGEON-CELL:TAKE-CRACK
                     232A 02..    DB $02, $0B, $00, $0B, $C6
                     ;LICK CRACK ?  -> DUNGEON-CELL:LICK-CRACK
                     232F 03..    DB $03, $0B, $00, $0B, $CC
                     ;ATTACK SLIME ?  -> DUNGEON-CELL:ATTACK-SLIME
                     2334 06..    DB $06, $04, $00, $0B, $D2
                     ;UNLOCK DOOR FINGER  -> DUNGEON-CELL:UNLOCK-DOOR-FINGER
                     2339 12..    DB $12, $0D, $13, $0B, $D8
                     ;UNLOCK DOOR ?  -> DUNGEON-CELL:UNLOCK-DOOR
                     233E 12..    DB $12, $0D, $00, $0B, $EF
                     ;USE KEY DOOR  -> DUNGEON-CELL:USE-KEY-DOOR
                     2343 14..    DB $14, $0C, $0D, $0B, $EF
                     ;CLOSE DOOR ?  -> DUNGEON-CELL:CLOSE-DOOR
                     2348 15..    DB $15, $0D, $00, $0C, $1B
                     ;LOCK DOOR ?  -> DUNGEON-CELL:LOCK-DOOR
                     234D 16..    DB $16, $0D, $00, $0C, $2C
                     ;EXIT ? ?  -> DUNGEON-CELL:EXIT
                     2352 05..    DB $05, $00, $00, $0C, $43
                     ;USE DOOR ?  -> DUNGEON-CELL:USE-DOOR
                     2357 14..    DB $14, $0D, $00, $0C, $43
                     ;LICK SLIME ?  -> DUNGEON-CELL:LICK-SLIME
                     235C 03..    DB $03, $04, $00, $0C, $5A
                     ;OPEN DOOR ?  -> DUNGEON-CELL:OPEN-DOOR
                     2361 17..    DB $17, $0D, $00, $0C, $74
                     ;Terminating byte for DUNGEON-CELL
                     2366 00..    DB $00
                     ;CORRIDOR dispatch table
                     ;EXAMINE TORCHES ?  -> CORRIDOR:EXAMINE-TORCHES
 DISPATCHER:CORRIDOR 2367 01..    DB $01, $18, $00, $11, $19
                     ;EXAMINE TORCH ?  -> CORRIDOR:EXAMINE-TORCH
                     236C 01..    DB $01, $19, $00, $11, $19
                     ;TAKE TORCH ?  -> CORRIDOR:TAKE-TORCH
                     2371 02..    DB $02, $19, $00, $11, $22
                     ;CHEEZOWS ? ?  -> CORRIDOR:CHEEZOWS
                     2376 1A..    DB $1A, $00, $00, $11, $2B
                     ;PACKET ? ?  -> CORRIDOR:PACKET
                     237B 1B..    DB $1B, $00, $00, $11, $2B
                     ;EXAMINE METAPHOR ?  -> CORRIDOR:EXAMINE-METAPHOR
                     2380 01..    DB $01, $1C, $00, $11, $3D
                     ;ENTER CELL ?  -> CORRIDOR:ENTER-CELL
                     2385 1D..    DB $1D, $1E, $00, $11, $43
                     ;ENTER GREEN DOOR  -> CORRIDOR:ENTER-GREEN-DOOR
                     238A 1D..    DB $1D, $1F, $0D, $11, $49
                     ;ENTER CELL DOOR  -> CORRIDOR:ENTER-CELL-DOOR
                     238F 1D..    DB $1D, $1E, $0D, $11, $5B
                     ;EXAMINE WALL ?  -> CORRIDOR:EXAMINE-WALL
                     2394 01..    DB $01, $09, $00, $11, $61
                     ;ENTER DOOR ?  -> CORRIDOR:ENTER-DOOR
                     2399 1D..    DB $1D, $0D, $00, $11, $78
                     ;EXAMINE GREEN DOOR  -> CORRIDOR:EXAMINE-GREEN-DOOR
                     239E 01..    DB $01, $1F, $0D, $11, $7E
                     ;KNOCK GREEN DOOR  -> CORRIDOR:KNOCK-GREEN-DOOR
                     23A3 20..    DB $20, $1F, $0D, $11, $84
                     ;KNOCK DOOR ?  -> CORRIDOR:KNOCK-DOOR
                     23A8 20..    DB $20, $0D, $00, $11, $9E
                     ;Terminating byte for CORRIDOR
                     23AD 00..    DB $00
                     ;FRAZBOLGS-CLOSET dispatch table
                     ;EXAMINE CHAIR ?  -> FRAZBOLGS-CLOSET:EXAMINE-CHAIR
HER:FRAZBOLGS-CLOSET 23AE 01..    DB $01, $21, $00, $16, $BF
                     ;EXIT ? ?  -> FRAZBOLGS-CLOSET:EXIT
                     23B3 05..    DB $05, $00, $00, $16, $C5
                     ;Terminating byte for FRAZBOLGS-CLOSET
                     23B8 00..    DB $00
                     ;GENERIC dispatch table
                     ;EXAMINE ? ?  -> GENERIC:EXAMINE
  DISPATCHER:GENERIC 23B9 01..    DB $01, $00, $00, $16, $CB
                     ;TAKE KEY ?  -> GENERIC:TAKE-KEY
                     23BE 02..    DB $02, $0C, $00, $17, $06
                     ;Terminating byte for GENERIC
                     23C3 00..    DB $00
                     ;String Table 69 entries
                     23C4 B0..    DCS 'You already have the key! Perhaps the
dungeon air is getting to you.' (68->33)
                     23E5 11..    DCS 'I don't know what that is.' (26->14)
                     23F3 11..    DCS 'It's broken.' (12->7)
                     23FA B0..    DCS 'You are in the well-appointed
closet of the goblin Frazbolg.
Over centuries of guarding
his prisoners he has amassed
an impressive collection of
posessions, a spare loin
cloth-tattered, a toaster and
a hundred-year-old copy of
Modern Necromancer
magazine.' (255->156)
                     2496 0E..    DCS 'FRAZBOLG'S CLOSET' (17->18)
                     24A8 1D..    DCS 'Which one?' (10->7)
                     24AF 17..    DCS 'Presently, it swings open. There appears to
be some sort of lodging beyond the
threshold.' (89->45)
                     24DC B0..    DCS 'You knock on the door and wait patiently.' (41->18)
                     24EE 17..    DCS 'Politely, you knock on the already open
green door, but theres is no answer.' (76->39)
                     2515 B0..    DCS 'You hear mumbling and sighing from behind
the door.' (51->26)
                     252F B0..    DCS 'You will need to be more specific.' (34->17)
                     2540 11..    DCS 'It is too dark at this end of the corridor to
see anything.' (59->29)
                     255D 0D..    DCS 'Etched on the wall is a diagram. A triangle
sits inside a circle, surrounded by flames. A
spell perhaps?' (104->63)
                     259C A7..    DCS 'The green door is closed.' (25->11)
                     25A7 B0..    DCS 'You enter your cell.' (20->10)
                     25B1 5B..    DCS 'That is really taking the biscuit.' (34->17)
                     25C2 1D..    DCS 'What a strange thing to say.' (28->13)
                     25CF A7..    DCS 'There are no Cheezows! It was a metaphor
for your situation.' (60->36)
                     25F3 B0..    DCS 'You take one of the torches.' (28->13)
                     2600 A7..    DCS 'The flickering shadows make you think of
something profound, like a packet of
Cheezows caught in the wind.' (106->56)
                     2638 09..    DCS 'A torch-lit corridor. You see
a row of cell doors, identical
to your own. Moans and pleas
waft through the bars-sounds
which you are not entirely
certain are human in origin.
At one end, a brick wall, at the
other a green door, different
than all the rest.' (256->140)
                     26C4 0B..    DCS 'CORRIDOR' (8->9)
                     26CD A7..    DCS 'The door creaks open.' (21->9)
                     26D6 A7..    DCS 'The door is locked.' (19->7)
                     26DD 10..    DCS 'Have you been licking the slime? It's
hallucinogenic. The door, not atypically for
a dungeon, is locked.' (104->56)
                     2715 A7..    DCS 'The door is already open.' (25->11)
                     2720 14..    DCS 'Myriad colours break in waves upon your
ears. Maia's cosmic tears rain down on you
in a shower of gold. The slime smiles.' (121->66)
                     2762 0E..    DCS 'Far out!' (8->6)
                     2768 15..    DCS 'Nothing happens. Your third eye is already
open. But... you do feel a strange urge to
grow a beard and play the guitar.' (119->62)
                     27A6 16..    DCS 'Ouch! You walk into the closed door.' (36->19)
                     27B9 A7..    DCS 'The lock mechanism clicks shut. You really
have got it in for yourself haven't you?' (83->43)
                     27E4 A7..    DCS 'The door is already locked.' (27->11)
                     27EF A7..    DCS 'The door closes.' (16->8)
                     27F7 A7..    DCS 'The door is already unlocked.' (29->12)
                     2803 B0..    DCS 'You rattle the key in the lock, but there is
a key stuck in the other side.' (75->36)
                     2827 A7..    DCS 'The lock mechanism clicks...' (28->14)
                     2835 1D..    DCS 'With what? Your finger?' (23->13)
                     2842 B0..    DCS 'You put your finger in the keyhole of an
unlocked door.' (55->25)
                     285B B0..    DCS 'You hear the faint sound of snickering...' (41->17)
                     286C 1D..    DCS 'Wise guy, eh? The lock doesn't budge. Your
finger is now sore.' (62->35)
                     288F B0..    DCS 'Your hand is stayed by the slime's gaze of
infinite sadness.' (60->31)
                     28AE 11..    DCS 'Inadvisable.' (12->9)
                     28B7 11..    DCS 'It's a keyhole, man.' (20->11)
                     28C2 1D..    DCS 'What key?' (9->6)
                     28C8 11..    DCS 'It's a key, man.' (16->8)
                     28D0 A7..    DCS 'The gods look away in shame.' (28->14)
                     28DE 11..    DCS 'I would hardly call the goblin's slop food.' (43->24)
                     28F6 09..    DCS 'A balanced soup of entrails, small
amphibians and mandibles. Ooh! Garlic
croutons!' (82->54)
                     292C 10..    DCS 'He flings some inedible slop through the
bars. You hear a key rattling in the lock.' (83->42)
                     2956 10..    DCS 'He tells you to keep the noise down using a
stream of vowel-free goblin profanities.
KRRPKCHK DRGKPK!' (101->63)
                     2995 09..    DCS 'A goblin appears at the window.' (31->16)
                     29A5 A7..    DCS 'The door is closed.' (19->8)
                     29AD A7..    DCS 'The door is open.' (17->7)
                     29B4 A7..    DCS 'The door is a grim iron affair with a tiny
barred window and a keyhole.' (71->40)
                     29DC 1D..    DCS 'What key? Do you know something I don't?' (40->23)
                     29F3 11..    DCS 'It's in your pocket... Perhaps the dungeon
air is getting to you?' (65->33)
                     2A14 B0..    DCS 'You take the shiny key.' (23->10)
                     2A1E A7..    DCS 'The Veil of Maia prevents you from seeing
anything interesting.' (63->33)
                     2A3F 09..    DCS 'A crack in the floor, just like any other.
One might hide a small key-like object here.
Like, for example, a key.' (113->65)
                     2A80 09..    DCS 'A glint of metal shines back at you... A key!' (45->23)
                     2A97 17..    DCS 'Perhaps it bears further examination?' (37->22)
                     2AAD A7..    DCS 'There is a crack in the floor.' (30->13)
                     2ABA 0D..    DCS 'Eugh! The slime is looking at you!' (34->19)
                     2ACD A7..    DCS 'The wall oozes with a repellant green slime.' (44->21)
                     2AE2 A7..    DCS 'They seem to be staring at the floor.' (37->17)
                     2AF3 14..    DCS 'Millions of sad eyes peer out from the
slime.' (45->25)
                     2B0C 09..    DCS 'A sharp metal hook with a spherical handle.' (43->23)
                     2B23 B0..    DCS 'You are in the dungeon
prison of Wangband under
the fortress of the Black
Wizard, Beelzepops. Home to
stench-rats, were-toads,
sniveling goblins and you. Of
the current denizens, you are
currently the most wretched.
A lime-green slime oozes out
of the wall, making a rasping, wheezing
sound. You must escape, but your cell has a
door...' (336->186)
                     2BDD 0C..    DCS 'DUNGEON CELL' (12->13)
          DECOMPRESS 2BEA 850C    STA $0C    ;Store the height
                     ;Get the LFB
                     2BEC A200    LDX #$00
                     2BEE 860E    STX $0E
                     2BF0 A100    LDA ($00,X)
                     2BF2 850A    STA $0A
                     ;Get a byte from the data and either emit
                     ;it to the screen or check for the special
                     ;LFB which signals a pattern
     DECOMPRESS:NEXT 2BF4 E600    INC $00    ;DATA
                     2BF6 D002    BNE $2BFA  ;$2BF4:INC16-DONE 
                     2BF8 E601    INC $01    ;DATA + 1
    $2BF4:INC16-DONE 2BFA A100    LDA ($00,X)
                     2BFC C50A    CMP $0A
                     2BFE F006    BEQ $2C06  ;DECOMPRESS:PATTERN 
                     2C00 20672C  JSR $2C67  ;DECOMPRESS:EMIT 
                     2C03 4CF42B  JMP $2BF4  ;DECOMPRESS:NEXT 
                     ;Found a pattern. Extract the offset and
                     ;set up a src pointer and a column to copy from
  DECOMPRESS:PATTERN 2C06 E600    INC $00    ;DATA
                     2C08 D002    BNE $2C0C  ;$2C06:INC16-DONE 
                     2C0A E601    INC $01    ;DATA + 1
    $2C06:INC16-DONE 2C0C A100    LDA ($00,X)
                     2C0E 290F    AND #$0F
                     2C10 F012    BEQ $2C24  ;DECOMPRESS:ROW-ABOVE 
                     ;Pattern is on the same row, so we use the same
                     ;scanline pointer for the source, but offset the
                     ;column index by the amount in the lo-nybble
                     2C12 49FF    EOR #$FF
                     2C14 38      SEC
                     2C15 650E    ADC $0E
                     2C17 850D    STA $0D
                     2C19 A502    LDA $02    ;DEST
                     2C1B 8504    STA $04    ;SRC
                     2C1D A503    LDA $03    ;DEST + 1
                     2C1F 8505    STA $05    ;SRC + 1
                     2C21 4C352C  JMP $2C35  ;DECOMPRESS:COPY 
DECOMPRESS:ROW-ABOVE 2C24 A502    LDA $02    ;DEST
                     2C26 38      SEC
                     2C27 E928    SBC #$28
                     2C29 8504    STA $04    ;SRC
                     2C2B A503    LDA $03    ;DEST + 1
                     2C2D E900    SBC #$00
                     2C2F 8505    STA $05    ;SRC + 1
                     2C31 A50E    LDA $0E
                     2C33 850D    STA $0D
                     ;Get the pattern width from the hi-nybble
     DECOMPRESS:COPY 2C35 A100    LDA ($00,X)
                     2C37 4A      LSR
                     2C38 4A      LSR
                     2C39 4A      LSR
                     2C3A 4A      LSR
                     2C3B 18      CLC
                     2C3C 6903    ADC #$03
                     2C3E AA      TAX
OMPRESS:NEXT-PATTERN 2C3F A50D    LDA $0D
                     2C41 C50B    CMP $0B
                     2C43 D011    BNE $2C56  ;DECOMPRESS:NOT-WRAPPED 
                     2C45 18      CLC
                     2C46 A504    LDA $04    ;SRC
                     2C48 6928    ADC #$28
                     2C4A 8504    STA $04    ;SRC
                     2C4C A505    LDA $05    ;SRC + 1
                     2C4E 6900    ADC #$00
                     2C50 8505    STA $05    ;SRC + 1
                     2C52 A900    LDA #$00
                     2C54 850D    STA $0D
COMPRESS:NOT-WRAPPED 2C56 A8      TAY
                     2C57 B104    LDA ($04),Y
                     2C59 A40E    LDY $0E
                     2C5B 9102    STA ($02),Y
                     2C5D 20672C  JSR $2C67  ;DECOMPRESS:EMIT 
                     2C60 E60D    INC $0D
                     2C62 CA      DEX
                     2C63 D0DA    BNE $2C3F  ;DECOMPRESS:NEXT-PATTERN 
                     2C65 F08D    BEQ $2BF4  ;DECOMPRESS:NEXT 
                     ;Emit a byte to the screen
     DECOMPRESS:EMIT 2C67 A40E    LDY $0E
                     2C69 9102    STA ($02),Y
                     2C6B C8      INY
                     2C6C 98      TYA
                     2C6D C50B    CMP $0B
                     2C6F D016    BNE $2C87  ;DECOMPRESS:EMIT-END 
                     ;Scanline wrap
                     2C71 18      CLC
                     2C72 A502    LDA $02    ;DEST
                     2C74 6928    ADC #$28
                     2C76 8502    STA $02    ;DEST
                     2C78 A503    LDA $03    ;DEST + 1
                     2C7A 6900    ADC #$00
                     2C7C 8503    STA $03    ;DEST + 1
                     2C7E A000    LDY #$00
                     2C80 C60C    DEC $0C
                     2C82 D003    BNE $2C87  ;DECOMPRESS:EMIT-END 
                     ;We are done, pop the stack return directly to caller
                     2C84 68      PLA
                     2C85 68      PLA
                     2C86 60      RTS
 DECOMPRESS:EMIT-END 2C87 840E    STY $0E
                     2C89 60      RTS
                     ;Font tables. First byte of each character is the width
                     ;Characters are stored upside down
                 END
              PROMPT 2C8A 03..    DB $03, $00, $00, $80, $40, $20, $40, $80, $00, $00, $00
~~~~