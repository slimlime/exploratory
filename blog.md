## 18/6/2017 Improved decompressor

Having been very disappointed with the compression ratio, particularly for the face image which is mostly whitespace, I decided to improve the decompressor so it could handle pattern matches that span over a scanline. First attempt was a disaster, there was too much state knocking about and too many edge cases. The re-write uses a co-routine style (not technically a co-routine) where there are to co-operating bits of code. One advances the data pointer and the pattern pointer, and the other emits bytes to the screen and takes care of scanline wrapping. This was much simpler to understand, though there is now an inefficiency in that we are doing a JSR for each byte emitted. Since this is an adventure game rather than a sprite shoot-em-up, it will probably be ok.

~~~~
           Original   Simple   Improved
	   
cellardoor     1352     1073   1070
porsche        1352     1182   1141
maxine         1352     1054    817
garagedoor     1352             777
odysseus       1352            1054

~~~~

All that was required in the compression routine was to remove the check for the end of the scanline.

### Neat return trick.

In a normal language, like C, if you are deep within a call stack and you want to escape, it's tricky, you have to return all the way up and hope you don't get shafted by one of the functions that calls you. In assembler, it's easy,

~~~~

    (DEC.ZP :imgh)
    (BNE :emit-end)
    (dc "We are done, pop the stack return directly to caller")
    (PLA)
    (PLA)
    (RTS)

~~~~

The code that emits bytes detected the end of the image, so why bother going up a really boring call stack? This means less code checking for conditions in places that would rather concentrate on something else. Of course this is a total violation of all the rules that have grown up around programming. Who cares? One of the principles of this blog is LIBRARY not FRAMEWORK. Let us use the things that work for us, not vice-versa.

### Reverse Subtraction

Another neat trick for you 6502 nuts is reverse subtraction. Here we want to subtract an offset, which is in the accumulator from a column number stored in the zeropage. One way to do it is by storing the offset in a temporary location, loading the column number then doing SBC with the temporary. Or, you can remember that A - B = A + (-B) = (-B) + A. School maths, it wasn't a waste of time. (h/t CODEBASE 64 and NESDEV)

~~~~
    (EOR #xFF)
    (SEC)
    (ADC.ZP :dest-y)
~~~~

## 17/6/2017 Decompress

Exciting news! The decompressor works, and here is the proof. The layout of the actual game will be very similar to the this, except there will be a location title and a space for live-actions and user input at the bottom.

![Alt text](blog/odysseus.png)

Here is some disassembly for you to peruse. This is the best bit of programming, you spend ages thinking about something that is mathematically plausible and should work in theory and then type out some meagre list of instructions which actually goes about doing it.  It helps that the algorithm is tested in LISP so that we don't have to spend ages going round the reasonably slow edit-compile-debug cycle for 6502. For this simple function I didn't need to use the single step monitor once. As usual, the problems were mostly of the OBOE variety.

~~~~

 ;+ DECOMPRESS
             ;Expect the height in A
  DECOMPRESS 09E6 850A    STA $0A
             09E8 A000    LDY #$00
             09EA B100    LDA ($00),Y
             09EC 8508    STA $08
             09EE E600    INC $00            ;SRC
             09F0 D002    BNE $09F4          ;COPY-ROW
             09F2 E601    INC $01            ;SRC + 1
    COPY-ROW 09F4 A200    LDX #$00
             09F6 A000    LDY #$00
             09F8 A50C    LDA $0C
             09FA 8509    STA $09
             09FC A90D    LDA #$0D
   COPY-BYTE 09FE A100    LDA ($00,X)        ;X should be zero
             0A00 E600    INC $00            ;SRC
             0A02 D002    BNE $0A06
             0A04 E601    INC $01            ;SRC + 1
             0A06 C508    CMP $08
             0A08 F019    BEQ $0A23          ;PATTERN
             0A0A 9102    STA ($02),Y
             0A0C C8      INY
             0A0D C609    DEC $09
             0A0F D0ED    BNE $09FE          ;COPY-BYTE
     ROW-END 0A11 18      CLC
             0A12 A502    LDA $02            ;DEST
             0A14 6928    ADC #$28
             0A16 8502    STA $02            ;DEST
             0A18 A503    LDA $03            ;DEST + 1
             0A1A 6900    ADC #$00
             0A1C 8503    STA $03            ;DEST + 1
             0A1E C60A    DEC $0A
             0A20 D0D2    BNE $09F4          ;COPY-ROW
             0A22 60      RTS
     PATTERN 0A23 A100    LDA ($00,X)        ;X should be zero
             ;Get the pattern offset from the lo-nybble
             0A25 290F    AND #$0F
             0A27 D003    BNE $0A2C          ;SAME-ROW
             ;Pattern is on the row above
             0A29 18      CLC
             0A2A 6928    ADC #$28
    SAME-ROW 0A2C 850B    STA $0B
             0A2E A502    LDA $02            ;DEST
             0A30 38      SEC
             0A31 E50B    SBC $0B
             0A33 8504    STA $04            ;PREV
             0A35 A503    LDA $03            ;DEST + 1
             0A37 E900    SBC #$00
             0A39 8505    STA $05            ;PREV + 1
             ;Get the length from high nybble
             0A3B A100    LDA ($00,X)
             0A3D 4A      LSR
             0A3E 4A      LSR
             0A3F 4A      LSR
             0A40 4A      LSR
             0A41 18      CLC
             0A42 6903    ADC #$03
             0A44 AA      TAX
             ;Advance past the match byte
             0A45 E600    INC $00            ;SRC
             0A47 D002    BNE $0A4B          ;PATTERN-NEXT
             0A49 E601    INC $01            ;SRC + 1
             ;Now we have offset in PREV and length in X
PATTERN-NEXT 0A4B B104    LDA ($04),Y
             0A4D 9102    STA ($02),Y
             0A4F C8      INY
             0A50 C609    DEC $09
             0A52 F0BD    BEQ $0A11          ;ROW-END ;End of pattern and row
             0A54 CA      DEX
             0A55 F0A7    BEQ $09FE          ;COPY-BYTE ;End of pattern
             0A57 D0F2    BNE $0A4B          ;PATTERN-NEXT
             ;- DECOMPRESS
         ODD 0A59 09      /home/dan/Downloads/odd.bmp pixels (1166)
         ODD 0EE7 00      /home/dan/Downloads/odd.bmp colours (162)
         STR 0F89 CF      DCS 'Tell me, O muse, of that

~~~~

## 15/6/2017 The Compresssion Rabbit Hole

I have spent the last two days down the compression rabbit hole. The further down you go, the fewer bytes there are to be had and the more desperate the search becomes. Rather than bore you all with the details, here is the very simple scheme I will be using. It meets these criteria,

- It's better than nothing
- It promises more in the future, should we need it
- It should be very simple to implement the decompressor in 6502

So, yaggers comes to the rescue once more. (If you want to see how I got to this sorry state, check out the excruciatingly detailed post which immediately precedes this.)

As you can see, by choosing the simple scheme we are leaving potentially a few hundred bytes on the table. This is a good thing however, as we can now get on with our lives. It also means that if we are really struggling toward the end we can do a bit of improvement here to help out.



~~~~
           Original   Simple   Best   Attributes Simple Total   %

cellardoor     1352     1073    995          169    131  1204  79
porsche        1352     1182   1042          169    154  1336  87
maxine         1352     1054    762          169    154  1213  80

~~~~

![Alt text](/blog/stairs.png) ![Alt text](/blog/porsche.png) ![Alt text](/blog/maxine.png)

We signal a pattern that has been seen before with a special byte, the LFB. This least frequent byte is output as the first byte of the compressed output so the decompressor knows what it is looking for. Following that are literal bytes. Each time we see a pattern that we have seen before either on that row, or on the scanline directly above, we emit the LFB followed by a byte with the following structure.

(Note, if we ever see the LFB in the wild, that is, in the input data we are replacing it with a 0...)

~~~~

    Length  Offset
Bit 7654    3210

~~~~

The hi-nybble specifies the length, with the values 0-15 encoding lengths of 3-18. The lo-nybble contains the offset from the current position where 0 encodes the special position of the line directly above. Offsets cannot go past the beginning of the row, and patterns cannot go past the end of the row. This limits the compression we get, but makes the decompressor very much simpler (in the 6502 implementation at least, in the LISP test decompressor it doesn't really make a difference).


## 14/6/2017 Boring very detailed post about image compression investigations.

This post is just some free-form experimentation. This is the black hole I was talking about in tomorrow's post; the experimentation is endless.

Using the scheme where we encode an already seen pattern as

~~~~

LFB
4 BITS WIDTH
4 BITS HI-OFFSET
8 BITS LO-OFFSET

~~~~


The pretty bobbed face goes from 1352->869 bytes. This is ok, but I was aiming for less than two pages of memory for each image. Given that there are attribute values to add, another 169 bytes, it isn't looking good. So I performed a bit of analysis on the values. It's all very well going back 4096 bytes to look for a pattern match and this does indeed improve the compression, but not by as much as one would think. Additionally, patterns are usually quite short, so the widths follow a distribution. Some compression schemes use a variable bit encoding for the width field which follows more closely the distribution of observed widths.

Here is the pattern width histogram for the face,

~~~~

#(0 0 0 0 33 28 15 18 8 11 10 4 6 0 0 0 0 0 1)

~~~~

As you can see there are no patterns at length 0-3 as they would be a net loss, then lots of patterns upto length 12, not so many after that and this is with searching the entire image!

This begs the question- are there more patterns of length 3? And do we care about searching the whole image? Images are highly spatially coherent- each pixel has a higher likelyhood of being like a pixel near to it. Searching massive spans is hardly worth it.

## Two byte encoding

~~~~

LFB
3 BITS WIDTH
5 BITS OFFSET

~~~~

Two bytes, so now three byte patterns are on the menu, as long as they are within 31 bytes. (31 bytes is enough for a row in the kind of images we are compressing).

Here is the updated histogram,

~~~~
#(0 0 0 26 31 21 12 12 11 11 17)
~~~~

And the surprise, the pretty face is now down from 1352->814 bytes. So, extending the search window is not worth the expense of an exta byte for every occurance of a pattern.

Here is more of a surprise, if we change it so that we only search up to 15 bytes back and
have a max length of 19, i.e. we use a nybble for the width and a nybble for the offset we get 806 bytes. Shocking. If we reduce it any further we will have RLE!!

~~~~

#(0 0 0 24 35 22 9 12 11 10 4 4 7 0 0 0 0 0 1 0)

~~~~

On the face of it, this histogram is suggesting that there are no gains to be had by increasing the width field even further, but for the sake of perversity let's do it anyway. 5 bits for width, 3 bits for offset.

The histogram again,

~~~~

#(0 0 0 53 43 29 4 3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

~~~~

Sanity has now prevailed- there is some merit in searching back further than 7 bytes. The new compressed size is 1089.

So the best scheme so far is,

~~~~

LFB
4 BITS WIDTH, i.e. 3-18 bytes
4 BITS OFFSET, i.e. 1-16 bytes

~~~~

I mentioned before that images have high spatial coherence and unlike text (which is also spatially coherent) have two dimensions. To exploit this fact we can guess that the row above is likely very similar to the row we are now on. Perhaps we can match with the row directly above us?

Extending the offset in the test compressor to 40 bytes (these images are 13 bytes wide) gives this histogram for *offsets*. Note that we cannot actually encode offsets of up to 40 bytes, I am just running the compressor to see how many patterns it can match and at what offsets.

~~~~

Offset histogram.       WOW!

7 2 0 0 4 5 7 4 0 0 1 3 90 0 0 2 1 1 0 0 0 1 0 1 1 4 0 0 0 1 1 1 0 0 0 1 1 3
  0 0 0

~~~~

Look at that, a massive peak at exactly 13 bytes! This is the coherence I was talking about, pixels are very much more likely to be the same as the pixel directly above them than any other randomly chosen pixel.

## Gaps

In an earlier post I described how we have to go directly from the stored image data to the screen. This means that we can't directly use the offsets we have calculated- the screen buffer has a different width than the image buffer. From the results I have found here my solution will be to use the following encoding scheme

0        1
76543210 7654  3210
LFB      Width Offset

Where

Width  = 0-15 => 3-19 bytes
Offset = 1-30 => 1-30 bytes on the same row
         0, to indicate the byte directly above. This is equivalent to an offset equal to the image width.

I have a sneaking suspicion that when I try this out, it will be better to use 5 bits for the width and 3 bits for the offset since the images are only 13 bytes wide and we can't make use of offsets larger than that. The earlier tests allowed the patterns to match straddling the rows. Let us see if this bears out...


Compression results, all files 1352 bytes (+ 169 attribute bytes which is not included)

~~~~

stairs 1070
face 817
porsche 1141

~~~~

Next is the redundancy in the attributes. We know from the conversion routine that we can have attributes that have the same foreground and background colour. This implies that the bitmap data is for this square.

We have already seen that inverting the attribute from foreground to background to reduce the number of set bits can save a few bytes (838 vs 817 for the face), but we should also insist that the foreground colour = the background colour when there are no set bits.

Omitting the bytes where the attribute is one colour gives the following results

~~~~

stairs 1068
porsche 1098
face 787

~~~~

Nothing to write home about in terms of improvement; probably not worth it in terms of the complexity it would involve in the decompressor. A pointer would have to be set up to track the attribute to test for emptiness.

I am disappointed by the level of compression so far so now I am going to try some more extreme tricks before moving on.

- Using an extra LFB to exploit 2 byte coincidence on the line above

Whenever we see two bytes the same directly above us, we can use a single byte to represent it. Also included is if we use 3 special bytes, the additional one signalling a three byte ocorrespondence.

~~~~

        2 LFBs 3 LFBs
stairs  1022      995
porsche 1092     1042
face    786       762

~~~~

Not bad, it's certainly easier to handle in the decompressor than the attribute hack and it gives us about the same gain.

In the end I decided to go with one LSB and pattern matching on the same row minus and offset, and pattern matching directly on the row above. The other schemes add complexity, and complexity in the decoder is what we don't want. Adopting this scheme will make the 6502 decompresser very simple indeed and it should be fast, not much slower than copying from memory.

Note on compressing the colour attributes. Each image has 169 of colour attributes, this does not compress well at all, typically saving ~20 bytes. Depending on how much overhead is involved in calling into the decompressor, I may just not bother.

# 11/6/2017 Disco-Era Image Compression for the 6502 with LISP
--------------------------------------------------------------

As you have been following my blog, you will know that I have been building a set of 6502 assembler utilities in LISP with the aim of building an adventure game. Today I am thinking about image compression. (See [here](#assembler) and [here](#gallery))

Typically [LZ77](https://en.wikipedia.org/wiki/LZ77_and_LZ78)  compression involves a stream of symbols, with a special symbol that indicates "hmm, this pattern of symbols looks familiar, I saw it X symbols ago, and it was the same for Y symbols." So it is very easy to decompress. One simply writes out the symbols until the special symbol is encountered, then nip back to the previous occurance -X symbols ago, write out Y symbols, then continue. Of course, for symbols we mean "bytes".

One of the reasons I did not use LZ77 for the text-compression is that we never actually write out the string to memory. There is no way to 'nip-back' as there is nothing to nip back to. We simply render a character's bit pattern to the screen then forget we ever saw it. Decompressing images is slightly different- we actually are writing out a byte pattern to memory, and it will stay there, but with one problem. There are gaps. Gaps where other things live in the screen memory, e.g. the aforementioned text.

We simply can't afford to write a generic decompressor which outputs to some dynamic memory buffer and then copy this data onto the screen taking the gaps into account. As with the text rendering, de-compressing an image has to go directly from the soup to the nuts. We need to solve the problem of the gaps.

~~~~
    |---Text-----------|---Image---| 
    8       16      24      32
    12345678123456781234567812345678  
0                      XXXXXXXXXXXXX                       
40    GAPS             X BONAFIDE  X
80          GAPS       X IMAGE MEM X
120                    XXXXXXXXXXXXX
160                    
200
240
...
~~~~

For this game, images will be roughly 104x104 pixels (though I don't want that to be fixed just yet- additionally there will be a 320x200 image as the start page). So I am proposing to use the following scheme.

a) Search the image data for the least frequent byte, the LFB, to use as the special symbol.

~~~~
   Happy Hack
   ----------
   Since any occurrance of the LSB would be a special case in the de-compressor
   we will simply change any occurrance of the LSB to something else. Something
   that differs by no more than one bit. No special case required. Tell the
   artist to suck it up, if they can drag themselves away from the wacom.
~~~~

b) Encode previously seen patterns as

~~~~
   LFB	  	     
   ROW            - How many rows up do we go? (including 0)
   		    In the decompressor we will have to multiply this by 40
   COLUMN         - How many bytes in do we start?
   WIDTH          - How many bytes to copy?
~~~~
   
   What are the constraints on each field?
   
~~~~
   WIDTH < 40 (6 BITS MAX)
   COLUMN < 40 (6 BITS MAX)
   ROW < 8 BITS MAX
~~~~
   
   So let us pluck some numbers out of the aether.
   
~~~~
   WIDTH 4 bits, giving 1-16 bytes?
   ROW   6 bits, can go back up 6 bits
   COL   6 bits, can spread across the whole width

   TOTAL 16 bits
~~~~

So, each instance of the LFB is 3 bytes, so no point in encoding patterns less than 4 bytes long. So the width nybble can actually encode 4-19 bytes. Pretty good!

There is some inefficiency in the ROW COL addressing scheme, we can address at most 40 columns * 63 rows = 2520 addresses, whereas 12 bits should be able to address 2^12, or 4096 positions.

Hmm, sounds like a lot, especially for an image that is 1352 bytes (+ 169 colour attribute bytes). So in actual fact, we have some headroom. We don't *need* to address 2520 positions. So what can we do? Well, for a start since we *know* (see note) that we are blitting to a 40 column screen (C64 hi-res mode), we can simply precompute the screen offset so we end up with a scheme like this.

~~~~
   LFB
   DELTA
   WIDTH
~~~~

Where DELTA is the number of bytes to go up *in screen memory*. We don't care that there are vast areas of the screen that are invalid addresses- we have to accept this inefficiency in the previous scheme anyway. Now we don't need to do any tricky multiplications, just mask off the width nybble and do a 16 bit subtraction to the zero page to get the address of our previously seen pattern.

Final encoding scheme, 8 bit LFB indicating a pattern, 4 bits of width and 12 bits of delta.

~~~~
   0        1               2      
   76543210 7654   3210     76543210
   LFB      WIDTH  HI-DELTA LO-DELTA
~~~~

How efficient is it in terms of addressing? Well, the efficiency depends on the size of the image. For a full screen image, the delta can address 4096 bytes, pretty much half the screen- every address will be part of the source image. For the 104x104 pixel images, not so much. But we already decided that for a simple row column scheme, we needed 12 bits, so we are no worse off, but we have vastly simplified the calculation. It is tempting to scavenge a bit from the hi-delta and give it to the width. It would mean we could copy an entire row! I may experiment and see if it makes a difference. One thing I am not prepared to do, is go to variable bit width symbols. Too complex both conceptually and computationally for this application- Yaggers.

Note - This is where ad-hoc program assembly comes into its own. Typically one would write a generic function with loads of parameters, e.g. for screen-width on the basis that you should not build in the special cases. Indeed, one would probably be fired if one did not do this (although I have seen a function called SendBatchOf1000Entries in production code. In this instance the culprit was in charge of hiring and firing. The tragedy is that a real programmer would have batched up 1024 entries.)

So we are agreed- this level of genericity is acceptable in our normal humdrum non-assembly-programming lives, but not in 6502. 6502 is *so* constrained that even this level of parameterisation is expensive both in terms of CPU-cycles and brain-cycles. Here we must not violate the classic programmers' maxim "Never do at runtime what ought to be done at compile time." (I think it was Miley Cyrus who said this first). So, I will write a *general* function in LISP which will be parameterised on screen-width, which will generate some *specific* 6502 code and *specific* image data which only works for a screen of width X. This is indeed the best of both worlds.


# 11/6/2017 PM - Image Gallery

Always the danger is that you complete a piece of work and then start playing around with it for hours on end rather than doing the next bit. So here is a gallery of images I converted, for research purposes. Images for the game are going to be 104x104 to start with, maybe there will be some variation if needed. Additionally there will be a title page, so I included an image of an eerie gas station to give an idea of what it will be like.

### Gallery

Eerie gas-station

![Alt text](/blog/gasstation.png)

A creepy door

![Alt text](/blog/stairs.png)

The final image of the Porsche, no dithering, biased YUV conversion

![Alt text](/blog/porsche.png)

Maxine...

![Alt text](/blog/maxine.png)


- Dithering - No.

Decided not to implement it and that using Imagemagick to do it was cheating for the purposes of this exercise. Also, dithering... the word itself is antithetical to the aims of this project.

- YUV

Two chroma channels and luminosity. It should give a better distance metric than RGB but I'll be honest, it's subjective and I messed around by biasing the YUV channels before they ended up in the difference metric. Sometimes RGB absolute distance worked better!

- Grey (or, Gray)

The colour selection is biased towards the greys. I have a feeling that this is due to the fact that the grays in the C64 palette vary in luminosity whereas the other colours are very similar. This means that any image where the colours are closer to grey in luminosity will end up when they are converted as... grey. The thing is, endless tweaking of the biases in the palette selection are not going to make the C64 look like a Silicon Graphics workstation.

I feel it is time to move on to image compression, which I may have mistakenly referred to as LZMA earlier. The algorithm I am actually going to use is LZ77, which I last implemented in 68000 machine code circa 1989. At the time I had no idea it was already a well established (and superseded) technique. It should work well for the simple images here. To improve things prior to the compression

- No dithering

More white and black space, fewer twiddly bits.

- Foreground == Background

Lots of attribute squares get the same foreground as background. This means they have empty data. More zeroes = better compression. I made a change to ensure that no pixel data was written in this case.

- Popcount

For the first time ever in over 30 years of programming I found a use for popcount (i.e. counting the set bits in a byte). When assigning an attribute you can just as easily swap the bits and switch foreground and background colours. The image conversion routine *posterize-image* does precisely this- if there are more set bits than unset bits, it inverts the cell. Hopefully this will aid the compression by reducing the number of different bytes. Perhaps it will do nothing, in which case I will have failed to properly render service to Lord YAGNI once more. (Also, the popcount is done manually, rather than with the HN obsessives x86 instruction)

# 11/6/2017 AM VICKY gets a playmate

First attempt at converting an image for use in the game. Not sure I'm keen on her corpse-like pallour, but her gaze is purposeful as well as purple, and undead romance is extremely popular with today's youth so perhaps it is alright.

![Alt text](/blog/conversion.png)

104 x 104 pixel image with attributes selected by a naive algorithm. Luckily I decided to try sum of absolute difference to select the colours- my first go was with sum of squares and the result was terrible. This has given me the motivation to try YUV which is simple, and CIELAB which is not.

These images should compress nicely for the game as they have a lot of space. To make them even more sparse

- If foreground attribute == background attribute => bitmap is empty for that cell
- If foreground has more pixels than background, invert the cell

## Dithering

![Alt text](/blog/porsche1.png) 
![Alt text](/blog/porsche2.png)

There seems to be a glitch where the bottom of the image hasn't been processed, also, the green of the grass hasn't really come through so I think there is more work to do on the colour-diff function. But I am liking the terrible aesthetic of this. Will all add to the crushing atmosphere when playing it in silence while it rains outside in the middle of the night.

# 10/6/2017 Future, past and present

After the SSD disaster of 8th June, Marceline is back from the dead with a spanky new 1TB drive. Here are some lessons

- Back up your code to one of these hosting repos. Thankfully I only lost a couple of hours worth of code
- Save your config files for emacs etc
- Ubuntu 17 has a nice font compared to Ubuntu 14

The work I did lose was making VICKY support C64 Hi-Res mode colour attributes. 320x200 resolution, split into 8x8 squares, each of which can have two colours from a palette of 16. Colours are stored in the 1000 byte character memory (aka screen memory). The lo-nybble contains the background colour, the hi-nybble the foreground colour.

Mr Timmerman has a nice article on C64 colours, so I used the RGB values he came up with here http://unusedino.de/ec64/technical/misc/vic656x/colors/

Two more screenshots, demonstrating justification for different fonts and the attribute changes.

![Alt text](/blog/yeolde.png)
![Alt text](/blog/future.png)

## Images

Next up, images. I can't decide whether I will draw them using a cross platform C64 image editor or to draw them and convert them. Programming a converter might be nice, but I don't want to get bogged down. What I will certainly be working on is a sliding window compressor.

## Other changes

- Shared memory now maps the entire 64K

I didn't want to map the character memory and bitmap memory separately. I will be able to attach a monitor at some point to a running game session.

- Shared memory now uses /dev/shm

The proper way to do this of course is with shm_open() rather than mmap'ing to a file in /dev/shm but I couldn't find this function in sb-posix. I wonder if having an mmap'd file updating to disk every second wasn't the reason my SSD carked it...

# 7/6/2017 Functional design, imperative code

Or, functional assembler.

Assembler is clearly an imperative paradigm, the Von Neumann architecture being pretty close to a real-world Turing machine. Purity (in the functional progamming sense) is hardly a consideration. Consider this design for the text rendering pipeline,

~~~~                           
                              +--------------------+
                              |                    |
                        screen state ->            |
compressed string data -> decompress -> combine ---+
                        bit patterns -> 

~~~~

Forgetting monads, a typical practical functional design is to minimise state except where necessary, this diagram is pretty close to that, functional in the middle with state at either end. Very simple to reason about and easy to test. We might implement it in a C like language by having something like,

~~~~

	draw(decompress(str), font, screen-buffer)

~~~~

Which can't make great claims to being functional and pure (but we are ignoring monads), it seems like typical, practical strategy. But, it does a lot of work that we don't need to do- for example, consider the decompress function. It probably returns a string. Haskell would probably be able to optimise this away at runtime by not computing anything until necessary (I doubt it though, in the general case).

A-ha, you say, you would not return a string, you would invert control and call out to a function-

~~~~

	(decompress(str, draw, font), screen-buffer)

~~~~

Much better, now you can decompress and draw without a temporary 2TB string on the stack. But it does seem to have introduced a coupling between decompress and draw, namely that the font has to be passed in. In C you would probably live with this. In a C#, you would surely introduce a closure,

~~~~

	(decompress(str, (c)=>draw(c,font), screen-buffer))

~~~~

Now it is decoupled and efficient. But how is a closure implemented? Well it is just some state stuffed somewhere, and this strategy is exactly what I have done with the 6502 implementation. So if you squint hard enough, it is functional, it merely has the appearance of mutating state left, right and centre. Moreover, we are forced to think this way by the extreme constraints imposed by the lack of memory and cycles in a 40 year old processor.

# 6/6/2017 Justified ancient text

![Alt text](/blog/muse.png)

The justification is pre-computed, following the principle of never doing at run-time what can be done at assembly time. Of course, this is often a space-time trade-off, but in this case it isn't. I did violate one rule, DRY, that is, the width calculation is done in LISP as well as in 6502. The text measurement could have been done by running the actual code, but then it is easy to take principles too far.

# 5/6/2017 Kerning has been achieved

Added a very basic kerning algorithm whereby a character can decide if it admits a character a pixel closer to the right, and a character can decide if it is such a character that can be admitted a pixel closer, to the left.

![Alt text](/blog/kerning.png)

The extra code to decide whether to kern or not. The widths of all characters were reduced by one in the tables- the extra pixel is added back in when (in the vast majority of cases) the two adjacent characters do not kern together.

~~~~

    (ASL "Now that flag is in bit 7")
    (dc "Bit 7 set iff prev char admits to right and current char admits to left")    
    (AND.IZX :char)
    (EOR #x80)
    (ASL)
    (dc "Now the carry is set iff there is no kerning between the two characters")
    (LDA.ZP :prev-width)
    (AND.IMM #xf)
    (ADC.ZP :shift)

~~~~

The more complex scheme where there are two classes of adjacency (over/under and sparseness) for kerning has not been implememnted, this is why the spacing of "hi" could be improved in the first typeface. I am pretty happy with the typesetting now so I will move on to text flow and then the exciting business of images, which will be compressed with a sliding-window algorithm, or a variant on the fixed width symbol. Since we are outputting the data into screen memory, sliding-window now becomes a possibility where it was not with the text- it is important to note that nowhere do we actually write out the strings to a memory in anything as vulgar as ASCII. That is for C programmers.

# 4/6/2017 Rendering test

So rendering from compressed strings in a variety of fonts is almost there. I wanted a certain feel to the game and variable width fonts were an absolute must. Since no-one is reading this blog, this screen shot will not give anything of the game away. The deadline for the IF competition is September, so that's a certain motivation, but it also means I won't be checking in the actual game data here until after.

![Alt text](/blog/three-fonts.png)

~~~~
           (sta16.zp :str1 :A2)
	   (sta16.zp '(:font . :present) :font)
	   (sta16.zp #x80F0 '(:typeset . :raster))

	   (JSR :typeset-cs)

	   ...
	   
	   (dcs :str1 "Chad Jenkins and his red Porsche.")
~~~~

Note the use of the faked 16 bit mnemonic.

## Kerning

I have an idea for a simple kerning algorithm. Each letter currently has its width measured by the assembler and leaves exactly one pixel between it and the next. In the length byte of the character data there is a nybble free.

- Admits an underloper to the right (e.g. f)
- Admits anyone to the right (e.g. the i in the gothic font)
- Admits anyone to the left (e.g. the t in the gothic font)
- Is an underloper (e.g. e, but not T)

The combination of these bits with the next characters kerning bits can then be used to generate a more pleasing spacing. For example, in the gothic font, f can overhang e by two pixels. These kerning flags will probably be set manually, but it may be possible to run a script to get a good first pass.

One thing I have learned so far in building and assembler for 6502 is that of the two important problems in computer science, cache invalidation and naming things, off-by-one errors are by far the most pernicious.

## Cache invalidation

Since the assembler is multi-pass, the operations have to be idempotent. Since this is a faff, there is now a flag *compiler-final-pass* which replaces the *compiler-ensure-labels-resolve*. This means that we can output invalid bytes, use dodgy addresses etc, until the final pass. Additionally, certain things will be no-ops, like adding to the list of strings present in the build- this only takes effect in the final pass to avoid non-idempotency issues.

## Naming things

Naming things is still a pain. I haven't found a nice way of naming 'parameters' to 'functions'. Zero page scratch is aliased inside the functions, but not outside. This will have to be solved as it is too easy to lose track of generically named zpg labels.

The other naming problem is the use of conses for namespaced labels, e.g '(:render . :some-label). This is pretty boring, but it seems to work so I am still sticking with it. The longer I leave it before declaring a function or reader macro, the harder it will be to change.

## OBOEs

Clear the carry, set the carry. Egads.

# 2/6/2017 Rendering an upside down G

## Flag and register value sneakery

6502 has no BRA. Sometimes we need a BRA, so

~~~~
(LDY 10 "10 pixel character height")
(BNE :go)
~~~~

Sneaky. 6502.org details a lot more of these kind of shenanighans.

For the font renderer, I have used Y as a counter to index into the character data. Counting down to zero is best, there's no comparison. Here I use the (supposedly rarely used) X indexed indirect zero page addressing mode without having to reach for the far more sensible Y indirect indexed mode, which would necessitate preserving the state of Y somewhere horrible like the stack.

~~~~
(DEX)
(BNE :shift-right)
(ORA.IZX :raster "Bam, free indirection into raster, since X is 0")
~~~~

Which leads me to the upside-downness of the G. Since we count down, we index from the top of the data. Boo. Rather than count up and do a wasteful and aesthetically displeasing CMP, we simply reverse the generated font data. Never do at run-time what can be done at assembly time. Especially when your runtime is 1Mhz and you don't even have an instruction to increment the accumulator.

## Label aliasing

Since the 6502 has basically no registers we use the zeropage in its place. So it is nice to define bytes and words for specific important uses (e.g. important global state), but it is also nice to use it for 'local variables' which may change from function to function. All this acheives is a nicer disassembly.

~~~~
(zp-w :something-important)
(zp-w :A0) ;scratch data
(zp-w :A1)
(zp-w :A2)

...

(label :a-function)
(alias :multiplier :A0)		;use A0 for our multiplier
(LDA :multiplier)
~~~~

## First conceptual problem

The first hard problem has arisen. Consider the following snippet,

~~~~
(defun inc16.zp (label)
  "Increment a zero-page word"
  (INC.ZP (lo-add label))
  (BNE :end)
  (INC.ZP (hi-add label))
  (label :end))
~~~~

To the human eye, this looks reasonable, but since labels are placed into a global table, if you call this function twice, one instance will not know which end to call. Wrapping in a namespace doesn't help either. Now the problem is one of identifying the instance of the label from one pass to another, which is really the second problem in computer science. There's probably a way to resolve the labels using a fancy algorithm. Until I figure out what that is and if it is worth it, the first solution is,

~~~~
(defun inc16.zp (label)
  "Increment a zero-page word"
  (INC.ZP (lo-add label))
  (BNE 2)
  (INC.ZP (hi-add label)))
~~~~

Ultimate Yaggers- now I can continue with the font renderer.

# 31/5/2017 8 bit multiplication by 10

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

# 30/5/2017 Label namespaces, VICKY and YAGNI

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

## VICKY

VICKY (I was going to go for V.I.C.K.Y. Perhaps there's a movie in there somewhere about a regular schoolgirl who is actually a robot) is a view of an mmap'ed section of memory, rendered to the screen in the washed out palette of the VIC-II fromm the C64. This is giving the 6502 code something to target. For now, it will just render the hi-res bitmap mode, 320x200 which I plan on using for the adventure game.

Look, here is a test rendering I dumped in using a 'medieval' style font. Sadly I think that at 12 pixels high, it will be too big, there being room for only 15 lines (the original, 8-bit break on which it was based is even bigger). Note the horrific horizontal spacing- variable width rendering of fonts is next on the agenda. For some of you, the brown and yellow colour scheme will bring back memories redolent of languorous summers spent indoors waiting for tapes to load. I had a Spectrum, so I feel nothing. Perhaps resentment, not greatly dulled by time.

![Alt text](/blog/capture.png)

UPDATE - As you can see three additional fonts have been added, 10 pixels high. There are some flaws. But, meh, fix it when it needs fixing. It will be okay at the sanding stage.

# 27/5/2017 Roundtrip encoding strings

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

# 19/5/2017 Or there about

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
01FA E906 0A06 0000 0000 0000 0000 0000 0000 ................
-- Watches --------------------------------------------------
060C 5468 6973 2069 7320 7468 6520 7465 7374 This is the test
061C 2073 7472 6920 2020 2020 2020 2020 2020  stri           
-- PC -------------------------------------------------------
06B5 A400 990C 06E6 0060 E602 D004 E603 F04B .......`.......K
06C5 A000 B102 F045 C957 901E 8501 AABD BA06 .....E.W........
-------------------------------------------------------------
                          SV BDIZC
       PC:06B5 SP:1F9  SR:00100101 A:6E X:0F Y:14
-------------------------------------------------------------
EMIT      06B5 A400    LDY $00
          06B7 990C06  STA $060C,Y           ;STR-BUFFER
          06BA E600    INC $00
          06BC 60      RTS
RSTR-NXT  06BD E602    INC $02

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

Metaprogramming is a lot of fun. We can change the strings and new tables will be generated automatically. Here the string decoder and the emit functions have been separated in the 'compiler' but are together in the output. Why don't we do this for modern programs? Ah yes, that's right, we have OO, JIT and SOLID design principles so it is entirely acceptable to have a monolithic compiler.

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






