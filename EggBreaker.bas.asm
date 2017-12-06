 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif
; This is a 2-line kernel!
kernel
 sta WSYNC
 lda #255
 sta TIM64T

 lda #1
 sta VDELBL
 sta VDELP0
 ldx ballheight
 inx
 inx
 stx temp4
 lda player1y
 sta temp3

 ifconst shakescreen
   jsr doshakescreen
 else
   ldx missile0height
   inx
 endif

 inx
 stx stack1

 lda bally
 sta stack2

 lda player0y
 ldx #0
 sta WSYNC
 stx GRP0
 stx GRP1
 stx PF1L
 stx PF2
 stx CXCLR
 ifconst readpaddle
   stx paddle
 else
   sleep 3
 endif

 sta temp2,x

 ;store these so they can be retrieved later
 ifnconst pfres
   ldx #128-44+(4-pfwidth)*12
 else
   ldx #132-pfres*pfwidth
 endif

 dec player0y

 lda missile0y
 sta temp5
 lda missile1y
 sta temp6

 lda playfieldpos
 sta temp1
 
 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif
 clc
 sbc playfieldpos
 sta playfieldpos
 jmp .startkernel

.skipDrawP0
 lda #0
 tay
 jmp .continueP0

.skipDrawP1
 lda #0
 tay
 jmp .continueP1

.kerloop ; enter at cycle 59??

continuekernel
 sleep 2
continuekernel2
 lda ballheight
 
 ifconst pfres
 ldy playfield+pfres*pfwidth-132,x
 sty PF1L ;3
 ldy playfield+pfres*pfwidth-131-pfadjust,x
 sty PF2L ;3
 ldy playfield+pfres*pfwidth-129,x
 sty PF1R ; 3 too early?
 ldy playfield+pfres*pfwidth-130-pfadjust,x
 sty PF2R ;3
 else
 ldy playfield-48+pfwidth*12+44-128,x
 sty PF1L ;3
 ldy playfield-48+pfwidth*12+45-128-pfadjust,x ;4
 sty PF2L ;3
 ldy playfield-48+pfwidth*12+47-128,x ;4
 sty PF1R ; 3 too early?
 ldy playfield-48+pfwidth*12+46-128-pfadjust,x;4
 sty PF2R ;3
 endif

 ; should be playfield+$38 for width=2

 dcp bally
 rol
 rol
; rol
; rol
goback
 sta ENABL 
.startkernel
 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawP1 ;2
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continueP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
   rol;2
   rol;2
   sta ENAM1 ;3
 else
   lda (player1color),y
   sta COLUP1
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif

 ifconst pfres
 lda playfield+pfres*pfwidth-132,x 
 sta PF1L ;3
 lda playfield+pfres*pfwidth-131-pfadjust,x 
 sta PF2L ;3
 lda playfield+pfres*pfwidth-129,x 
 sta PF1R ; 3 too early?
 lda playfield+pfres*pfwidth-130-pfadjust,x 
 sta PF2R ;3
 else
 lda playfield-48+pfwidth*12+44-128,x ;4
 sta PF1L ;3
 lda playfield-48+pfwidth*12+45-128-pfadjust,x ;4
 sta PF2L ;3
 lda playfield-48+pfwidth*12+47-128,x ;4
 sta PF1R ; 3 too early?
 lda playfield-48+pfwidth*12+46-128-pfadjust,x;4
 sta PF2R ;3
 endif 
; sleep 3

 lda player0height
 dcp player0y
 bcc .skipDrawP0
 ldy player0y
 lda (player0pointer),y
.continueP0
 sta GRP0

 ifnconst no_blank_lines
 ifnconst playercolors
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
 else
   lda (player0color),y
   sta player0colorstore
   sleep 6
 endif
   dec temp1
   bne continuekernel
 else
   dec temp1
   beq altkernel2
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle
   inc paddle
   jmp continuekernel2
noreadpaddle
   sleep 2
   jmp continuekernel
 else
 ifnconst playercolors 
 ifconst PFcolors
   txa
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
   jmp continuekernel
 else
 ifconst kernelmacrodef
   kernelmacro
 else
   sleep 12
 endif
 endif
 else
   lda (player0color),y
   sta player0colorstore
   sleep 4
 endif
   jmp continuekernel
 endif
altkernel2
   txa
   sbx #256-pfwidth
   bmi lastkernelline
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
   jmp continuekernel
 endif

altkernel

 ifconst PFmaskvalue
   lda #PFmaskvalue
 else
   lda #0
 endif
 sta PF1L
 sta PF2


 ;sleep 3

 ;28 cycles to fix things
 ;minus 11=17

; lax temp4
; clc
 txa
 sbx #256-pfwidth

 bmi lastkernelline

 ifconst PFcolorandheight
   ifconst pfres
     ldy playfieldcolorandheight-131+pfres*pfwidth,x
   else
     ldy playfieldcolorandheight-87,x
   endif
 ifnconst backgroundchange
   sty COLUPF
 else
   sty COLUBK
 endif
   ifconst pfres
     lda playfieldcolorandheight-132+pfres*pfwidth,x
   else
     lda playfieldcolorandheight-88,x
   endif
   sta.w temp1
 endif
 ifconst PFheights
   lsr
   lsr
   tay
   lda (pfheighttable),y
   sta.w temp1
 endif
 ifconst PFcolors
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 ifnconst PFcolorandheight
 ifnconst PFcolors
 ifnconst PFheights
 ifnconst no_blank_lines
 ; read paddle 0
 ; lo-res paddle read
  ; bit INPT0
  ; bmi paddleskipread
  ; inc paddle0
;donepaddleskip
   sleep 10
 ifconst pfrowheight
   lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 endif
 endif
 endif
 

 lda ballheight
 dcp bally
 sbc temp4


 jmp goback


 ifnconst no_blank_lines
lastkernelline
 ifnconst PFcolors
   sleep 10
 else
   ldy #124
   lda (pfcolortable),y
   sta COLUPF
 endif

 ifconst PFheights
 ldx #1
 sleep 4
 else
 ldx playfieldpos
 sleep 3
 endif

 jmp enterlastkernel

 else
lastkernelline
 
 ifconst PFheights
 ldx #1
 sleep 5
 else
   ldx playfieldpos
 sleep 4
 endif

   cpx #0
   bne .enterfromNBL
   jmp no_blank_lines_bailout
 endif

 if ((<*)>$d5)
 align 256
 endif
 ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
 sleep 2
 lda #0
 jmp .continuelastP1

.endkerloop ; enter at cycle 59??
 
 nop

.enterfromNBL
 ifconst pfres
 ldy.w playfield+pfres*pfwidth-4
 sty PF1L ;3
 ldy.w playfield+pfres*pfwidth-3-pfadjust
 sty PF2L ;3
 ldy.w playfield+pfres*pfwidth-1
 sty PF1R ; possibly too early?
 ldy.w playfield+pfres*pfwidth-2-pfadjust
 sty PF2R ;3
 else
 ldy.w playfield-48+pfwidth*12+44
 sty PF1L ;3
 ldy.w playfield-48+pfwidth*12+45-pfadjust
 sty PF2L ;3
 ldy.w playfield-48+pfwidth*12+47
 sty PF1R ; possibly too early?
 ldy.w playfield-48+pfwidth*12+46-pfadjust
 sty PF2R ;3
 endif

enterlastkernel
 lda ballheight

; tya
 dcp bally
; sleep 4

; sbc stack3
 rol
 rol
 sta ENABL 

 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawlastP1
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continuelastP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
 else
   lda (player1color),y
   sta COLUP1
 endif

 dex
 ;dec temp4 ; might try putting this above PF writes
 beq endkernel


 ifconst pfres
 ldy.w playfield+pfres*pfwidth-4
 sty PF1L ;3
 ldy.w playfield+pfres*pfwidth-3-pfadjust
 sty PF2L ;3
 ldy.w playfield+pfres*pfwidth-1
 sty PF1R ; possibly too early?
 ldy.w playfield+pfres*pfwidth-2-pfadjust
 sty PF2R ;3
 else
 ldy.w playfield-48+pfwidth*12+44
 sty PF1L ;3
 ldy.w playfield-48+pfwidth*12+45-pfadjust
 sty PF2L ;3
 ldy.w playfield-48+pfwidth*12+47
 sty PF1R ; possibly too early?
 ldy.w playfield-48+pfwidth*12+46-pfadjust
 sty PF2R ;3
 endif

 ifnconst player1colors
   rol;2
   rol;2
   sta ENAM1 ;3
 else
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif
 
 lda.w player0height
 dcp player0y
 bcc .skipDrawlastP0
 ldy player0y
 lda (player0pointer),y
.continuelastP0
 sta GRP0



 ifnconst no_blank_lines
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
   jmp .endkerloop
 else
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle2
   inc paddle
   jmp .endkerloop
noreadpaddle2
   sleep 4
   jmp .endkerloop
 else ; no_blank_lines and no paddle reading
 pla
 pha ; 14 cycles in 4 bytes
 pla
 pha
 ; sleep 14
 jmp .endkerloop
 endif
 endif


;  ifconst donepaddleskip
;paddleskipread
 ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
 ; plus we get a lo-res paddle read
; bmi donepaddleskip
;  endif

.skipDrawlastP0
 sleep 2
 lda #0
 jmp .continuelastP0

 ifconst no_blank_lines
no_blank_lines_bailout
 ldx #0
 endif

endkernel
 ; 6 digit score routine
 stx PF1
 stx PF2
 stx PF0
 clc

 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 sbc playfieldpos
 sta playfieldpos
 txa

 ifconst shakescreen
   bit shakescreen
   bmi noshakescreen2
   ldx #$3D
noshakescreen2
 endif

   sta WSYNC,x

;                STA WSYNC ;first one, need one more
 sta REFP0
 sta REFP1
                STA GRP0
                STA GRP1
 ;               STA PF1
   ;             STA PF2
 sta HMCLR
 sta ENAM0
 sta ENAM1
 sta ENABL

 lda temp2 ;restore variables that were obliterated by kernel
 sta player0y
 lda temp3
 sta player1y
 ifnconst player1colors
   lda temp6
   sta missile1y
 endif
 ifnconst playercolors
 ifnconst readpaddle
   lda temp5
   sta missile0y
 endif
 endif
 lda stack2
 sta bally

 ifconst no_blank_lines
 sta WSYNC
 endif

 lda INTIM
 clc
 ifnconst vblank_time
 adc #43+12+87
 else
 adc #vblank_time+12+87
 endif
; sta WSYNC
 sta TIM64T

 ifconst minikernel
 jsr minikernel
 endif

 ; now reassign temp vars for score pointers

; score pointers contain:
; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
; swap lo2->temp1
; swap lo4->temp3
; swap lo6->temp5
 ifnconst noscore
 lda scorepointers+1
; ldy temp1
 sta temp1
; sty scorepointers+1

 lda scorepointers+3
; ldy temp3
 sta temp3
; sty scorepointers+3


 sta HMCLR
 tsx
 stx stack1 
 ldx #$E0
 stx HMP0

               LDA scorecolor 
                STA COLUP0
                STA COLUP1
 ifconst pfscore
 lda pfscorecolor
 sta COLUPF
 endif
 sta WSYNC
 ldx #0
                STx GRP0
                STx GRP1 ; seems to be needed because of vdel

 lda scorepointers+5
; ldy temp5
 sta temp5,x
; sty scorepointers+5
 lda #>scoretable
 sta scorepointers+1
 sta scorepointers+3
 sta scorepointers+5
 sta temp2
 sta temp4
 sta temp6
                LDY #7
        STY VDELP0
                STA RESP0
                STA RESP1


        LDA #$03
        STA NUSIZ0
        STA NUSIZ1
        STA VDELP1
        LDA #$F0
        STA HMP1
 lda  (scorepointers),y
 sta  GRP0
                STA HMOVE ; cycle 73 ?
 jmp beginscore


 if ((<*)>$d4)
 align 256 ; kludge that potentially wastes space!  should be fixed!
 endif

loop2
 lda  (scorepointers),y     ;+5  68  204
 sta  GRP0            ;+3  71  213      D1     --      --     --
 ifconst pfscore
 lda.w pfscore1
 sta PF1
 else
 sleep 7
 endif
 ; cycle 0
beginscore
 lda  (scorepointers+$8),y  ;+5   5   15
 sta  GRP1            ;+3   8   24      D1     D1      D2     --
 lda  (scorepointers+$6),y  ;+5  13   39
 sta  GRP0            ;+3  16   48      D3     D1      D2     D2
 lax  (scorepointers+$2),y  ;+5  29   87
 txs
 lax  (scorepointers+$4),y  ;+5  36  108
 sleep 3

 ifconst pfscore
 lda pfscore2
 sta PF1
 else
 sleep 6
 endif

 lda  (scorepointers+$A),y  ;+5  21   63
 stx  GRP1            ;+3  44  132      D3     D3      D4     D2!
 tsx
 stx  GRP0            ;+3  47  141      D5     D3!     D4     D4
 sta  GRP1            ;+3  50  150      D5     D5      D6     D4!
 sty  GRP0            ;+3  53  159      D4*    D5!     D6     D6
 dey
 bpl  loop2           ;+2  60  180

 ldx stack1 
 txs
; lda scorepointers+1
 ldy temp1
; sta temp1
 sty scorepointers+1

                LDA #0   
 sta PF1
               STA GRP0
                STA GRP1
        STA VDELP0
        STA VDELP1;do we need these
        STA NUSIZ0
        STA NUSIZ1

; lda scorepointers+3
 ldy temp3
; sta temp3
 sty scorepointers+3

; lda scorepointers+5
 ldy temp5
; sta temp5
 sty scorepointers+5
 endif ;noscore
 LDA #%11000010
 sta WSYNC
 STA VBLANK
 RETURN

 ifconst shakescreen
doshakescreen
   bit shakescreen
   bmi noshakescreen
   sta WSYNC
noshakescreen
   ldx missile0height
   inx
   rts
 endif

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifconst pfrowheight
 lda pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*pfwidth-1
 else
 ldx #47-(4-pfwidth)*12 ; will this work?
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 if pfwidth=4
  asl ; multiply y pos by 4
 endif ; else multiply by 2
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 if pfwidth=4
   asl ; multiply by 4
 endif ; else multiply by 2
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 if pfwidth=4
   iny
   iny
 endif
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 ifnconst pfcenter
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 endif
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
 ifconst debugscore
   ldx #14
   lda INTIM ; display # cycles left in the score

 ifconst mincycles
 lda mincycles 
 cmp INTIM
 lda mincycles
 bcc nochange
 lda INTIM
 sta mincycles
nochange
 endif

;   cmp #$2B
;   bcs no_cycles_left
   bmi cycles_left
   ldx #64
   eor #$ff ;make negative
cycles_left
   stx scorecolor
   and #$7f ; clear sign bit
   tax
   lda scorebcd,x
   sta score+2
   lda scorebcd1,x
   sta score+1
   jmp done_debugscore   
scorebcd
 .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
 .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
 .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
 .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
 .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
 .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
 .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
 .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
 endif

 ifconst debugcycles
   lda INTIM ; if we go over, it mucks up the background color
;   cmp #$2B
;   BCC overscan
   bmi overscan
   sta COLUBK
   bcs doneoverscan
 endif

 
overscan
 lda INTIM ;wait for sync
 bmi overscan
doneoverscan
;do VSYNC
 lda #2
 sta WSYNC
 sta VSYNC
 STA WSYNC
 STA WSYNC
 LDA #0
 STA WSYNC
 STA VSYNC
 sta VBLANK
 ifnconst overscan_time
 lda #37+128
 else
 lda #overscan_time+128
 endif
 sta TIM64T

 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop
 lda player0x,x
 sec
 sbc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop
 endif
 endif
 if (<*)>$F0
 ;align 256, $ea
 ; the above puts in zeros. Why? replaced by repeat below:
 repeat ($100-(<*))
 nop
 repend
 endif
  sta WSYNC
  ldx #4
  SLEEP 3
HorPosLoop       ;     5
  lda player0x,X  ;+4   9
  sec           ;+2  11
DivideLoop
  sbc #15
  bcs DivideLoop;+4  15
  sta temp1,X    ;+4  19
  sta RESP0,X   ;+4  23
  sta WSYNC
  dex
  bpl HorPosLoop;+5   5
                ;     4

  ldx #4
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 18

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 32

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 46

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 60

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 74

  sta WSYNC
 
  sta HMOVE     ;+3   3


 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop2
 lda player0x,x
 clc
 adc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop2
 endif
 endif




;set score pointers
 lax score+2
 jsr scorepointerset
 sty scorepointers+5
 stx scorepointers+2
 lax score+1
 jsr scorepointerset
 sty scorepointers+4
 stx scorepointers+1
 lax score
 jsr scorepointerset
 sty scorepointers+3
 stx scorepointers

vblk
; run possible vblank bB code
 ifconst vblank_bB_code
   jsr vblank_bB_code
 endif
vblk2
 LDA INTIM
 bmi vblk2
 jmp kernel
 

    .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
    .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
 and #$0F
 asl
 asl
 asl
 adc #<scoretable
 tay 
 txa
 arr #$F0
 tax
 sbx #<(256-<scoretable)
 rts
; y and a contain multiplicands, result in a

mul8
 sty temp1
 sta temp2
 lda #0
reptmul8
 lsr temp2
 bcc skipmul8
 clc
 adc temp1
;bcs donemul8 might save cycles?
skipmul8
;beq donemul8 might save cycles?
 asl temp1
 bne reptmul8
donemul8
 RETURN

div8
 ; a=numerator y=denominator, result in a
 cpy #2
 bcc div8end+1;div by 0 = bad, div by 1=no calc needed, so bail out
 sty temp1
 ldy #$ff
div8loop
 sbc temp1
 iny
 bcs div8loop
div8end
 tya
 ; result in a
 RETURN
game
.L00 ;  rem **********************************

.L01 ;  rem Egg Breaker

.L02 ;  rem 

.L03 ;  rem Humpty Dumpty has a neighbor he doesn't like 

.L04 ;  rem There's a wall between them like fences for human 

.L05 ;  rem  neighbors. Get your ball past them to win a point

.L06 ;  rem  and have your ball hit the other egg to win 10 points

.L07 ;  rem 

.L08 ;  rem LMC 2700

.L09 ;  rem Project 6

.L010 ;  rem Lilliann Andrews, Sranee Bayapureddy, David Le

.L011 ;  rem **********************************

.L012 ;  include div_mul.asm

.L013 ;  rem include fixed_point_math.asm

.L014 ;  set legacy

.L015 ;  set romsize 4k

.L016 ;  pfclear

	LDA #0
 jsr pfclear
.L017 ;  player0x = 2 : player0y = 52

	LDA #2
	STA player0x
	LDA #52
	STA player0y
.L018 ;  player0:

	LDA #<playerL018_0

	STA player0pointerlo
	LDA #>playerL018_0

	STA player0pointerhi
	LDA #9
	STA player0height
.L019 ;  player1x = 158 : player1y = 52

	LDA #158
	STA player1x
	LDA #52
	STA player1y
.L020 ;  player1:

	LDA #<playerL020_1

	STA player1pointerlo
	LDA #>playerL020_1

	STA player1pointerhi
	LDA #9
	STA player1height
.
 ; 

.L021 ;  scorecolor  =  60

	LDA #60
	STA scorecolor
.L022 ;  dim missile0dx  =  a

.L023 ;  dim missile0dy  =  b

.L024 ;  dim missile1dx  =  c

.L025 ;  dim missile1dy  =  d

.L026 ;  dim tempx  =  e

.L027 ;  dim tempy  =  f

.L028 ;  dim musicPointer  =  g

.L029 ;  dim musicTimer  =  h

.L030 ;  dim tempaudv  =  i

.L031 ;  dim musicDist  =  j

.L032 ;  dim soundTimer  =  k

.L033 ;  dim randCountDown  =  l

.L034 ;  randCountDown  =  30

	LDA #30
	STA randCountDown
.L035 ;  musicPointer = $FF

	LDA #$FF
	STA musicPointer
.L036 ;  musicTimer = 0

	LDA #0
	STA musicTimer
.L037 ;  AUDV0 = 0

	LDA #0
	STA AUDV0
.L038 ;  AUDC0 = 4

	LDA #4
	STA AUDC0
.L039 ;  AUDV1 = 0

	LDA #0
	STA AUDV1
.L040 ;  AUDC1 = 14

	LDA #14
	STA AUDC1
.
 ; 

.startgame
 ; startgame

.L041 ;  player0x = 2 : player0y = 52

	LDA #2
	STA player0x
	LDA #52
	STA player0y
.L042 ;  player1x = 158 : player1y = 52

	LDA #158
	STA player1x
	LDA #52
	STA player1y
.L043 ;  missile0x = 40

	LDA #40
	STA missile0x
.L044 ;  missile0y = 44

	LDA #44
	STA missile0y
.L045 ;  missile0dx  =  0

	LDA #0
	STA missile0dx
.L046 ;  missile0dy  =  0

	LDA #0
	STA missile0dy
.L047 ;  missile1x = 150

	LDA #150
	STA missile1x
.L048 ;  missile1y = 44

	LDA #44
	STA missile1y
.L049 ;  missile1dx  =  0

	LDA #0
	STA missile1dx
.L050 ;  missile1dy  =  0

	LDA #0
	STA missile1dy
.
 ; 

.drawborders
 ; drawborders

.L051 ;  pfvline 14 0 11 on

	LDA #11
	STA temp3
	LDA #14
	LDY #0
	LDX #0
 jsr pfvline
.L052 ;  pfvline 15 0 11 on

	LDA #11
	STA temp3
	LDA #15
	LDY #0
	LDX #0
 jsr pfvline
.L053 ;  pfvline 16 0 11 on

	LDA #11
	STA temp3
	LDA #16
	LDY #0
	LDX #0
 jsr pfvline
.
 ; 

.
 ; 

.gameloop
 ; gameloop

.L054 ;  COLUP0  =  14

	LDA #14
	STA COLUP0
.L055 ;  COLUP1  =  14

	LDA #14
	STA COLUP1
.L056 ;  COLUPF  =  64

	LDA #64
	STA COLUPF
.L057 ;  COLUBK  =  70

	LDA #70
	STA COLUBK
.L058 ;  drawscreen

 jsr drawscreen
.L059 ;  if joy0fire  &&  missile0dx  =  0 then gosub startball0

 bit INPT4
	BMI .skipL059
.condpart0
	LDA missile0dx
	CMP #0
     BNE .skip0then
.condpart1
 jsr .startball0

.skip0then
.skipL059
.L060 ;  if joy0up then player0y  =  player0y  -  2 :  if player0y  <  16 then player0y  =  16

 lda #$10
 bit SWCHA
	BNE .skipL060
.condpart2
	LDA player0y
	SEC
	SBC #2
	STA player0y
	LDA player0y
	CMP #16
     BCS .skip2then
.condpart3
	LDA #16
	STA player0y
.skip2then
.skipL060
.L061 ;  if joy0down then player0y  =  player0y  +  2 :  if player0y  >  88 then player0y  =  88

 lda #$20
 bit SWCHA
	BNE .skipL061
.condpart4
	LDA player0y
	CLC
	ADC #2
	STA player0y
	LDA #88
	CMP player0y
     BCS .skip4then
.condpart5
	LDA #88
	STA player0y
.skip4then
.skipL061
.L062 ;  if joy1fire  &&  missile1dx  =  0 then gosub startball1

 bit INPT5
	BMI .skipL062
.condpart6
	LDA missile1dx
	CMP #0
     BNE .skip6then
.condpart7
 jsr .startball1

.skip6then
.skipL062
.L063 ;  if joy1up then player1y  =  player1y  -  2 :  if player1y  <  16 then player1y  =  16

 lda #1
 bit SWCHA
	BNE .skipL063
.condpart8
	LDA player1y
	SEC
	SBC #2
	STA player1y
	LDA player1y
	CMP #16
     BCS .skip8then
.condpart9
	LDA #16
	STA player1y
.skip8then
.skipL063
.L064 ;  if joy1down then player1y  =  player1y  +  2 :  if player1y  >  88 then player1y  =  88

 lda #2
 bit SWCHA
	BNE .skipL064
.condpart10
	LDA player1y
	CLC
	ADC #2
	STA player1y
	LDA #88
	CMP player1y
     BCS .skip10then
.condpart11
	LDA #88
	STA player1y
.skip10then
.skipL064
.L065 ;  missile0x  =  missile0x  +  missile0dx

	LDA missile0x
	CLC
	ADC missile0dx
	STA missile0x
.L066 ;  missile1x  =  missile1x  +  missile1dx

	LDA missile1x
	CLC
	ADC missile1dx
	STA missile1x
.L067 ;  missile0y  =  missile0y  +  missile0dy

	LDA missile0y
	CLC
	ADC missile0dy
	STA missile0y
.L068 ;  missile1y  =  missile1y  +  missile1dy

	LDA missile1y
	CLC
	ADC missile1dy
	STA missile1y
.L069 ;  rem PADDLE COLLISIONS

.L070 ;  if collision(player1,missile0) then soundTimer  =  16

	BIT CXM0P
	BPL .skipL070
.condpart12
	LDA #16
	STA soundTimer
.skipL070
.L071 ;  if collision(player1,missile0) then gosub collidep1b0

	BIT CXM0P
	BPL .skipL071
.condpart13
 jsr .collidep1b0

.skipL071
.L072 ;  if collision(player0,missile0) then soundTimer  =  16

	BIT CXM0P
	BVC .skipL072
.condpart14
	LDA #16
	STA soundTimer
.skipL072
.L073 ;  if collision(player0,missile0) then gosub collidep0b0

	BIT CXM0P
	BVC .skipL073
.condpart15
 jsr .collidep0b0

.skipL073
.L074 ;  if collision(player1,missile1) then soundTimer  =  16

	BIT CXM1P
	BVC .skipL074
.condpart16
	LDA #16
	STA soundTimer
.skipL074
.L075 ;  if collision(player1,missile1) then gosub collidep1b1

	BIT CXM1P
	BVC .skipL075
.condpart17
 jsr .collidep1b1

.skipL075
.L076 ;  if collision(player0,missile1) then soundTimer  =  16

	BIT CXM1P
	BPL .skipL076
.condpart18
	LDA #16
	STA soundTimer
.skipL076
.L077 ;  if collision(player0,missile1) then gosub collidep0b1

	BIT CXM1P
	BPL .skipL077
.condpart19
 jsr .collidep0b1

.skipL077
.L078 ;  rem VERTICAL BORDER COLLISION

.L079 ;  if missile0y  <=  1 then missile0dy = # - missile0dy

	LDA #1
	CMP missile0y
     BCC .skipL079
.condpart20
	LDA ##
	SEC
	SBC missile0dy
	STA missile0dy
.skipL079
.L080 ;  if missile0y  <=  1 then missile0y = 1

	LDA #1
	CMP missile0y
     BCC .skipL080
.condpart21
	LDA #1
	STA missile0y
.skipL080
.L081 ;  if missile1y  <=  1 then missile1dy = # - missile1dy

	LDA #1
	CMP missile1y
     BCC .skipL081
.condpart22
	LDA ##
	SEC
	SBC missile1dy
	STA missile1dy
.skipL081
.L082 ;  if missile1y  <=  1 then missile1y = 1

	LDA #1
	CMP missile1y
     BCC .skipL082
.condpart23
	LDA #1
	STA missile1y
.skipL082
.L083 ;  if missile0y  >=  88 then missile0dy =  # - missile0dy

	LDA missile0y
	CMP #88
     BCC .skipL083
.condpart24
	LDA ##
	SEC
	SBC missile0dy
	STA missile0dy
.skipL083
.L084 ;  if missile0y  >=  88 then missile0y = 88

	LDA missile0y
	CMP #88
     BCC .skipL084
.condpart25
	LDA #88
	STA missile0y
.skipL084
.L085 ;  if missile1y  >=  88 then missile1dy =  # - missile1dy

	LDA missile1y
	CMP #88
     BCC .skipL085
.condpart26
	LDA ##
	SEC
	SBC missile1dy
	STA missile1dy
.skipL085
.L086 ;  if missile1y  >=  88 then missile1y = 88

	LDA missile1y
	CMP #88
     BCC .skipL086
.condpart27
	LDA #88
	STA missile1y
.skipL086
.L087 ;  rem HORIZONTAL BORDER COLLISION

.L088 ;  if missile0x  <=  16 then goto player2win

	LDA #16
	CMP missile0x
     BCC .skipL088
.condpart28
 jmp .player2win

.skipL088
.L089 ;  if missile0x  >=  175 then goto player1win

	LDA missile0x
	CMP #175
     BCC .skipL089
.condpart29
 jmp .player1win

.skipL089
.L090 ;  if missile1x  <=  16 then goto player2win

	LDA #16
	CMP missile1x
     BCC .skipL090
.condpart30
 jmp .player2win

.skipL090
.L091 ;  if missile1x  >=  175 then goto player1win

	LDA missile1x
	CMP #175
     BCC .skipL091
.condpart31
 jmp .player1win

.skipL091
.L092 ;  rem BLOCK COLLISION

.
 ; 

.L093 ;  if collision(missile0,playfield) then gosub pixelcollide0

	BIT CXM0FB
	BPL .skipL093
.condpart32
 jsr .pixelcollide0

.skipL093
.L094 ;  if collision(missile1,playfield) then gosub pixelcollide1

	BIT CXM1FB
	BPL .skipL094
.condpart33
 jsr .pixelcollide1

.skipL094
.
 ; 

.L095 ;  rem MUSIC

.L096 ;  if musicTimer  <= 1 then gosub changeMusicNote

	LDA #1
	CMP musicTimer
     BCC .skipL096
.condpart34
 jsr .changeMusicNote

.skipL096
.L097 ;  musicTimer  =  musicTimer  -  1

	DEC musicTimer
.L098 ;  if soundTimer  >  0 then AUDF1  =  26

	LDA #0
	CMP soundTimer
     BCS .skipL098
.condpart35
	LDA #26
	STA AUDF1
.skipL098
.L099 ;  if soundTimer  >  0 then AUDV1  =  5

	LDA #0
	CMP soundTimer
     BCS .skipL099
.condpart36
	LDA #5
	STA AUDV1
.skipL099
.L0100 ;  if soundTimer  >=  1 then soundTimer  =  soundTimer  -  1 else AUDV1  =  0

	LDA soundTimer
	CMP #1
     BCC .skipL0100
.condpart37
	DEC soundTimer
 jmp .skipelse0
.skipL0100
	LDA #0
	STA AUDV1
.skipelse0
.L0101 ;  goto gameloop

 jmp .gameloop

.
 ; 

.L0102 ;  return

	RTS
.
 ; 

.pixelcollide0
 ; pixelcollide0

.L0103 ;  tempy =  ( missile0y )  / 8

; complex statement detected
	LDA missile0y
	lsr
	lsr
	lsr
	STA tempy
.L0104 ;  tempx  =  missile0x

	LDA missile0x
	STA tempx
.L0105 ;  if tempx  <=  86 then tempx =  ( missile0x )  / 5  -  4

	LDA #86
	CMP tempx
     BCC .skipL0105
.condpart38
; complex statement detected
	LDA missile0x
	LDY #5
 jsr div8
	SEC
	SBC #4
	STA tempx
.skipL0105
.L0106 ;  if tempx  <  96  &&  tempx  >  86 then tempx =  ( missile0x )  / 5  -  3

	LDA tempx
	CMP #96
     BCS .skipL0106
.condpart39
	LDA #86
	CMP tempx
     BCS .skip39then
.condpart40
; complex statement detected
	LDA missile0x
	LDY #5
 jsr div8
	SEC
	SBC #3
	STA tempx
.skip39then
.skipL0106
.L0107 ;  if tempx  >=  96  &&  tempx  < 100 then tempx =  ( missile0x )  / 5  -  3

	LDA tempx
	CMP #96
     BCC .skipL0107
.condpart41
	LDA tempx
	CMP #100
     BCS .skip41then
.condpart42
; complex statement detected
	LDA missile0x
	LDY #5
 jsr div8
	SEC
	SBC #3
	STA tempx
.skip41then
.skipL0107
.L0108 ;  if tempx  >=  100 then tempx =  ( missile0x )  / 5  -  2

	LDA tempx
	CMP #100
     BCC .skipL0108
.condpart43
; complex statement detected
	LDA missile0x
	LDY #5
 jsr div8
	SEC
	SBC #2
	STA tempx
.skipL0108
.L0109 ;  pfpixel tempx tempy off

	LDA tempx
	LDY tempy
	LDX #1
 jsr pfpixel
.L0110 ;  rem if missile0dx = #-1 then missile0dx = 1

.L0111 ;  if missile0dx  =  1 then missile0dx  =  # - 1 else missile0dx  =  1

	LDA missile0dx
	CMP #1
     BNE .skipL0111
.condpart44
	LDA ##
	SEC
	SBC #1
	STA missile0dx
 jmp .skipelse1
.skipL0111
	LDA #1
	STA missile0dx
.skipelse1
.L0112 ;  rem missile0dy = #-missile0dy

.L0113 ;  return

	RTS
.pixelcollide1
 ; pixelcollide1

.L0114 ;  tempy =  ( missile1y )  / 8

; complex statement detected
	LDA missile1y
	lsr
	lsr
	lsr
	STA tempy
.L0115 ;  tempx  =  missile1x

	LDA missile1x
	STA tempx
.L0116 ;  if tempx  <=  86 then tempx =  ( missile1x )  / 5  -  4

	LDA #86
	CMP tempx
     BCC .skipL0116
.condpart45
; complex statement detected
	LDA missile1x
	LDY #5
 jsr div8
	SEC
	SBC #4
	STA tempx
.skipL0116
.L0117 ;  if tempx  <  96  &&  tempx  >  86 then tempx =  ( missile1x )  / 5  -  3

	LDA tempx
	CMP #96
     BCS .skipL0117
.condpart46
	LDA #86
	CMP tempx
     BCS .skip46then
.condpart47
; complex statement detected
	LDA missile1x
	LDY #5
 jsr div8
	SEC
	SBC #3
	STA tempx
.skip46then
.skipL0117
.L0118 ;  if tempx  >=  96  &&  tempx  < 100 then tempx =  ( missile1x )  / 5  -  3

	LDA tempx
	CMP #96
     BCC .skipL0118
.condpart48
	LDA tempx
	CMP #100
     BCS .skip48then
.condpart49
; complex statement detected
	LDA missile1x
	LDY #5
 jsr div8
	SEC
	SBC #3
	STA tempx
.skip48then
.skipL0118
.L0119 ;  if tempx  >=  100 then tempx =  ( missile1x )  / 5  -  2

	LDA tempx
	CMP #100
     BCC .skipL0119
.condpart50
; complex statement detected
	LDA missile1x
	LDY #5
 jsr div8
	SEC
	SBC #2
	STA tempx
.skipL0119
.L0120 ;  pfpixel tempx tempy off

	LDA tempx
	LDY tempy
	LDX #1
 jsr pfpixel
.L0121 ;  rem if missile1dx = #-1 then missile1dx = 1

.L0122 ;  if missile1dx  =  1 then missile1dx  =  # - 1 else missile1dx  =  1

	LDA missile1dx
	CMP #1
     BNE .skipL0122
.condpart51
	LDA ##
	SEC
	SBC #1
	STA missile1dx
 jmp .skipelse2
.skipL0122
	LDA #1
	STA missile1dx
.skipelse2
.L0123 ;  rem missile1dy = #-missile1dy

.L0124 ;  return

	RTS
.
 ; 

.collidep0b0
 ; collidep0b0

.L0125 ;  z  =  player0y  -  missile0y

	LDA player0y
	SEC
	SBC missile0y
	STA z
.L0126 ;  z  =  z / 4

	LDA z
	lsr
	lsr
	STA z
.L0127 ;  if z  >=  2 then missile0dy  =  # - 1

	LDA z
	CMP #2
     BCC .skipL0127
.condpart52
	LDA ##
	SEC
	SBC #1
	STA missile0dy
.skipL0127
.L0128 ;  if z  <=  1 then missile0dy  =  1

	LDA #1
	CMP z
     BCC .skipL0128
.condpart53
	LDA #1
	STA missile0dy
.skipL0128
.L0129 ;  missile0dx  =  1

	LDA #1
	STA missile0dx
.L0130 ;  missile0x  =  missile0x  +  missile0dx

	LDA missile0x
	CLC
	ADC missile0dx
	STA missile0x
.L0131 ;  missile0y  =  missile0y  +  missile0dy

	LDA missile0y
	CLC
	ADC missile0dy
	STA missile0y
.L0132 ;  return

	RTS
.
 ; 

.collidep1b0
 ; collidep1b0

.L0133 ;  z  =  player1y  -  missile0y

	LDA player1y
	SEC
	SBC missile0y
	STA z
.L0134 ;  z  =  z / 4

	LDA z
	lsr
	lsr
	STA z
.L0135 ;  if z  >=  2 then missile0dy  =  # - 1

	LDA z
	CMP #2
     BCC .skipL0135
.condpart54
	LDA ##
	SEC
	SBC #1
	STA missile0dy
.skipL0135
.L0136 ;  if z  <=  1 then missile0dy  =  1

	LDA #1
	CMP z
     BCC .skipL0136
.condpart55
	LDA #1
	STA missile0dy
.skipL0136
.L0137 ;  missile0dx  =  # - 1

	LDA ##
	SEC
	SBC #1
	STA missile0dx
.L0138 ;  missile0x  =  missile0x  +  missile0dx

	LDA missile0x
	CLC
	ADC missile0dx
	STA missile0x
.L0139 ;  missile0y  =  missile0y  +  missile0dy

	LDA missile0y
	CLC
	ADC missile0dy
	STA missile0y
.L0140 ;  score  =  score  +  10000

	SED
	CLC
	LDA score
	ADC #$01
	STA score
	CLD
.L0141 ;  return

	RTS
.
 ; 

.collidep0b1
 ; collidep0b1

.L0142 ;  z  =  player0y  -  missile1y

	LDA player0y
	SEC
	SBC missile1y
	STA z
.L0143 ;  z  =  z / 4

	LDA z
	lsr
	lsr
	STA z
.
 ; 

.L0144 ;  if z  >=  2 then missile1dy  =  # - 1

	LDA z
	CMP #2
     BCC .skipL0144
.condpart56
	LDA ##
	SEC
	SBC #1
	STA missile1dy
.skipL0144
.L0145 ;  if z  <=  1 then missile1dy  =  1

	LDA #1
	CMP z
     BCC .skipL0145
.condpart57
	LDA #1
	STA missile1dy
.skipL0145
.L0146 ;  missile1dx  =  1

	LDA #1
	STA missile1dx
.L0147 ;  missile1x  =  missile1x  +  missile1dx

	LDA missile1x
	CLC
	ADC missile1dx
	STA missile1x
.L0148 ;  missile1y  =  missile1y  +  missile1dy

	LDA missile1y
	CLC
	ADC missile1dy
	STA missile1y
.L0149 ;  score  =  score  +  10

	SED
	CLC
	LDA score+2
	ADC #$10
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0150 ;  return

	RTS
.
 ; 

.collidep1b1
 ; collidep1b1

.L0151 ;  z  =  player1y  -  missile1y

	LDA player1y
	SEC
	SBC missile1y
	STA z
.L0152 ;  z  =  z / 4

	LDA z
	lsr
	lsr
	STA z
.
 ; 

.L0153 ;  if z  >=  2 then missile1dy  =  # - 1

	LDA z
	CMP #2
     BCC .skipL0153
.condpart58
	LDA ##
	SEC
	SBC #1
	STA missile1dy
.skipL0153
.L0154 ;  if z  <=  1 then missile1dy  =  1

	LDA #1
	CMP z
     BCC .skipL0154
.condpart59
	LDA #1
	STA missile1dy
.skipL0154
.L0155 ;  missile1dx  =  # - 1

	LDA ##
	SEC
	SBC #1
	STA missile1dx
.L0156 ;  missile1x  =  missile1x  +  missile1dx

	LDA missile1x
	CLC
	ADC missile1dx
	STA missile1x
.L0157 ;  missile1y  =  missile1y  +  missile1dy

	LDA missile1y
	CLC
	ADC missile1dy
	STA missile1y
.L0158 ;  return

	RTS
.
 ; 

.startball0
 ; startball0

.L0159 ;  missile0dx  =  # - 1

	LDA ##
	SEC
	SBC #1
	STA missile0dx
.L0160 ;  return

	RTS
.
 ; 

.startball1
 ; startball1

.L0161 ;  missile1dx  =  1

	LDA #1
	STA missile1dx
.L0162 ;  return

	RTS
.
 ; 

.playsound
 ; playsound

.L0163 ;  AUDV1  =  8

	LDA #8
	STA AUDV1
.L0164 ;  soundTimer  =  10

	LDA #10
	STA soundTimer
.L0165 ;  return

	RTS
.
 ; 

.player1win
 ; player1win

.L0166 ;  score  =  score  +  1000

	SED
	CLC
	LDA score+1
	ADC #$10
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0167 ;  goto startgame

 jmp .startgame

.
 ; 

.player2win
 ; player2win

.L0168 ;  score  =  score  +  1

	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0169 ;  goto startgame

 jmp .startgame

.
 ; 

.changeMusicNote
 ; changeMusicNote

.L0170 ;  musicPointer  =  musicPointer  +  1

	INC musicPointer
.
 ; 

.
 ; 

.L0171 ;  musicPointer  =  musicPointer  +  1

	INC musicPointer
.L0172 ;  rem value is (2 * #_OF_NOTES) - 1

.L0173 ;  if musicPointer  >  29 then musicPointer  =  # - 1

	LDA #29
	CMP musicPointer
     BCS .skipL0173
.condpart60
	LDA ##
	SEC
	SBC #1
	STA musicPointer
.skipL0173
.L0174 ;  return

	RTS
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL018_0

	.byte  %00111100
	.byte  %01111110
	.byte  %01111110
	.byte  %11111111
	.byte  %11100111
	.byte  %11111111
	.byte  %11011011
	.byte  %01111110
	.byte  %00111100
	.byte  %00011000
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL020_1

	.byte  %00111100
	.byte  %01111110
	.byte  %01111110
	.byte  %11111111
	.byte  %11100111
	.byte  %11111111
	.byte  %11011011
	.byte  %01111110
	.byte  %00111100
	.byte  %00011000
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif


scoretable
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 


 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
