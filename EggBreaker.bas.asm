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
game
.L00 ;  rem **********************************

.L01 ;  rem Egg Breaker

.L02 ;  rem LMC 2700

.L03 ;  rem Project 6

.L04 ;  rem Lilliann Andrews, Sranee Bayapureddy, David Le

.L05 ;  rem **********************************

.
 ; 

.L06 ;  set romsize 4k

.L07 ;  set kernel_options pfcolors

.
 ; 

.L08 ;  rem setting colors for the playfield's rows

.L09 ;  pfcolors:

 lda # $00
 sta COLUPF
 ifconst pfres
 lda #>(pfcolorlabel13-132+pfres*pfwidth)
 else
 lda #>(pfcolorlabel13-84)
 endif
 sta pfcolortable+1
 ifconst pfres
 lda #<(pfcolorlabel13-132+pfres*pfwidth)
 else
 lda #<(pfcolorlabel13-84)
 endif
 sta pfcolortable
.
 ; 

.L010 ;  player0color:

	LDA #<playercolorL010_0

	STA player0color
	LDA #>playercolorL010_0

	STA player0color+1
.
 ; 

.L011 ;  player1color:

	LDA #<playercolorL011_1

	STA player1color
	LDA #>playercolorL011_1

	STA player1color+1
.
 ; 

.L012 ;  rem drawing the playfield

.L013 ;  playfield:

  ifconst pfres
	  ldx #(9>pfres)*(pfres*pfwidth-1)+(9<=pfres)*35
  else
	  ldx #35
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %11011011, %10110110, %10110110, %11011011
	.byte %11011011, %10110110, %10110110, %11011011
	.byte %11011011, %10110110, %10110110, %11011011
	.byte %11011011, %10110110, %10110110, %11011011
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ; 

.L014 ;  rem defining the player

.L015 ;  player0:

	LDA #<playerL015_0

	STA player0pointerlo
	LDA #>playerL015_0

	STA player0pointerhi
	LDA #1
	STA player0height
.
 ; 

.L016 ;  player1:

	LDA #<playerL016_1

	STA player1pointerlo
	LDA #>playerL016_1

	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.L017 ;  dim xDirection  =  1

.L018 ;  dim yDirection  =  1

.
 ; 

.L019 ;  dim missile0dx  =  a

.L020 ;  dim missile0dy  =  b

.L021 ;  dim tempx  =  c

.L022 ;  dim tempy  =  d

.
 ; 

.startgame
 ; startgame

.L023 ;  player0x  =  75

	LDA #75
	STA player0x
.L024 ;  player0y  = 88

	LDA #88
	STA player0y
.
 ; 

.L025 ;  player1x  =  30

	LDA #30
	STA player1x
.L026 ;  player1y  =  16

	LDA #16
	STA player1y
.
 ; 

.L027 ;  missile0x = 80

	LDA #80
	STA missile0x
.L028 ;  missile0y = 70

	LDA #70
	STA missile0y
.L029 ;  missile0dx  =  0

	LDA #0
	STA missile0dx
.L030 ;  missile0dy  =  0

	LDA #0
	STA missile0dy
.L031 ;  missile0height =  1

	LDA #1
	STA missile0height
.
 ; 

.L032 ;  rem displays the screen

.draw_loop
 ; draw_loop

.L033 ;  rem color of background

.L034 ;  COLUBK  =  $9E

	LDA #$9E
	STA COLUBK
.L035 ;  COLUP0  =  14

	LDA #14
	STA COLUP0
.L036 ;  COLUP1  =  14

	LDA #14
	STA COLUP1
.
 ; 

.L037 ;  drawscreen

 jsr drawscreen
.L038 ;  if joy0fire  &&  missile0dx  =  0 then gosub startball0

 bit INPT4
	BMI .skipL038
.condpart0
	LDA missile0dx
	CMP #0
     BNE .skip0then
.condpart1
 jsr .startball0

.skip0then
.skipL038
.L039 ;  if joy0right then player0x  =  player0x  +  1 :  if player0x  >  153 then player0x  =  153 :  xDirection  =  1

 bit SWCHA
	BMI .skipL039
.condpart2
	INC player0x
	LDA #153
	CMP player0x
     BCS .skip2then
.condpart3
	LDA #153
	STA player0x
	LDA #1
	STA xDirection
.skip2then
.skipL039
.L040 ;  if joy0left then player0x  =  player0x  -  1 :  if player0x  <  1 then player0x  =  2 :  xDirection  =   - 1

 bit SWCHA
	BVS .skipL040
.condpart4
	DEC player0x
	LDA player0x
	CMP #1
     BCS .skip4then
.condpart5
	LDA #2
	STA player0x
	LDA #255
	STA xDirection
.skip4then
.skipL040
.L041 ;  missile0y  =  missile0y  +  missile0dy

	LDA missile0y
	CLC
	ADC missile0dy
	STA missile0y
.L042 ;  missile0x  =  missile0x  +  missile0dy

	LDA missile0x
	CLC
	ADC missile0dy
	STA missile0x
.L043 ;  rem PADDLE COLLISIONS

.L044 ;  if collision(player0,missile0) then gosub collidep0b0

	BIT CXM0P
	BVC .skipL044
.condpart6
 jsr .collidep0b0

.skipL044
.L045 ;  if missile0y  <= 01 then missile0dy =  - missile0dy

	LDA #01
	CMP missile0y
     BCC .skipL045
.condpart7
	LDA #0
	SEC
	SBC missile0dy
	STA missile0dy
.skipL045
.L046 ;  if collision(missile0,playfield) then gosub pixelcollide0

	BIT CXM0FB
	BPL .skipL046
.condpart8
 jsr .pixelcollide0

.skipL046
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L047 ;  goto draw_loop

 jmp .draw_loop

.
 ; 

.pixelcollide0
 ; pixelcollide0

.L048 ;  rem tempy=(missile0y)/2

.L049 ;  rem tempx = missile0x

.L050 ;  pfpixel tempx tempy off

	LDA tempx
	LDY tempy
	LDX #1
 jsr pfpixel
.L051 ;  rem if missile0dx = #-1 then missile0dx = 1

.L052 ;  if missile0dy  =  1 then missile0dy  =  # - 1 else missile0dx  =  1

	LDA missile0dy
	CMP #1
     BNE .skipL052
.condpart9
	LDA ##
	SEC
	SBC #1
	STA missile0dy
 jmp .skipelse0
.skipL052
	LDA #1
	STA missile0dx
.skipelse0
.L053 ;  rem missile0dy = #-missile0dy

.L054 ;  if missile0y  >=  88 then goto startgame

	LDA missile0y
	CMP #88
     BCC .skipL054
.condpart10
 jmp .startgame

.skipL054
.return
 ; return

.
 ; 

.collidep0b0
 ; collidep0b0

.L055 ;  z  =  player0x  -  missile0x

	LDA player0x
	SEC
	SBC missile0x
	STA z
.L056 ;  z  =  z / 4

	LDA z
	lsr
	lsr
	STA z
.L057 ;  if z  >=  2 then missile0dx  =  # - 1

	LDA z
	CMP #2
     BCC .skipL057
.condpart11
	LDA ##
	SEC
	SBC #1
	STA missile0dx
.skipL057
.L058 ;  if z  <=  1 then missile0dx  =  1

	LDA #1
	CMP z
     BCC .skipL058
.condpart12
	LDA #1
	STA missile0dx
.skipL058
.L059 ;  missile0dy  =   - 1

	LDA #255
	STA missile0dy
.L060 ;  missile0x  =  missile0x  +  missile0dx

	LDA missile0x
	CLC
	ADC missile0dx
	STA missile0x
.L061 ;  missile0y  =  missile0y  +  missile0dy

	LDA missile0y
	CLC
	ADC missile0dy
	STA missile0y
.L062 ;  return

	RTS
.
 ; 

.startball0
 ; startball0

.L063 ;  missile0dy  =  1

	LDA #1
	STA missile0dy
.L064 ;  missile0dx  =  0

	LDA #0
	STA missile0dx
.L065 ;  return
	RTS
 ifconst pfres
 if (<*) > (254-pfres*pfwidth)
	align 256
	endif
 if (<*) < (136-pfres*pfwidth)
	repeat ((136-pfres*pfwidth)-(<*))
	.byte 0
	repend
	endif
 else
 if (<*) > 206
	align 256
	endif
 if (<*) < 88
	repeat (88-(<*))
	.byte 0
	repend
	endif
 endif
pfcolorlabel13
 .byte  $00,0,0,0
 .byte  $40,0,0,0
 .byte  $40,0,0,0
 .byte  $40,0,0,0
 .byte  $40,0,0,0
 .byte  $40,0,0,0
 .byte  $40,0,0,0
 .byte  $00,0,0,0
 .byte  $00,0,0,0
 .byte  $00,0,0,0
 if (<*) > (<(*+8))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL010_0

	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
 if (<*) > (<(*+8))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL011_1

	.byte  $66
	.byte  $66
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
	.byte  $0E
 if (<*) > (<(*+2))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL015_0

	.byte   %1111111
	.byte   %1111111
 if (<*) > (<(*+8))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL016_1

	.byte  %01111110
	.byte  %11111111
	.byte  %11111111
	.byte  %11100111
	.byte  %11111111
	.byte  %11011011
	.byte  %01111110
	.byte  %00111100
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
