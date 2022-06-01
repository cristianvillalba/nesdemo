  .inesprg 2	; 2x 16KB PRG code - 32k NESROM-256
;  .inesprg 1	; 1x 16KB PRG code - 16k NESROM-128
;  .ineschr 1	; 1x  8KB CHR data
  .ineschr 0	; CHR-RAM
  .inesmap 0	; mapper 0 = NROM, no bank swapping
  .inesmir 2	; single screen
  
;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
ballx      .rs 1  ; ball horizontal position
bally      .rs 1  ; ball vertical position
render     .rs 1  ; compute raymarch or not
rayoriginx .rs 1  ;
rayoriginy .rs 1  ;
rayoriginz .rs 1  ;
raydirx    .rs 1  ;
raydiry    .rs 1  ;
raydirz    .rs 1  ;
pixelx	   .rs 1  ;
pixely     .rs 1  ;
spritecolumn .rs 1;
spriterow  .rs 1  ;
raysteps    .rs 1  ;
dist       .rs 1  ;
prevpixelrow .rs 1;
dataindex  .rs 1;
finalcolor .rs 1;
datavalue  .rs 1;
initialx   .rs 1;
num1	   .rs 1;
num2	   .rs 1;
num1Hi	   .rs 1;
xvarlow	   .rs 1;
xvarhi	   .rs 1;
yvarlow	   .rs 1;
yvarhi	   .rs 1;
zvarlow	   .rs 1;
zvarhi	   .rs 1;
resultlow  .rs 1;
resulthi   .rs 1;
sqr22	   .rs 1;
sqr23	   .rs 1;
prevx	   .rs 1;
prevy      .rs 1;
datavalue2 .rs 1;
bitchoose  .rs 1;
timers     .rs 1;
timerm     .rs 1;
timerm2    .rs 1;
radius     .rs 1;


; for ca65
PPUMASK = $2001
PPUADDR = $2006
PPUDATA = $2007
src = 200 ; Address C8 in RAM
srcbackground = 202; Adress CA in RAM
chrdata = 210; Adddress D2 in RAM

  .bank 0		; load bank 0 or PRG code - in nesASM code is divided in banks of 8k
  .org $8000	; starting at memory address $8000 - full 32k NESROM-256
  
RESET:
	SEI			; disable IRQs
	CLD			; disable decimal mode
	LDX #$40	
	STX $4017	; disable APU frame IRQ
	LDX #$FF	
	TXS			; Set up stack
	INX			; now X = 0
	STX $2000	; disable NMI
	STX $2001	; disable rendering
	STX $4010	; disable DMC IRQs
  
; asm code is sequencial, meaning that this will first wait a vblank, then a clear mem, then another vblank and finally infinite loop
vblankwait1:	; First wait for vblank to make sure PPU is ready
	BIT $2002
	BPL vblankwait1

clrmem:
	LDA #$00
	STA $0000, x
	STA $0100, x
	STA $0200, x
	STA $0400, x
	STA $0500, x
	STA $0600, x
	STA $0700, x
	LDA #$FE
	STA $0300, x
	INX
	BNE clrmem
   
vblankwait2:			;Second wait for vblank, PPU is ready after this
	BIT $2002
	BPL vblankwait2
	
copy_custom_chr:
	lda #$00  ; load the source address into a pointer in zero page
	sta src
	lda #$A0
	sta src+1

	ldy #0       ; starting index into the first page, PPU address $0000 meaning pattern table 0
	sty PPUMASK  ; turn off rendering just in case
	sty PPUADDR  ; load the destination address into the PPU
	sty PPUADDR
	ldx #32      ; number of 256-byte pages to copy, 32 pages in decimal - total of 8kbytes patterns, it will copy 2 pattern tables(sprite - background)
looppaint:
	lda [src],y  ; copy one byte
	sta PPUDATA
	iny
	bne looppaint  ; repeat until we finish the page
	inc src+1  ; go to the next page
	dex
	bne looppaint  ; repeat until we've copied enough pages

LoadBackground:
	LDA $2002             ; read PPU status to reset the high/low latch
	LDA #$20
	STA $2006             ; write the high byte of $2000 address
	LDA #$00
	STA $2006             ; write the low byte of $2000 address
  
	lda #$00              ; load $E000 in the indirect pointer
	sta srcbackground
	lda #$E0
	sta srcbackground+1
  
	ldy #0              	; start out at 0
	ldx #3      			; reading 3 pages of 256 bytes, giving 768 bytes final, reading 192 more will finish the read	
LoadBackgroundLoop:
	LDA [srcbackground], y     ; load data from address (background + the value in x)
	STA $2007             ; write to PPU
	INY
	bne LoadBackgroundLoop  ; repeat until we finish the page
	inc srcbackground+1  ; go to the next page
	dex
	bne LoadBackgroundLoop  ; repeat until we've copied enough pagesNot Equal to zero
                        ; if compare was equal to 128, keep going down
	
	;--------final read
	lda #$00              ; load $E300 in the indirect pointer to finish with the read
	sta srcbackground
	lda #$E3
	sta srcbackground+1
	
	ldy #0              	; start out at 0
LoadBackgroundFinalLoop:
	LDA [srcbackground], y  ;
	STA $2007             ; write to PPU
	INY
	CPY #$C0
	bne LoadBackgroundFinalLoop
	

LoadAttribute:          ; controls which palette is assigned to each part of the background.
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $40, decimal 64 - copying 64 bytes
  BNE LoadAttributeLoop


LoadPalettes:
	LDA $2002             ; read PPU status to reset the high/low latch
	LDA #$3F
	STA $2006             ; write the high byte of $3F00 address (3F00 is the address of image pallete in PPU)
	LDA #$00
	STA $2006             ; write the low byte of $3F00 address
	LDX #$00              ; start out at 0
	
LoadPalettesLoop:
	LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
	STA $2007             ; write to PPU
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes = 16 bytes for pallete background and 16 for pallete sprites
	BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
						  ; if compare was equal to 32, keep going down

;;;Set some initial ball stats
	LDA #$70
	STA bally
  
	LDA #$80
	STA ballx
	
	LDA #$1
	STA render
	
	LDA #25
	STA radius

Foreverloop:
	LDA render ;check if we need to raymarch (calculate pixels)
	BEQ Foreverloop

	ldy #0       ; starting index into the first page, PPU address $0000 meaning pattern table 0
	sty PPUMASK  ; turn off rendering just in case
	sty PPUADDR  ; load the destination address into the PPU
	sty PPUADDR

	lda #$D3  ; load the direction D3 in the D2 pointer
	sta chrdata

	ldy #$0 ; start with 0 in vertical row until 1E
loophcolumn:
	ldx #$0 ;start with 0 in horizontal column until 10
	;STX initialx; reset initialx
	JSR initnegx; reset initialx
calcsprite:
	stx spritecolumn
	sty spriterow
	;raymarching goes here-------------------------------------------------------
	
	JSR initvars
	
	LDY spritecolumn ;if the spritecolumn is different than 0
	CPY #0
	BEQ checkrows
addpixelx:
	LDA rayoriginx  ; Load one operand into the accumulator.
	CLC        ; Clear the carry flag so it does not get added into the result
	ADC #8      ; Add 8 pixels
	STA rayoriginx ; Store the operand back to x
	DEY
	BNE addpixelx
	STA initialx; store the initial x value
checkrows:
	LDY spriterow ;if the spriterow is different than 0
	CPY #0
	BEQ eachpixelstart
addpixely:
	LDA rayoriginy  ; Load one operand into the accumulator.
	CLC        ; Clear the carry flag so it does not get added into the result
	ADC #8      ; Add 8 pixels
	STA rayoriginy ; Store the operand back to x
	DEY
	BNE addpixely

eachpixelstart:
	LDY #0
eachpixel:
	CLC
	LDA dataindex
	ADC #1
	STA dataindex

	LDX #0
pixelcol:
	;now this is the individual pixel part
	jsr initpixel
	jsr raymarch

	JSR incrayx
	inx
	cpx #8; break if we reach 8
	bne pixelcol
	
	sty prevpixelrow ; store y for further use
	JSR transfercolor;
	
	
	jsr incrayy
	jsr restoreoriginx
	
	LDY prevpixelrow ; back to the previous y value
	
	iny
	cpy #8; break if we reach 8
	bne eachpixel
	;end of raymarching---------------------------------------------------------
	
	jsr transfersprite
	
	LDX spritecolumn ; take back previous values
	LDY spriterow ;take back previous values
	inx
	cpx #16; break if we reach 16
	bne calcsprite
	iny
	cpy #16; breka if we reach 16
	bne loophcolumn
	
	LDA #0 ;break the pixel processing
	STA render ; break the pixel processing
	STA timers;
	STA timerm
	STA timerm2
	
	;finally start looping the NMI
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	
	JMP Foreverloop		;jump back to Forever, infinite loop

initpixel:
	LDA #0
	;STA rayoriginz
	STA finalcolor
	
	LDA #32 ;start at -32 two compliment value
	EOR #$FF
	CLC
	ADC #$01
	STA rayoriginz;
	
	RTS
	
initvars:
	;init some variables
	LDA #64 ;start at -64 two compliment value
	EOR #$FF
	CLC
	ADC #$01
	STA rayoriginx;
	STA rayoriginy;
	
	LDA #0
	;STA rayoriginz;
	STA datavalue
	STA datavalue2
	
	LDA #$FF
	STA dataindex
	
	RTS

restoreoriginx:
	LDA initialx
	STA rayoriginx
	RTS
	
incrayx:
	CLC
	LDA rayoriginx
	ADC #1
	STA rayoriginx
	RTS

incrayy:
	CLC
	LDA rayoriginy
	ADC #1
	STA rayoriginy
	LDA #0
	STA datavalue ;clean data value
	STA datavalue2 ;clean the other data value
	RTS
	
initnegx:
	LDA #64
	EOR #$FF
	CLC
	ADC #$01
	STA initialx
	RTS
	
transfercolor:
	LDY dataindex    
	LDA datavalue
	STA $D3,y; put final data in dataindex
	
	LDA datavalue2
	STA $DB,y; put final data2 in dataindex
	RTS

map:
	STX prevx;
	STY prevy;
	
	;mult x by x
	LDA rayoriginx
	STA resultlow
	SBC #$7F         ;check if value is negative, if it is, then make it positive
	BMI nchangeposx
	LDA rayoriginx
	EOR #$FF
	CLC
	ADC #$01
	STA resultlow
nchangeposx:
	LDA resultlow
	LDY resultlow
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	;mult y by y
	LDA rayoriginy 
	STA resultlow
	SBC #$7F        ;check if value is negative, if it is, then make it positive
	BMI nchangeposy
	LDA rayoriginy
	EOR #$FF
	CLC
	ADC #$01
	STA resultlow
nchangeposy:
	LDA resultlow
	LDY resultlow
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	;mult z by z
	LDA rayoriginz
	STA resultlow
	SBC #$7F    ;check if value is negative, if it is, then make it positive
	BMI nchangeposz
	LDA rayoriginz
	EOR #$FF
	CLC
	ADC #$01
	STA resultlow
nchangeposz:
	LDA resultlow
	LDY resultlow
	JSR mult16
	STA zvarhi
	LDA num1
	STA zvarlow
	
	;add all results
	clc						; clear carry
	lda xvarlow
	adc yvarlow
	sta resultlow			; store sum of LSBs
	lda xvarhi
	adc yvarhi				; add the MSBs using carry from
	sta resulthi			; the previous calculation
	
	clc						; clear carry
	lda resultlow
	adc zvarlow
	sta resultlow			; store sum of LSBs
	lda resulthi
	adc zvarhi				; add the MSBs using carry from
	sta resulthi			; the previous calculation
	
	
	;take the sqrt
	LDY #$01 ; lsby of first odd number = 1
	STY sqr22
	DEY
	STY sqr23 ; msby of first odd number (sqrt = 0)
sqagain:
	SEC
	LDA resultlow ; save remainder in X register
	TAX ; subtract odd lo from integer lo
	SBC sqr22
	STA resultlow
	LDA resulthi ; subtract odd hi from integer hi
	SBC sqr23
	STA resulthi ; is subtract result negative?
	BCC sqnomore ; no. increment square root
	INY
	LDA sqr22 ; calculate next odd number
	ADC #$01
	STA sqr22
	BCC sqagain
	INC sqr23
	JMP sqagain
sqnomore:
	STY resultlow ; all done, store square root
	STX resulthi ; and remainder

	CLC
	lda resultlow
	SBC radius;take the radious of initially 25
	STA resultlow
	
	LDA resultlow
    CMP #10
    BCS returnmap
	
	LDA #0
	STA dist
returnmap:
	LDX prevx;
	LDY prevy;
	RTS
	
	
raymarch:
	LDA #0
	STA raysteps ;init raysteps to 0
raymarchloop:
	;advance ray 5 units
	LDA rayoriginz
	CLC
	ADC #5
	STA rayoriginz
	
	;advance 1 step in loop
	LDA raysteps
	CLC
	ADC #1
	STA raysteps
	
	;init distance
	LDA #$FF
	STA dist
	
	jsr map
	LDA raysteps
	CMP #10
	BEQ breakinfinty
	LDA dist
	CMP #0
	BNE raymarchloop
	
	LDA #1
	STA finalcolor
	
breakinfinty:
	LDA finalcolor
	CMP #1
	BEQ flagmodify
	RTS
;fill data here
flagmodify:
	LDA #0
	STA bitchoose; init bitchoose
	CPX #0
	BEQ flag0
	CPX #1
	BEQ flag1
	CPX #2
	BEQ flag2
	CPX #3
	BEQ flag3
	CPX #4
	BEQ flag4
	CPX #5
	BEQ flag5
	CPX #6
	BEQ flag6
	CPX #7
	BEQ flag7
	RTS
flag0:
	LDA bitchoose
	ORA #%10000000
	STA bitchoose
	JMP paintflag
flag1:
	LDA bitchoose
	ORA #%01000000
	STA bitchoose
	JMP paintflag
flag2:
	LDA bitchoose
	ORA #%00100000
	STA bitchoose
	JMP paintflag
flag3:
	LDA bitchoose
	ORA #%00010000
	STA bitchoose
	JMP paintflag
flag4:
	LDA bitchoose
	ORA #%00001000
	STA bitchoose
	JMP paintflag
flag5:
	LDA bitchoose
	ORA #%00000100
	STA bitchoose
	JMP paintflag
flag6:
	LDA bitchoose
	ORA #%00000010
	STA bitchoose
	JMP paintflag
flag7:
	LDA bitchoose
	ORA #%00000001
	STA bitchoose
	JMP paintflag
paintflag:
	LDA raysteps
	SBC #5
	BPL changecolor02;
	LDA raysteps
	SBC #1
	BPL changecolor01;
	LDA datavalue
	ORA bitchoose
	STA datavalue
	LDA datavalue2
	ORA bitchoose
	STA datavalue2
	RTS
changecolor01:
	LDA datavalue2
	ORA bitchoose
	STA datavalue2
	RTS
changecolor02:
	LDA datavalue
	ORA bitchoose
	STA datavalue
	RTS
	
transfersprite:
	LDY #0
loopbpattern:
	;lda [chrdata],y  ; copy one byte
	lda $D3,y ; copy one byte
	sta PPUDATA
	iny
	CPY #16; if 16 bytes are copied
	bne loopbpattern  ; repeat until we finish the sprite
	RTS

mult16:
	lsr a  ; prime the carry bit for the loop
	sta num1
	sty num2
	lda #0
	ldy #8
loopmulti:
  ; At the start of the loop, one bit of prodlo has already been
  ; shifted out into the carry.
	bcc noaddmulti
	clc
	adc num2
noaddmulti:
	ror a
	ror num1  ; pull another bit out for the next iteration
	dey         ; inc/dec don't modify carry; only shifts and adds do
	bne loopmulti
	rts

	
NMI:					;non maskable interrupt, this is one of 2 main interrupts, the nmi is for updating paint, the other resets.
	;LDA #$00
	;STA $2003       ; set the low byte (00) of the RAM address
	;LDA #$02
	;STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  
	;;This is the PPU clean up section, so rendering the next frame starts properly.
	;LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	;STA $2000

	;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	;STA $2001
	
	;LDA #$00        ;;tell the ppu there is no background scrolling
	;STA $2005
	;STA $2005
	
	;move ball
	LDA ballx
	CLC
	ADC #$01        ;;ballx position = ballx + ballspeedx
	STA ballx
	
	;this will update all ball sprite info in the RAM memory space $0200
	LDA bally
	STA $0200
  
	LDA #$01	   ;;pattern number 1 in pattern table
	STA $0201
  
	LDA #$00
	STA $0202
  
	LDA ballx
	STA $0203
	;;update paddle sprites
		
	
;WaitForNoHit:
;	LDA $2002
;	AND #%01000000
;	BNE WaitForNoHit
;WaitForHit:
;	LDA $2002
;	AND #%01000000
;	BEQ WaitForHit
	
	LDA render
	CMP #0
	BEQ checktimers
	RTI
	
checktimers:
	CLC
	LDA timers
	ADC #1
	STA timers
	CLC
	SBC #60
	BPL addsecond
	RTI

addsecond:
	LDA #0
	STA timers
	LDA timerm
	CLC
	ADC #1
	STA timerm
	CLC
	SBC #60
	BPL addminute1
	RTI
addminute1:
	LDA #0
	STA timerm
	LDA timerm2
	CLC
	ADC #1
	STA timerm2
	CLC
	SBC #2
	BPL addminute
	RTI
addminute:
	LDX #0
	STX $2000	; disable NMI
	
	LDA radius
	SBC #2
	STA radius

	LDX #1
	STX render
	RTI

;---------------other bank-----------------
  .bank 1
  .org $A000	;I will use this bank as an internal store of CHR pattern data, 16 bytes for each tile, two 256 tiles tables
  .db $FF,$00,$00,$00,  $00,$00,$00,$FF,  $00,$00,$00,$00,  $00,$00,$00,$00   ;;sprite pattern 1
  .db $00,$FF,$FF,$00,  $00,$FF,$FF,$00,  $00,$FF,$FF,$00,  $00,$FF,$FF,$00   ;;sprite pattern 2

  .org $B000    ;start of the second 256 tile table
  .db $AA,$0C,$0C,$00,  $00,$0C,$0C,$AA,  $AA,$0C,$0C,$00,  $00,$0C,$0C,$AA   ;;sprite pattern 1

;----------------bank-----------------
  .bank 2		
  .org $C000

;----------------bank-------------------------------
  .bank 3		;now defining blank for pallete and interrupts
  .org $E000

;32x30 sprites background
nametable:;background rows are split into two 16 byte sections to keep lines shorter
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07  ;;row 1
  .db $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$00,$00,$00,$00,$00,$00,$00,$00  ;;taking pattern 1

  .db $00,$00,$00,$00,$00,$00,$00,$00,$10,$11,$12,$13,$14,$15,$16,$17  ;;row 2
  .db $18,$19,$1A,$1B,$1C,$1D,$1E,$1F,$00,$00,$00,$00,$00,$00,$00,$00  ;;taking pattern 1

  .db $00,$00,$00,$00,$00,$00,$00,$00,$20,$21,$22,$23,$24,$25,$26,$27  ;;row 3
  .db $28,$29,$2A,$2B,$2C,$2D,$2E,$2F,$00,$00,$00,$00,$00,$00,$00,$00  ;;

  .db $00,$00,$00,$00,$00,$00,$00,$00,$30,$31,$32,$33,$34,$35,$36,$37  ;;row 4
  .db $38,$39,$3A,$3B,$3C,$3D,$3E,$3F,$00,$00,$00,$00,$00,$00,$00,$00  ;;

  .db $00,$00,$00,$00,$00,$00,$00,$00,$40,$41,$42,$43,$44,$45,$46,$47  ;;row 5
  .db $48,$49,$4A,$4B,$4C,$4D,$4E,$4F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$50,$51,$52,$53,$54,$55,$56,$57  ;;row 6
  .db $58,$59,$5A,$5B,$5C,$5D,$5E,$5F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$60,$61,$62,$63,$64,$65,$66,$67  ;;row 7
  .db $68,$69,$6A,$6B,$6C,$6D,$6E,$6F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$70,$71,$72,$73,$74,$75,$76,$77  ;;row 8
  .db $78,$79,$7A,$7B,$7C,$7D,$7E,$7F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$82,$83,$84,$85,$86,$87  ;;row 9
  .db $88,$89,$8A,$8B,$8C,$8D,$8E,$8F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$90,$91,$92,$93,$94,$95,$96,$97  ;;row 90
  .db $98,$99,$9A,$9B,$9C,$9D,$9E,$9F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7  ;;row AA
  .db $A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7  ;;row B2
  .db $B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7  ;;row C3
  .db $C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7  ;;row D4
  .db $D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7  ;;row E5
  .db $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7  ;;row F6
  .db $F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  ;;row 26
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$56,$00,$00  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;row 29
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;
  
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;row 30
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;
  
attribute:;finally put some colors, 64 byte attribute table
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000

palette:;colors defined in the pallete
  .db $22,$29,$2A,$2B,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

;These are just values that describe where to place a sprite on the screen and its index in the tile map
;I think it is not used, code needed to upload this section
sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3

;final vector interrupt values
  .org $FFFA	;first of the three vectors starts here
  .dw NMI		;when an NMI happens (once per frame if enabled) the processor will jump to the label NMI:
  .dw RESET		;when the processor first turns on or is reset, it will jump to the label RESET:
  .dw 0			;external interrupt IRQ is not used in this tutorial

;------remove this if we want to use CHR RAM-----------
;----------------bank of sprites-------------------
;  .bank 4
; .org $0000
;  .incbin "mario.chr"	;includes 8KB graphics file from SMB1
