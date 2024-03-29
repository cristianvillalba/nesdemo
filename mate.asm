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
pattern    .rs 1;
pixelstep  .rs 1; interlacing render (this is needed to paint something in the sprite 0 to generate the switch pattern logic
tempx	   .rs 1; variables to translate sdf
tempy      .rs 1;
tempz      .rs 1;
temp1xhi   .rs 1; variables for matrix multiplication
temp1xlo   .rs 1;
temp1yhi   .rs 1;
temp1ylo   .rs 1;
temp1zhi   .rs 1;
temp1zlo   .rs 1;
temp2xhi   .rs 1; variables for matrix multiplication
temp2xlo   .rs 1;
temp2yhi   .rs 1;
temp2ylo   .rs 1;
temp2zhi   .rs 1;
temp2zlo   .rs 1;
divisor	   .rs 1;next used for hi-byte
dividend   .rs 2;next used for hi-byte
materad	   .rs 1;
planeposh  .rs 1;
thickness  .rs 1;
qvecx      .rs 1;
qvecy      .rs 1;
wvalue     .rs 1;
isyneg     .rs 1;
isxneg     .rs 1;
yflip      .rs 1;
rayyflip   .rs 1;
pillh      .rs 1;
pillr      .rs 1;
finalclampy .rs 1;
map1val    .rs 1;
map2val    .rs 1;

; for ca65
PPUMASK = $2001
PPUADDR = $2006
PPUDATA = $2007
src = 200 ; Address C8 in RAM
srcbackground = 202; Adress CA in RAM
chrdata = 210; Adddress D2 in RAM

  .bank 0		; load bank 0 or PRG code - in nesASM code is divided in banks of 8k
  .org $8000	; starting at memory address $8000 - full 32k NESROM-256
  
  .include "famitone2.asm"
  .include "desprolijo.asm"
  
  
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
	LDA #60
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

	JSR initfirstloop
otherpattern:
	LDA pattern
	CLC
	ADC #1
	STA pattern

	ldy #$0 ; start with 0 in vertical row until 1E
loophcolumn:
	ldx #$0 ;start with 0 in horizontal column until 10
	JSR initnegx; reset initialx
calcsprite:
	stx spritecolumn
	sty spriterow
	;raymarching goes here-------------------------------------------------------
	
	JSR initvars
	
	LDY spritecolumn ;if the spritecolumn is different than 0
	CPY #0
	BEQ checkrows
	JSR addpixelx
checkrows:
	LDY spriterow ;if the spriterow is different than 0
	CPY #0
	BEQ eachpixelstart
	JSR addpixely
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
	jsr changeinterlace
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
	jsr checksignature
continuenormalprocess:
	cpx #16; break if we reach 16
	bne calcsprite
	iny
	cpy #16; breka if we reach 16
	bne loophcolumn
	
	LDA pattern
	CMP #0
	BEQ otherpattern
breakprocessloop:
	LDA #0 ;break the pixel processing
	STA render ; break the pixel processing
	STA timers;
	STA timerm
	STA timerm2
	
	ldx #LOW(desprolijo_music_data)
	ldy #HIGH(desprolijo_music_data)
	lda #$80;This sets Famitone to use NTSC mode.
	;lda #$0;This sets Famitone to use PAL mode.
	jsr FamiToneInit

	lda #0;Play first subsong
	jsr FamiToneMusicPlay
	
	;finally start looping the NMI
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	

		
	JMP Foreverloop		;jump back to Forever, infinite loop

initpixel:
	LDA #0
	STA finalcolor
		
	LDA #64 ;start at 64 two compliment value
	;EOR #$FF
	;CLC
	;ADC #$01
	STA rayoriginz;
	
	RTS
	
checksignature:
	LDA pattern
	CMP #1
	BEQ checksignature1
	JMP continuenormalprocess

checksignature1:
	cpx #13
	BEQ checksignature2
	JMP continuenormalprocess

checksignature2:
	cpy #15
	BEQ putsignature
	JMP continuenormalprocess

putsignature:
	lda #$00  ; load the source address into a pointer in zero page
	sta src
	lda #$A0
	sta src+1
	
	ldy #0
	ldx #48      ; copy 3 patterns of 16 bytes each CEV signature
loopsignature:
	lda [src],y  ; copy one byte
	sta PPUDATA
	iny
	dex
	bne loopsignature  ; repeat until we've copied enough
	JMP breakprocessloop
	
initvars:
	;init some variables
	LDA #64 ;start at 64 value
	STA rayoriginx;
	
	LDA pattern
	CMP #0
	BEQ firstpatterny
	LDA #8 ;start at 8 in second pattern table
	EOR #$FF
	CLC
	ADC #$01
	STA rayoriginy;
	JMP makeneg
firstpatterny:
	LDA #120; start at -120 in first pattern table
makeneg:
	EOR #$FF
	CLC
	ADC #$01
	STA rayoriginy;
	
	LDA #0
	STA datavalue
	STA datavalue2
	
	LDA #$FF
	STA dataindex
	
	LDA rayoriginy
	EOR #$FF
	CLC
	ADC #$01
	STA rayyflip;
	RTS

changeinterlace:
	LDA pixelstep
	CMP #0
	BEQ flipstep1
	LDA #0
	STA pixelstep
	RTS
flipstep1:
	LDA #1
	STA pixelstep
	RTS
	
addpixelx:
	LDA rayoriginx  ; Load one operand into the accumulator.
	SEC        ; Clear the carry flag so it does not get added into the result
	SBC #8      ; subs 8 pixels
	STA rayoriginx ; Store the operand back to x
	DEY
	BNE addpixelx
	STA initialx; store the initial x value
	RTS

addpixely:
	LDA rayoriginy  ; Load one operand into the accumulator.
	CLC        ; Clear the carry flag so it does not get added into the result
	ADC #8      ; Add 8 pixels
	STA rayoriginy ; Store the operand back to x
	
	LDA rayoriginy
	EOR #$FF
	CLC
	ADC #$01
	STA rayyflip;
	
	DEY
	BNE addpixely
	RTS

initfirstloop:
	ldy #0       ; starting index into the first page, PPU address $0000 meaning pattern table 0
	sty PPUMASK  ; turn off rendering just in case
	sty PPUADDR  ; load the destination address into the PPU
	sty PPUADDR
	
	lda #$D3  ; load the direction D3 in the D2 pointer
	sta chrdata
	
	LDA #$FF	;start with pattern table number 0
	STA pattern
	RTS

restoreoriginx:
	LDA initialx
	STA rayoriginx
	RTS
	
incrayx:
	SEC
	LDA rayoriginx
	SBC #1
	STA rayoriginx
	RTS

incrayy:
	CLC
	LDA rayoriginy
	ADC #1
	STA rayoriginy
	
	LDA rayoriginy
	EOR #$FF
	CLC
	ADC #$01
	STA rayyflip;
	
	LDA #0
	STA datavalue ;clean data value
	STA datavalue2 ;clean the other data value
	RTS
	
initnegx:
	LDA #64 ;start with -64 in x
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
	
	LDA rayoriginx
	STA tempx
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuewithy
	LDA tempx
	EOR #$FF
	CLC
	ADC #$01
	STA tempx
continuewithy:
	;LDA rayoriginy
	LDA rayyflip
	EOR #$FF
	CLC
	ADC #$01
	STA yflip
	;CLC
	;ADC #64 ;this will translate y 64 units up
	STA tempy
	AND #%10000000  ;check if value is negative, if it is, then make it positive
	BEQ continuewithz;
	LDA tempy
	EOR #$FF
	CLC
	ADC #$01
	STA tempy
continuewithz:
	LDA rayoriginz	
	STA tempz
	AND #%10000000   ;check if value is negative, if it is, then make it positive
	BEQ rotatestart
	LDA tempz
	EOR #$FF
	CLC
	ADC #$01
	STA tempz
rotatestart:
	;just for debugging
	;JMP pillmap
	;using phitagorean triplets to rotate
	;this will rotate in the Z axis 
	;iq example pos.xy = (mat2(5,12,-12,5)/13.0)*pos.xy;
	;float tempx = 12.0*pos.x -5.0*pos.y;
    ;float tempy = 5.0*pos.x + 12.0*pos.y;
    ;pos.x = tempx / 13.0;
    ;pos.y = tempy / 13.0;
	
	;calc tempx
	LDA tempx
	LDY #12
	JSR mult16
	STA temp1xhi
	LDA num1
	STA temp1xlo
	
	LDA rayoriginx   ;check if x value is negative
	AND #%10000000 
	BEQ step1
	SEC
	LDA #$00
	SBC temp1xlo
    STA temp1xlo
	LDA #$00
	SBC temp1xhi
	STA temp1xhi
step1:
	LDA tempy
	LDY #5
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA rayoriginy   ;check if y value is negative
	LDA rayyflip
	AND #%10000000
	BEQ step1plus
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

step1plus:
	SEC              ;complement to add
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

	clc				; clear carry
	lda temp1xlo
	adc temp1ylo
	sta temp2xlo			; store sum of LSBs
	lda temp1xhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2xhi			; the previous calculation
	
step2:
	;calc tempy
	LDA tempx
	LDY #5
	JSR mult16
	STA temp1xhi
	LDA num1
	STA temp1xlo
	
	LDA rayoriginx   ;check if x value is negative
	AND #%10000000 
	BEQ step3
	SEC
	LDA #$00
	SBC temp1xlo
    STA temp1xlo
	LDA #$00
	SBC temp1xhi
	STA temp1xhi
	
step3:
	LDA tempy
	LDY #12
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA rayoriginy   ;check if y value is negative
	LDA rayyflip
	AND #%10000000 
	BEQ step4jplus
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi
	
step4jplus:
	clc				; clear carry
	lda temp1xlo
	adc temp1ylo
	sta temp2ylo			; store sum of LSBs
	lda temp1xhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2yhi			; the previous calculation
	
	;----------------------------------------------------------------------
	LDA #0
	STA isxneg
	
	LDA temp2xhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivx
	SEC
	LDA #$00
	SBC temp2xlo
    STA temp2xlo
	LDA #$00
	SBC temp2xhi
	STA temp2xhi
	
	LDA #1
	STA isxneg
continuedivx:
	lda temp2xlo            ;divide by 13
	sta dividend
	lda temp2xhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2xlo
	sta tempx
	lda dividend + 1
	sta temp2xhi
	;------------------------------------------
	LDA #0
	STA isyneg
	
	LDA temp2yhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivy
	SEC
	LDA #$00
	SBC temp2ylo
    STA temp2ylo
	LDA #$00
	SBC temp2yhi
	STA temp2yhi
	
	LDA #1
	STA isyneg
	
continuedivy:
	lda temp2ylo            ;divide by 13
	sta dividend
	lda temp2yhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2ylo
	sta tempy
	lda dividend + 1
	sta temp2yhi
	
	;-----------------------------------------------------------------------------------------------------
	;using phitagorean triplets to rotate
	;this will rotate in the X axis 
	;iq example pos.zy = (mat2(5,12,-12,5)/13.0)*pos.zy;
	;float tempz = 12.0*pos.z -5.0*pos.y;
    ;float tempy = 5.0*pos.z + 12.0*pos.y;
    ;pos.z = tempz / 13.0;
    ;pos.y = tempy / 13.0;
	
	;calc tempz
	LDA tempz
	LDY #12
	JSR mult16
	STA temp1zhi
	LDA num1
	STA temp1zlo
	
	LDA rayoriginz   ;check if z value is negative
	AND #%10000000 
	BEQ step1z
	SEC
	LDA #$00
	SBC temp1zlo
    STA temp1zlo
	LDA #$00
	SBC temp1zhi
	STA temp1zhi
step1z:
	LDA tempy
	LDY #5
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA yflip   ;check if y value is negative
	;AND #%10000000
	LDA isyneg
	CMP #0
	BEQ step1plusz
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

step1plusz:
	SEC              ;complement to add
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

	clc				; clear carry
	lda temp1zlo
	adc temp1ylo
	sta temp2zlo			; store sum of LSBs
	lda temp1zhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2zhi			; the previous calculation
	
step2z:
	;calc tempz
	LDA tempz
	LDY #5
	JSR mult16
	STA temp1zhi;
	LDA num1
	STA temp1zlo
	
	LDA rayoriginz   ;check if z value is negative
	AND #%10000000 
	BEQ step3z
	SEC
	LDA #$00
	SBC temp1zlo
    STA temp1zlo
	LDA #$00
	SBC temp1zhi
	STA temp1zhi
	
step3z:
	LDA tempy
	LDY #12
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA yflip   ;check if y value is negative
	;AND #%10000000 
	LDA isyneg
	CMP #0
	BEQ step4jpluz
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi
	
step4jpluz:
	clc				; clear carry
	lda temp1zlo
	adc temp1ylo
	sta temp2ylo			; store sum of LSBs
	lda temp1zhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2yhi			; the previous calculation
	
	;----------------------------------------------------------------------
	
	LDA temp2zhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivz
	SEC
	LDA #$00
	SBC temp2zlo
    STA temp2zlo
	LDA #$00
	SBC temp2zhi
	STA temp2zhi
continuedivz:
	lda temp2zlo            ;divide by 13
	sta dividend
	lda temp2zhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2zlo
	sta tempz
	lda dividend + 1
	sta temp2zhi

	;------------------------------------------
	LDA #0
	STA isyneg
	
	LDA temp2yhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivyz
	SEC
	LDA #$00
	SBC temp2ylo
    STA temp2ylo
	LDA #$00
	SBC temp2yhi
	STA temp2yhi
	
	LDA #1
	STA isyneg
	
continuedivyz:
	lda temp2ylo            ;divide by 13
	sta dividend
	lda temp2yhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2ylo
	sta tempy
	lda dividend + 1
	sta temp2yhi
	

	
contmatez:
	;-----------------------------------------------------------------------------------------------------
	;float r = 0.5; radius of sphere
    ;float h = 0.2; cut plane position
    ;float t = 0.01; thickness
	lda #45
	sta materad
	lda #30
	sta planeposh
	lda #2
	sta thickness
	
	;vec2( length(p.xz), p.y );
	LDA tempx
	LDY tempx
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDY tempz
	LDA tempz
	JSR mult16
	STA zvarhi
	LDA num1
	STA zvarlow
	
	;add all results
	clc						; clear carry
	lda xvarlow
	adc zvarlow
	sta resultlow			; store sum of LSBs
	lda xvarhi
	adc zvarhi				; add the MSBs using carry from
	sta resulthi			; the previous calculation
	
	JSR sqrt
	
	LDA resultlow
	STA qvecx
	
	LDA tempy
	STA qvecy
	
	; float w = sqrt(r*r-h*h);
	LDA materad
	LDY materad
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDA planeposh
	LDY planeposh
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	sec				; set carry for borrow purpose
	lda xvarlow
	sbc yvarlow			; perform subtraction on the LSBs
	sta resultlow
	lda xvarhi			; do the same for the MSBs, with carry
	sbc yvarhi			; set according to the previous result
	sta resulthi
	
	JSR sqrt
	
	LDA resultlow
	STA wvalue
		
	;(h*q.x<w*q.y)
	LDA planeposh
	LDY qvecx
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDA wvalue
	LDY qvecy
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	;LDA rayoriginy  ; patch to check y negative values
	;AND #%10000000
	LDA isyneg
	CMP #1
	BEQ HigherBrc;
	
	lda     xvarhi
	cmp     yvarhi
	bne     decideh
	lda     xvarlow
    cmp     yvarlow
decideh:
	BCC LowerBrc
	BNE HigherBrc
LowerBrc:
	;length(q-vec2(w,h))
	sec				; set carry for borrow purpose
	lda qvecx
	sbc wvalue		; perform subtraction on the LSBs
	sta temp1xlo
	
	sec				; set carry for borrow purpose
	lda qvecy
	sbc planeposh		; perform subtraction on the LSBs
	sta temp1ylo
	
	LDA temp1xlo
	LDY temp1xlo
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDA temp1ylo
	LDY temp1ylo
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	clc						; clear carry
	lda xvarlow
	adc yvarlow
	sta resultlow			; store sum of LSBs
	lda xvarhi
	adc yvarhi				; add the MSBs using carry from
	sta resulthi			; the previous calculation
	
	JSR sqrt

	LDA resultlow
	STA map1val
	JMP pillmap
HigherBrc:
	;abs(length(q)-r)  - t
	LDA qvecx
	LDY qvecx
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDA qvecy
	LDY qvecy
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	clc						; clear carry
	lda xvarlow
	adc yvarlow
	sta resultlow			; store sum of LSBs
	lda xvarhi
	adc yvarhi				; add the MSBs using carry from
	sta resulthi			; the previous calculation
	
	JSR sqrt
	
	clc
	LDA resultlow
	SBC materad
	STA resultlow
	
	LDA resultlow
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ minusthick
	LDA resultlow
	EOR #$FF
	CLC
	ADC #$01
	STA resultlow

minusthick:
;	STA resultlow
	;minus thickness at the end
	SEC
	LDA resultlow
	SBC thickness
	STA resultlow
	
	LDA resultlow
	STA map1val
	JMP pillmap

;----------------------------------------------------------------------
;pill map
; p.y -= clamp( p.y, 0.0, h );
pillmap:
	LDA #2
	STA pillr
	
	LDA #100
	STA pillh
	
	;SEC
	LDA rayoriginx 
	;SBC #30 ;move pill in x axis y 30 units
	STA tempx ;
	
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuewithyb
	LDA tempx
	EOR #$FF
	CLC
	ADC #$01
	STA tempx
continuewithyb:
	;LDA rayoriginy
	LDA rayyflip
	EOR #$FF
	CLC
	ADC #$01
	STA yflip
	;CLC
	;ADC #64 ;this will translate y 64 units up
	STA tempy
	AND #%10000000  ;check if value is negative, if it is, then make it positive
	BEQ continuewithzb;
	LDA tempy
	EOR #$FF
	CLC
	ADC #$01
	STA tempy
continuewithzb:
	LDA rayoriginz
	
	SEC
	SBC #35
	STA tempz
	AND #%10000000   ;check if value is negative, if it is, then make it positive
	BEQ rotatestartb
	LDA tempz
	EOR #$FF
	CLC
	ADC #$01
	STA tempz
rotatestartb:
	;just for debugging
	;JMP pillmap
	;using phitagorean triplets to rotate
	;this will rotate in the Z axis 
	;iq example pos.xy = (mat2(5,12,-12,5)/13.0)*pos.xy;
	;float tempx = 12.0*pos.x -5.0*pos.y;
    ;float tempy = 5.0*pos.x + 12.0*pos.y;
    ;pos.x = tempx / 13.0;
    ;pos.y = tempy / 13.0;
	
	;calc tempx
	LDA tempx
	LDY #12
	JSR mult16
	STA temp1xhi
	LDA num1
	STA temp1xlo
	
	;SEC
	LDA rayoriginx   ;check if x value is negative
	;SBC #30;move pill in x axis y 30 units
	AND #%10000000 
	BEQ step1b
	SEC
	LDA #$00
	SBC temp1xlo
    STA temp1xlo
	LDA #$00
	SBC temp1xhi
	STA temp1xhi
step1b:
	LDA tempy
	LDY #5
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	LDA rayoriginy   ;check if y value is negative
	LDA rayyflip
	AND #%10000000
	BEQ step1plusb
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

step1plusb:
	SEC              ;complement to add
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

	clc				; clear carry
	lda temp1xlo
	adc temp1ylo
	sta temp2xlo			; store sum of LSBs
	lda temp1xhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2xhi			; the previous calculation
	
step2b:
	;calc tempy
	LDA tempx
	LDY #5
	JSR mult16
	STA temp1xhi
	LDA num1
	STA temp1xlo
	
	SEC
	LDA rayoriginx   ;check if x value is negative
	SBC #30;move pill in x axis y 30 units
	AND #%10000000 
	BEQ step3b
	SEC
	LDA #$00
	SBC temp1xlo
    STA temp1xlo
	LDA #$00
	SBC temp1xhi
	STA temp1xhi
	
step3b:
	LDA tempy
	LDY #12
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA rayoriginy   ;check if y value is negative
	LDA rayyflip
	AND #%10000000 
	BEQ step4jplusb
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi
	
step4jplusb:
	clc				; clear carry
	lda temp1xlo
	adc temp1ylo
	sta temp2ylo			; store sum of LSBs
	lda temp1xhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2yhi			; the previous calculation
	
	;----------------------------------------------------------------------
	LDA #0
	STA isxneg
	
	LDA temp2xhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivxb
	SEC
	LDA #$00
	SBC temp2xlo
    STA temp2xlo
	LDA #$00
	SBC temp2xhi
	STA temp2xhi
	
	LDA #1
	STA isxneg
continuedivxb:
	lda temp2xlo            ;divide by 13
	sta dividend
	lda temp2xhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2xlo
	sta tempx
	lda dividend + 1
	sta temp2xhi
	;------------------------------------------
	LDA #0
	STA isyneg
	
	LDA temp2yhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivyb
	SEC
	LDA #$00
	SBC temp2ylo
    STA temp2ylo
	LDA #$00
	SBC temp2yhi
	STA temp2yhi
	
	LDA #1
	STA isyneg
	
continuedivyb:
	lda temp2ylo            ;divide by 13
	sta dividend
	lda temp2yhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2ylo
	sta tempy
	lda dividend + 1
	sta temp2yhi
	
	; ;-----------------------------------------------------------------------------------------------------
	;using phitagorean triplets to rotate
	;this will rotate in the X axis 
	;iq example pos.zy = (mat2(5,12,-12,5)/13.0)*pos.zy;
	;float tempz = 12.0*pos.z -5.0*pos.y;
    ;float tempy = 5.0*pos.z + 12.0*pos.y;
    ;pos.z = tempz / 13.0;
    ;pos.y = tempy / 13.0;
	
	;calc tempz
	LDA tempz
	LDY #12
	JSR mult16
	STA temp1zhi
	LDA num1
	STA temp1zlo
	
	LDA rayoriginz   ;check if z value is negative
	AND #%10000000 
	BEQ step1zb
	SEC
	LDA #$00
	SBC temp1zlo
    STA temp1zlo
	LDA #$00
	SBC temp1zhi
	STA temp1zhi
step1zb:
	LDA tempy
	LDY #5
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA yflip   ;check if y value is negative
	;AND #%10000000
	LDA isyneg
	CMP #0
	BEQ step1pluszb
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

step1pluszb:
	SEC              ;complement to add
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi

	clc				; clear carry
	lda temp1zlo
	adc temp1ylo
	sta temp2zlo			; store sum of LSBs
	lda temp1zhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2zhi			; the previous calculation
	
step2zb:
	;calc tempz
	LDA tempz
	LDY #5
	JSR mult16
	STA temp1zhi;
	LDA num1
	STA temp1zlo
	
	LDA rayoriginz   ;check if z value is negative
	AND #%10000000 
	BEQ step3zb
	SEC
	LDA #$00
	SBC temp1zlo
    STA temp1zlo
	LDA #$00
	SBC temp1zhi
	STA temp1zhi
	
step3zb:
	LDA tempy
	LDY #12
	JSR mult16
	STA temp1yhi
	LDA num1
	STA temp1ylo
	
	;LDA yflip   ;check if y value is negative
	;AND #%10000000 
	LDA isyneg
	CMP #0
	BEQ step4jpluzb
	SEC
	LDA #$00
	SBC temp1ylo
    STA temp1ylo
	LDA #$00
	SBC temp1yhi
	STA temp1yhi
	
step4jpluzb:
	clc				; clear carry
	lda temp1zlo
	adc temp1ylo
	sta temp2ylo			; store sum of LSBs
	lda temp1zhi
	adc temp1yhi			; add the MSBs using carry from
	sta temp2yhi			; the previous calculation
	
	;----------------------------------------------------------------------
	
	LDA temp2zhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivzb
	SEC
	LDA #$00
	SBC temp2zlo
    STA temp2zlo
	LDA #$00
	SBC temp2zhi
	STA temp2zhi
continuedivzb:
	lda temp2zlo            ;divide by 13
	sta dividend
	lda temp2zhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2zlo
	sta tempz
	lda dividend + 1
	sta temp2zhi

	;------------------------------------------
	LDA #0
	STA isyneg
	
	LDA temp2yhi
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ continuedivyzb
	SEC
	LDA #$00
	SBC temp2ylo
    STA temp2ylo
	LDA #$00
	SBC temp2yhi
	STA temp2yhi
	
	LDA #1
	STA isyneg
	
continuedivyzb:
	lda temp2ylo            ;divide by 13
	sta dividend
	lda temp2yhi
	sta dividend + 1
	lda #13
	sta divisor
	jsr divide
	
	lda dividend
	sta temp2ylo
	sta tempy
	lda dividend + 1
	sta temp2yhi
	

clamp0:
	LDA rayyflip
    CMP #$80
    BCC clamp1 ;lower means rayyflip positive
    BNE clamp2 ;higher means ryayflip negative
	
clamp1:
	;using rayyflip
	LDA rayyflip
	CMP pillh
	BCC clamp3
	BNE clamp4
clamp2:
	;using zero
	LDA #0
	CMP pillh
	BCC clamp5
	BNE clamp6
clamp3:
	LDA rayyflip
	STA finalclampy
	jmp pill1
clamp4:
	LDA pillh
	STA finalclampy
	jmp pill1
clamp5:
	LDA #0
	STA finalclampy
	jmp pill1
clamp6:
	LDA pillh
	STA finalclampy
	
pill1:
	;compliment to add
	LDA finalclampy
	EOR #$FF
	CLC
	ADC #$01
	STA finalclampy
	
	LDA rayyflip
	CLC
	ADC finalclampy
	STA tempy
	AND #%10000000  ;check if value is negative, if it is, then make it positive
	BEQ lenghtofp;
	LDA tempy
	EOR #$FF
	CLC
	ADC #$01
	STA tempy

lenghtofp:
	LDA tempx
	LDY tempx
	JSR mult16
	STA xvarhi
	LDA num1
	STA xvarlow
	
	LDY tempy
	LDA tempy
	JSR mult16
	STA yvarhi
	LDA num1
	STA yvarlow
	
	LDY tempz
	LDA tempz
	JSR mult16
	STA zvarhi
	LDA num1
	STA zvarlow
	
	;add all values and sqrt
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
	
	JSR sqrt
	
	;and minus radius
	clc
	LDA resultlow
	SBC pillr ;pill radius
	STA resultlow
	STA map2val
	
	LDA map1val
	CMP map2val
	BCC union1 ; take map1val
	BNE union2 ; take map2val
union1:
	LDA map1val
	STA resultlow
	jmp finalizemap
union2:
	LDA map2val
	STA resultlow
	jmp finalizemap
finalizemap:
	LDA resultlow
    CMP #2
	BCC distancehit
	BNE returnmap
distancehit:
	LDA #0
	STA dist
returnmap:
	LDX prevx;
	LDY prevy;
	RTS
	
	
raymarch:
	LDA #0
	STA raysteps ;init raysteps to 0
	
	LDA #5
	STA resultlow
	
	LDA pixelstep
	CMP #1
	BEQ invertflag
	LDA #1
	STA pixelstep
	jmp raymarchloop
invertflag:
	LDA #0
	sta pixelstep
	JMP flagmodify
raymarchloop:
	;advance ray 5 units
	;LDA rayoriginz
	;CLC
	;ADC #5
	;STA rayoriginz
	
	;or advance sdf value
	;LDA rayoriginz
	;CLC
	;ADC resultlow
	;STA rayoriginz
	SEC
	LDA rayoriginz
	SBC resultlow
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
	LDA rayoriginz ;invert rayorignz first
	EOR #$FF
	CLC
	ADC #$01
	STA rayoriginz
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ checkrange1
	LDA rayoriginz
	EOR #$FF
	CLC
	ADC #$01
checkrange1:
	SEC
	SBC #40
	BPL changecolor02;
	LDA rayoriginz
	AND #%10000000     ;check if value is negative, if it is, then make it positive
	BEQ checkrange2
	LDA rayoriginz
	EOR #$FF
	CLC
	ADC #$01
checkrange2:
	SEC
	SBC #23
	BPL changecolor01;
	LDA datavalue
	ORA bitchoose
	STA datavalue
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
	LDA datavalue2
	ORA bitchoose
	STA datavalue2
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

divide:
	ldx #16
	lda #0
divloop:
	asl dividend
	rol dividend+1
	rol a
	cmp divisor
	bcc no_sub
	sbc divisor
	inc dividend
no_sub:
	dex
	bne divloop
	rts

sqrt:	;take the sqrt
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
	RTS

	
NMI:					;non maskable interrupt, this is one of 2 main interrupts, the nmi is for updating paint, the other resets.	
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  
	;;This is the PPU clean up section, so rendering the next frame starts properly.
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	
	LDA #$00        ;;tell the ppu there is no background scrolling
	STA $2005
	STA $2005
	
	;move ball
;	LDA ballx
;	CLC
;	ADC #$01        ;;ballx position = ballx + ballspeedx
	LDA #74
	STA ballx
	
	LDA #125
	STA bally
	
	;this will update all ball sprite info in the RAM memory space $0200
	LDA bally
	STA $0200
  
	LDA #$00	   ;;pattern number 0 in pattern table
	STA $0201
  
	LDA #$00
	STA $0202
  
	LDA ballx
	STA $0203
	;;update paddle sprites	

	
WaitForNoHit:
	LDA $2002
	AND #%01000000
	BNE WaitForNoHit
WaitForHit:
	LDA $2002
	AND #%01000000
	BEQ WaitForHit

	;inmediately switch to a bank 1 of pattern table for background
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA $2000
		
	;LDA render
	;CMP #0
	;BEQ checktimers ; dont check timers
	
	;;update music
	jsr FamiToneUpdate;Other lines here for context	

		
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
  .db $00,$7E,$40,$40,  $40,$40,$7E,$00,  $00,$00,$00,$00,  $00,$00,$00,$00   ;;sprite pattern 1 <----Letter C
  .db $00,$7E,$40,$78,  $40,$40,$7E,$00,  $00,$00,$00,$00,  $00,$00,$00,$00   ;;sprite pattern 2 <----Letter E
  .db $00,$42,$42,$44,  $28,$28,$10,$00,  $00,$00,$00,$00,  $00,$00,$00,$00   ;;sprite pattern 3 <----Letter V

  .org $B000    ;start of the second 256 tile table
  .db $00,$00,$00,$00,  $00,$00,$00,$00,  $00,$00,$00,$00,  $00,$00,$00,$00   ;;sprite pattern 1

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
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$90,$91,$92,$93,$94,$95,$96,$97  ;;row 10
  .db $98,$99,$9A,$9B,$9C,$9D,$9E,$9F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7  ;;row 11
  .db $A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7  ;;row 12
  .db $B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7  ;;row 13
  .db $C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7  ;;row 14
  .db $D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7  ;;row 15
  .db $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7  ;;row 16
  .db $F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07  ;;row 17
  .db $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$10,$11,$12,$13,$14,$15,$16,$17  ;;row 18
  .db $18,$19,$1A,$1B,$1C,$1D,$1E,$1F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$20,$21,$22,$23,$24,$25,$26,$27  ;;row 19
  .db $28,$29,$2A,$2B,$2C,$2D,$2E,$2F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$30,$31,$32,$33,$34,$35,$36,$37  ;;row 20
  .db $38,$39,$3A,$3B,$3C,$3D,$3E,$3F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$40,$41,$42,$43,$44,$45,$46,$47  ;;row 21
  .db $48,$49,$4A,$4B,$4C,$4D,$4E,$4F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$50,$51,$52,$53,$54,$55,$56,$57  ;;row 22
  .db $58,$59,$5A,$5B,$5C,$5D,$5E,$5F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$60,$61,$62,$63,$64,$65,$66,$67  ;;row 23
  .db $68,$69,$6A,$6B,$6C,$6D,$6E,$6F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$70,$71,$72,$73,$74,$75,$76,$77  ;;row 24
  .db $78,$79,$7A,$7B,$7C,$7D,$7E,$7F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$82,$83,$84,$85,$86,$87  ;;row 25
  .db $88,$89,$8A,$8B,$8C,$8D,$8E,$8F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$90,$91,$92,$93,$94,$95,$96,$97  ;;row 26
  .db $98,$99,$9A,$9B,$9C,$9D,$9E,$9F,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7  ;;row 27
  .db $A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$00,$00,$00,$FD,$FE,$FF,$00,$00  ;;adding cev signature
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7  ;;row 28
  .db $B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7  ;;row 29
  .db $C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
  .db $00,$00,$00,$00,$00,$00,$00,$00,$D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7  ;;row 30
  .db $D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,$00,$00,$00,$00,$00,$00,$00,$00  ;;
  
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
  .db $22,$14,$24,$34,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$14,$24,$34,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

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
