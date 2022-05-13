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

; for ca65
PPUMASK = $2001
PPUADDR = $2006
PPUDATA = $2007
src = 200 ; Address C8 in RAM
srcbackground = 202; Adress who knows in RAM

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
	

LoadAttribute:
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
	LDA #$50
	STA bally
  
	LDA #$80
	STA ballx

	;finally start looping the NMI
	;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001

Foreverloop:
	JMP Foreverloop		;jump back to Forever, infinite loop
	
NMI:					;non maskable interrupt, this is one of 2 main interrupts, the nmi is for updating game logic, the other resets.
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  
  
	;;This is the PPU clean up section, so rendering the next frame starts properly.
	;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
	STA $2000

	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA $2001
	
	LDA #$00        ;;tell the ppu there is no background scrolling
	STA $2005
	STA $2005
	
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

	RTI

;---------------other bank-----------------
  .bank 1
  .org $A000	;I will use this bank as an internal store of CHR pattern data, 16 bytes for each tile, two 256 tiles tables
  .db $FF,$00,$00,$00,  $00,$00,$00,$FF,  $FF,$00,$00,$00,  $00,$00,$00,$FF   ;;sprite pattern 1
  .db $00,$FF,$FF,$00,  $00,$FF,$FF,$00,  $00,$FF,$FF,$00,  $00,$FF,$FF,$00   ;;sprite pattern 2

;----------------bank-----------------
  .bank 2		
  .org $C000

;----------------bank-------------------------------
  .bank 3		;now defining blank for pallete and interrupts
  .org $E000

;32x30 sprites background
nametable:;background rows are split into two 16 byte sections to keep lines shorter
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  ;;row 1
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  ;;taking pattern 1

  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  ;;row 2
  .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  ;;taking pattern 1

  .db $24,$24,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;

  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 14
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 15
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
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
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $24,$24,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;
  
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;row 29
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;
  
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;row 30
  .db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ;;
  
attribute:;finally put some colors, 64 byte attribute table
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000
  .db %00010000, %00010000, %0010000, %00010000, %00010000, %00010000, %00010000, %00110000

palette:;colors defined in the pallete
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
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