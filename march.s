; march-U test for Durango home retrocomputers!
; (c) 2025 Carlos J. Santisteban
; based on https://github.com/misterblack1/appleII_deadtest/
; last modified 20250629-1946

; xa march.s
; add -DPOCKET for non-cartridge, standard RAM version

; *** hardware definitions ***

#ifdef	POCKET
* = $0800					; standard pocket address
#else
* = $E000					; 8K should be more than enough
#endif
; ***********************
; *** standard header ***
; ***********************
rom_start:
; header ID
	.byt	0				; [0]=NUL, first magic number

#ifdef	POCKET
	.asc	"pX"			; pocket executable
	.word	rom_start		; load address
	.word	reset			; execution address
#else
	.asc	"dX"			; bootable ROM for Durango-X devCart
	.asc	"****"			; reserved
#endif

	.byt	13				; [7]=NEWLINE, second magic number
; filename
	.asc	"marchtest", 0	; C-string with filename @ [8]
	.asc	"Based on the work from Adrian Black/World of Jani/IZ8DWF/David Giller", 0	; comment with IMPORTANT attribution

; advance to end of header
	.dsb	rom_start + $E6 - *, $FF

; NEW library commit (user field 2)
	.asc	"$$$$$$$$"
; NEW main commit (user field 1)
	.asc	"$$$$$$$$"
; NEW coded version number
	.word	$1001			; 1.0a1		%vvvvrrrr sshhbbbb, where revision = %hhrrrr, ss = %00 (alpha), %01 (beta), %10 (RC), %11 (final)
; date & time in MS-DOS format at byte 248 ($F8)
	.word	$9400			; time, 18.32		1001 0-100 000-0 0000
	.word	$5ADD			; date, 2025/6/29	0101 101-0 110-1 1101
; filesize in top 32 bits (@ $FC) now including header ** must be EVEN number of pages because of 512-byte sectors
	.word	file_end-rom_start			; actual executable size
	.word	0							; 64K space does not use upper 16 bits, [255]=NUL may be third magic number
; *****************
; *** main code ***
; *****************
reset:
	SEI						; usual 6502 stuff
	CLD
	LDX #$FF
;	TXS
; Durango specific stuff
	STX IOAie				; turn error LED off
	LDA #%10110000			; HIRES mode as usual
	STA IO8mode
; make a clear strip for pres-test progress
	LDA #0					; not using STZ
	TAX
clear:
		STA $7000, X		; about middle of the screen
		INX
		BNE clear			; 256 bytes are 8 rasters on HIRES mode
; at this point we don't trust stack or ZP, test these first
start:	
	LDX #(tst_tbl_end-tst_tbl-1)	; initialize the pointer to the table of values

; step 0; up - w0 - write the test value
marchU:	
		LDA tst_tbl,X		; get the test value into A
		TXS					; save the index of the test value into SP
		TAX					; save the test value into X
		LDY #$00
marchU0:
			STA $00,Y		; w0 - write the test value
			STA $0100,Y		;    - also to stack page
			INY				; count up
			BNE marchU0		; repeat until Y overflows back to zero

; step 1; up - r0,w1,r1,w0
; A contains test value
marchU1:
			EOR $00,Y		; r0 - read and compare with test value (by XOR'ing with accumulator)
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR $0100,Y		; r0s - also stack page
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR #$FF		; invert
			STA $00,Y		; w1 - write the inverted test value
			EOR $00,Y		; r1 - read the same value back and compare using XOR
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR #$FF		; invert
			STA $0100,Y		; w1s - also stack page
			EOR $0100,Y		; r1s
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get a fresh copy of the test value
			STA $00,Y		; w0 - write the test value to the memory location
			STA $0100,Y		; w0s - also stack page
			INY				; count up
			BNE marchU1		; repeat until Y overflows back to zero

; 100ms delay for finding bit rot (unlikely on Durango's SRAM)
		TYA					; Y known to be zero, now A as well
marchU1delay:
				INY
				BNE marchU1delay	; 1279t inner loop
			CLC:ADC #1				; NMOS-savvy
			BNE marchU1delay		; total 329215t, ~94 mS @ 3.5 MHz, ~214 mS @ 1,536 MHz

;		LDY #$00			; reset Y to 0 -- no longer needed
; step 2; up - r0,w1
; A contains test value from prev step
marchU2:
			TXA				; recover test value
			EOR $00,Y		; r0 - read and compare with test value (by XOR'ing with accumulator)
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR $0100,Y		; r0s  - also stack page
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR #$FF		; invert
			STA $00,Y		; w1 - write the inverted test value
			STA $0100,Y		; w1s - also stack page
			INY				; count up
			BNE marchU2		; repeat until Y overflows back to zero

; 100ms delay for finding bit rot (unlikely on Durango's SRAM)
;marchU2delay:
				INY
				BNE marchU2delay	; 1279t inner loop
			CLC:ADC #1				; NMOS-savvy
			BNE marchU2delay		; total 329215t, ~94 mS @ 3.5 MHz, ~214 mS @ 1,536 MHz

; skip to remainder of ZP/Stack test
		JMP continue

zp_bad:
	JMP zp_error			; error routine entry point

continue:
		LDY #$FF			; reset Y to $FF and count down
		TXA					; recover test value
		EOR #$FF			; invert

; step 3; down - r1,w0,r0,w1
marchU3:
			EOR $00,Y		; r1 - read and compare with inverted test value (by XOR'ing with accumulator)
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR #$FF
			EOR $0100,Y		; r1s - also stack page
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			STA $00,Y		; w0 - write the test value
			EOR $00,Y		; r0 - read the same value back and compare using XOR
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get a fresh copy of the test value
			STA $0100,Y		; w0s - write the test value
			EOR $0100,Y		; r0s - read the same value back and compare using XOR
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get a fresh copy of the test value
			EOR #$FF		; invert
			STA $00,Y		; w1 - write the inverted test value
			STA $0100,Y		; w1s - also stack page
			DEY				; count down
			CPY #$FF		; did we wrap?
			BNE marchU3		; repeat until Y overflows back to FF
; step 4; down - r1,w0
; A contains the inverted test value from prev step
marchU4:
			EOR $00,Y		; r1 - read and compare with inverted test value (by XOR'ing with accumulator)
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			EOR #$FF		; invert
			EOR $0100,Y		; r1s - read and compare with inverted test value (by XOR'ing with accumulator)
			BNE zp_bad		; if bits differ, location is bad
			TXA				; get the test value
			STA $00,Y		; w0 - write the test value
			STA $0100,Y		; w0s - also stack page
			EOR #$FF
			DEY				; count down
			CPY #$FF		; did we wrap?
			BNE marchU4		; repeat until Y overflows back to FF
		TSX					; recover the test value index from SP
		DEX					; choose the next one
		CPX #$FF			; see if we've wrapped
		BNE marchup			; start again with next value

	JMP zp_good				; *** ZP/stack test ended ***

marchup:
		JMP marchU			; because of distance...

zp_error:
	TAX						; bat bit mask is in A, save it to X
	TXS  					; then save it in the SP

; *** display some message ***
  
	TSX						; retrieve the test value
	TXA
	LDY #0
print_bit:; ******
	asl				; get top bit into carry flag
		tax				; save the current value
		lda #'0'|$80
		adc #0			; increment by one if we had a carry
		sta $075A,Y		; print bit to screen
		txa
		iny
		cpy #8
		bne print_bit	; repeat 8 times

; find the bit to beep out
	TSX						; get the bad bit mask back into A
	TXA
	LDX #1					; count up
chkbit:	
		LSR					; move lowest bit into carry
	BCS start_beeping		; bit set, display it
		INX					; count down
		CPX #$09
		BNE chkbit			; test next bit
	JMP *					; only get here if there was no bad bit

; now X contains the index of the bit, starting at 1
start_beeping:
	TXS						; save the bit index of the top set bit into SP
beeploop:
		LDA #1
type_beep:					; beep an annoying chirp to indicate page err
			inline_beep_xy $FF, $FF
			SEC
			SBC #1
			BPL type_beep

		TSX					; fetch the bit number
		TXA
bit_beep:
			TAX
			inline_delay_cycles_ay 400000;***
			TXA
;			sta TXTCLR 			; turn on graphics
			inline_beep_xy $FF, $80
;			sta TXTSET			; text mode
			SEC
			SBC #1
			BNE bit_beep

; pause betwen beeping ~1.5 sec
		LDX #3
dl1:
			inline_delay_cycles_ay 500000;***
			DEX
			BNE dl1
		JMP beeploop

; *** bad ZP/SP message ***
bad_msg:
	.asc	"ZP/SP ERR", 0
bad_msg_len = * - bad_msg

zp_good:
; *** some proc section *** REVISE
page_test:
		; ldx #$F0				; simulate error
		; jmp page_error

	LDA #0					; write zero to zp location 0
	TAY
wz:
		STA $00,Y
		DEY
		BNE wz

wr:
		STA $0100,Y			; write to the pages
		LDX $00,Y			; check the zp address
		BNE page_error
		STA $0200,Y
		LDX $00,Y			; check the zp address
		BNE page_error
		STA $0400,Y
		LDX $00,Y			; check the zp address
		BNE page_error
#ifndef	POCKET
		STA $0800,Y
		LDX $00,Y			; check the zp address
		BNE page_error
#endif
		STA $1000,Y
		LDX $00,Y			; check the zp address
		BNE page_error
		STA $2000,Y
		LDX $00,Y			; check the zp address
		BNE page_error
		STA $4000,Y
		LDX $00,Y			; check the zp address
		BNE page_error
		INY
		BNE wr
	JMP page_ok

page_error:
	; TAX				; bat bit mask is in A, save it to X
	TXS  					; then save it in the SP

;	STA TXTSET		; text mode
;	sta MIXSET		; mixed mode on
;	STA LOWSCR		; page 2 off
;	inline_cls
;	inline_print bad_page_msg, $0750;*****

	TSX						; retrieve the test value
	TXA
	LDY #0
print_bit:;***********
	asl				; get top bit into carry flag
	tax				; save the current value
	lda #'0'|$80
	adc #0			; increment by one if we had a carry
	sta $0759,Y		; print bit to screen
	txa
	iny
	cpy #8
	bne print_bit	; repeat 8 times

; find the bit to beep out
	TSX						; get the bad bit mask back into A
	TXA
	CMP #$FF				; if it's FF, it's a motherboard error
	BEQ start_beeping
	LDX #1					; count up
page_chkbit:	
		LSR					; move lowest bit into carry
	BCS start_beeping		; bit set, display it
		inx					; count down
		cpx #$09
		bne page_chkbit		; test next bit
	JMP *					; only get here if there was no bad bit

; now X contains the index of the bit, starting at 1
start_beeping:
	txs					; save the bit index of the top set bit into SP
beeploop:
	ldx #5
type_beep:				; beep an annoying chirp to indicate page err
	inline_delay_cycles_ay 30000
	txa
	inline_beep_xy $40, $40
	tax
	dex
	bne type_beep

	tsx					; fetch the bit number
	txa
	cmp #$FF
	beq beeploop		; continuous beeping for MB error
bit_beep:
	tax
	inline_delay_cycles_ay 400000
	txa
	sta TXTCLR 			; turn on graphics
	inline_beep_xy $FF, $80
	sta TXTSET			; text mode
	sec
	sbc #1
	bne bit_beep

	; pause betwen beeping ~1.5 sec
	ldx #3
dl2:	inline_delay_cycles_ay 500000
	dex
	bne dl2

	JMP beeploop

bad_page_msg:
	.asc	"PAGE ERR", 0

page_ok:

; ************
; *** data ***
; ************
tst_tbl:
	.byt	$80,$40,$20,$10, $08,$04,$02,$01,$00,$FF,$A5,$5A 
tst_tbl_end:
