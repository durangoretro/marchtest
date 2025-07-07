; march-U test for Durango home retrocomputers!
; (c) 2025 Carlos J. Santisteban
; based on https://github.com/misterblack1/appleII_deadtest/
; last modified 20250707-1447

; xa march.s
; add -DPOCKET for non-cartridge, standard RAM version
; add -DSAFE to make sure NOT to use stack EVER

; *** macros ***
#define	BEEP(l,p)	LDX#l:LDY#p:DEY:NOP:NOP:BNE *-3:DEX:STX IOBeep:BNE *-11
#define DELAY(c)	LDA#>(c/9):LDY#<(c/9):CPY#1:DEY:SBC#0:BCS *-5
#define PRINT(m,d)	LDY#0:LDA m,Y:BEQ *+32:ASL:ASL:TAX:LDA font,X:STA d,Y:LDA font+1,X:STA d+32,Y:LDA font+2,X:STA d+64,Y:LDA font+3,X:STA d+96,Y:INY:BNE *-33
#define CHAR(d)		ASL:ASL:TAX:LDA font,X:STA d:LDA font+1,X:STA d+32:LDA font+2,X:STA d+64:LDA font+3,X:STA d+96
#define CHAR_Y(d)	ASL:ASL:TAX:LDA font,X:STA d,Y:LDA font+1,X:STA d+32,Y:LDA font+2,X:STA d+64,Y:LDA font+3,X:STA d+96,Y
#define	CHECKBAD	BEQ *+5:JSR markbad

; *** hardware definitions ***
IO8mode	=	$DF80
IOAie	=	$DFA0
IOBeep	=	$DFB0

; *** memory usage ***
mu_ptr	=	$03				; temporary pointer, 6510 & minimOS-savvy
count	=	mu_ptr+1		; AKA mu_ptr+1
mu_page_end	= count+1
mu_page_st	= mu_page_end+1
mu_test_idx	= mu_page_st+1
mu_ysave	= mu_test_idx+1
all_errs	= mu_ysave+1
scratch		= all_errs+1
results		= scratch+1
con_loc		= results+128	; 32*4, as this has no more than 32KiB
con_str		= con_loc+2
con_xsave	= con_str+2
con_ysave	= con_xsave+1
con_asave	= con_ysave+1
_end_zp		= con_asave+1
irq_ptr		= $0200
nmi_ptr		= $0202

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
	.word	$1043			; 1.0b3		%vvvvrrrr sshhbbbb, where revision = %hhrrrr, ss = %00 (alpha), %01 (beta), %10 (RC), %11 (final)
; date & time in MS-DOS format at byte 248 ($F8)
	.word	$7600			; time, 14.40		0111 0-110 000-0 0000
	.word	$5AE7			; date, 2025/7/7	0101 101-0 111-0 0111
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
	TXS						; is this needed?
; Durango specific stuff
	STX IOAie				; turn error LED off
	LDA #%10110000			; HIRES mode as usual
	STA IO8mode
; some beep
	BEEP($20,$C0)
; make a clear strip for pre-test progress
	LDX #0					; not using STZ
	TXA
clear:
		STA $6F00, X
		STA $7000, X		; about middle of the screen
		INX
		BNE clear			; 256 bytes are 8 rasters on HIRES mode
; ******************************************************************
; *** at this point we don't trust stack or ZP, test these first ***
; ******************************************************************
	PRINT(zp_msg,$6F4B)
.(
start:	
	LDX #(tst_tbl_end-tst_tbl-1)	; initialize the pointer to the table of values

; step 0; up - w0 - write the test value
marchU:	
		LDA tst_tbl,X		; get the test value into A
		TXS					; save the index of the test value into SP
		TAX					; save the test value into X

		LDY #31				; write value pattern on some line
mul:
			STA $6FE0, Y
			DEY
			BPL mul

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
marchU1delay:
		DELAY(35000)
		LDY #$00			; reset Y to 0

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
marchU2delay:
		DELAY(35000)

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
		BNE marchup			;  again with next value

	JMP zp_good				; *** ZP/stack test ended ***

marchup:
		JMP marchU			; because of distance...
.)
zp_error:
.(
	TAX						; bat bit mask is in A, save it to X
	TXS  					; then save it in the SP

	PRINT(bad_msg,$7040)
 
	TSX						; retrieve the test value
	TXA

#ifdef	SAFE
; printing the bits must be inlined, as we ran out of registers
	ASL						; get top bit into carry flag
	TAY						; save the current value
	LDA #'0'
	ADC #0					; increment by one if we had a carry
	CHAR($704A)				; print bit to screen, note position
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704B)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704C)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704D)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704E)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704F)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($7050)
	TYA
	ASL						; same for all 8 bits
;	TAY						; no need for last save
	LDA #'0'
	ADC #0
	CHAR($7051)
;	TYA
#else
; otherwise try to stack A and hope for the best!
	LDY #0
print_bit:
		ASL					; get top bit into carry flag
		PHA					; save the current value, hopefully
		LDA #'0'
		ADC #0				; increment by one if we had a carry
		CHAR_Y($704A)		; print bit to screen
		PLA
		INY
		CPY #8
		BNE print_bit		; repeat 8 times
#endif

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
	LDA #0:STA IOAie		; *** turn LED on as error display (NMOS-savvy)
	JMP *					; only get here if there was no bad bit

; now X contains the index of the bit, ing at 1
start_beeping:
	TXS						; save the bit index of the top set bit into SP
beeploop:
		LDA #1
type_beep:					; beep an annoying chirp to indicate page err
			BEEP($FF,$FF)
			SEC
			SBC #1
			BPL type_beep

		TSX					; fetch the bit number
		TXA
bit_beep:
			TAX
			DELAY(466667)
			DELAY(466667)
			DELAY(466667)	; 4-second delay
			TXA
;			sta TXTCLR 		; turn on graphics***
			BEEP($FF,$80)
;			sta TXTSET		; text mode***
			SEC
			SBC #1
			BNE bit_beep

; pause betwen beeping ~1.5 sec
		LDX #10
dl:
			DELAY(525000)
			DEX
			BNE dl
		JMP beeploop
.)

zp_good:
.(
page_test:
		; ldx #$F0			; simulate error
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
.)
page_error:
.(
	; TAX					; bad bit mask is in A, save it to X
	TXS  					; then save it in the SP

	PRINT(bad_page_msg,$7040)

	TSX						; retrieve the test value
	TXA
#ifdef	SAFE
; printing the bits must be inlined, as we ran out of registers
	ASL						; get top bit into carry flag
	TAY						; save the current value
	LDA #'0'
	ADC #0					; increment by one if we had a carry
	CHAR($7049)				; print bit to screen, note position
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704A)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704B)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704C)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704D)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704E)
	TYA
	ASL						; same for all 8 bits
	TAY
	LDA #'0'
	ADC #0
	CHAR($704F)
	TYA
	ASL						; same for all 8 bits
;	TAY						; no need for last save
	LDA #'0'
	ADC #0
	CHAR($7050)
;	TYA
#else
; otherwise try to stack A and hope for the best!
	LDY #0
print_bit:
		ASL					; get top bit into carry flag
		PHA					; save the current value, hopefully
		LDA #'0'
		ADC #0				; increment by one if we had a carry
		CHAR_Y($7049)		; print bit to screen
		PLA
		INY
		CPY #8
		BNE print_bit		; repeat 8 times
#endif

; find the bit to beep out
	TSX						; get the bad bit mask back into A
	TXA
	CMP #$FF				; if it's FF, it's a motherboard error
	BEQ start_beeping
	LDX #1					; count up
page_chkbit:	
		LSR					; move lowest bit into carry
	BCS start_beeping		; bit set, display it
		INX					; count down
		CPX #$09
		BNE page_chkbit		; test next bit
	JMP *					; only get here if there was no bad bit

; now X contains the index of the bit, starting at 1
start_beeping:
	TXS						; save the bit index of the top set bit into SP
beeploop:
		LDX #5
type_beep:					; beep an annoying chirp to indicate page err
			DELAY(105000)
			TXA
			BEEP($40,$40)
			TAX
			DEX
			BNE type_beep
	
		TSX					; fetch the bit number
		TXA
		CMP #$FF
		BEQ beeploop		; continuous beeping for MB error
bit_beep:
		TAX
		DELAY(466667)
		DELAY(466667)
		DELAY(466667)		; 4-second delay
		TXA
;		sta TXTCLR 			; ****turn on graphics
		BEEP($FF,$80)
;		sta TXTSET			; ****text mode
		SEC
		SBC #1
		BNE bit_beep

; pause betwen beeping ~1.5 sec
	LDX #10
dl:
		DELAY(525000)
		DEX
		BNE dl

	JMP beeploop
.)

page_ok:

; *************************************************
; *** now we trust the zero page and stack page ***
; *************************************************
	LDX #$FF
	TXS						; initialize the stack pointer
; *** Durango-X specifics, init vectored iterrupts, just in case ***
	LDX #>reset
	LDY #<reset
	STY nmi_ptr				; NMI button will reset the test, not the bootloader
	STX nmi_ptr+1
	LDX #>null
	LDY #<null
	STY irq_ptr				; IRQ will do nothing, for extra safety
	STX irq_ptr+1
; *** continue with complete test ***
	JSR count_ram			; count how much RAM is installed -- all Durangos have 32 KiB, but check for POCKET version
	JSR show_banner
	PRINT(zp_ok,$7B46)

;	JSR show_charset

	LDX #$40				; cycles
	LDA #$80				; period
	JSR beep
	LDX #$80				; cycles
	LDA #$40				; period
	JSR beep

	LDA #5
	JSR delay_seconds

	JSR init_results
test_ram:
	JSR marchU				; run the test on RAM and report
	JMP test_ram			; ...forever!

; *************************
; *** business routines ***
; *************************
; count useable RAM *** complete revamp for Durango-X
count_ram:
.(
#ifdef	POCKET
	LDA #$10				; pocket might start somewhat before this...
#else
	LDA #$02
#endif
	STA mu_page_st			; start page
	LDA #$80				; standard RAM end, might change for ShadowRAM
	STA mu_page_end
	RTS
.)

; clear results array
init_results:
.(
	LDA #0
	STA all_errs
	TAY
lp:
		STA results,Y
		INY
		BPL lp				; *** not BNE!
	RTS
.)

; full test!
marchU:
.(
;	sta TXTCLR			; use graphics
;	sta HIRES 			; set high res
;	sta MIXSET			; mixed mode on

;	LDA #FIRST_PAGE			; set starting address (maybe change later to a parameter?)
;	STA mu_page_st			; *** already done by count_ram ***

	LDA #(tst_tbl_end-tst_tbl-1) ; number of test values
	STA mu_test_idx

init:	
		LDA #0				; low bits 0 so we test a hardware page at a time
		STA mu_ptr
		LDY #$00			; Y will be the pointer into the page
		LDX mu_test_idx		; get the index to the test value pages
		LDA tst_tbl,X		; get the test value into A
		TAX					; X will contain the test val throughout marchU
		LDA mu_page_st
		STA mu_ptr+1
	
		; lda #08
		; sta results+$19
		; lda #01
		; sta results+$A1
		; sta all_errs
		; jmp show_report	; simulate a run with canned errors
	
; In the descriptions below:
;	up 		perform the test from low addresses to high ones
; 	down	perform the test from high addresses to low ones
;	r0		read the current location, compare to the test value, fail if different
;	r1		read the current location, compare to the inverted test value, fail if different
;	w0		write the test value to current location
;	w1		write the inverted test value to current location
	
; step 0; up - w0 - write the test value
step0:	
			TXA				; get the test value
			STA (mu_ptr),Y	; w0 - write the test value to current location
			INY				; count up
			BNE step0		; repeat until Y overflows back to zero (do the whole page)
	
		INC mu_ptr+1		; increment the page
		LDA mu_ptr+1
		CMP mu_page_end		; compare with (one page past) the last page
		BNE step0			; if not there yet, loop again
	
;	LDA #$08				; simulate error
;	JMP bad
	
; step 1; up - r0,w1,r1,w0
		LDA mu_page_st		; set up the starting page again for next stage
		STA mu_ptr+1
step1:	
			TXA				; get the test value
			EOR (mu_ptr),Y	; r0 - read and compare with test value (by XOR'ing with accumulator)
			CHECKBAD
			TXA				; get the test value
			EOR #$FF		; invert
			STA (mu_ptr),Y	; w1 - write the inverted test value
			EOR (mu_ptr),Y	; r1 - read the same value back and compare using XOR
			CHECKBAD
			TXA				; get the test value
			STA (mu_ptr),Y	; w0 - write the test value to the memory location
			INY				; count up
			BNE step1		; repeat until Y overflows back to zero
	
		INC mu_ptr+1		; increment the page
		LDA mu_ptr+1
		CMP mu_page_end		; compare with (one page past) the last page
			BNE step1		; if not there yet, loop again
	
; step 2; up - r0,w1
		LDA mu_page_st	; set up the starting page again for next stage
		STA mu_ptr+1
step2:	
			TXA				; get the test value
			EOR (mu_ptr),Y	; r0 - read and compare with test value (by XOR'ing with accumulator)
			CHECKBAD
			TXA				; get the test value
			EOR #$FF		; invert
			STA (mu_ptr),Y	; w1 - write the inverted test value
			INY				; count up
			BNE step2		; repeat until Y overflows back to zero
	
		INC mu_ptr+1		; increment the page
		LDA mu_ptr+1
		CMP mu_page_end		; compare with (one page past) the last page
			BNE step2		; if not there yet, loop again
	
; step 3; down - r1,w0,r0,w1
		LDA mu_page_end
		STA mu_ptr+1
		DEC mu_ptr+1		; start at the end page minus one

  		LDY #$FF			; start at FF and count down
step3:	
			TXA				; get the test value
			EOR #$FF		; invert
			EOR (mu_ptr),Y	; r1 - read and compare with inverted test value (by XOR'ing with accumulator)
			CHECKBAD
			TXA				; get the test value
			STA (mu_ptr),Y	; w0 - write the test value
			EOR (mu_ptr),Y	; r0 - read the same value back and compare using XOR
			; BNE bad			; if bits differ, location is bad
			CHECKBAD
			TXA				; get the test value
			EOR #$FF		; invert
			STA (mu_ptr),Y	; w1 - write the inverted test value
			DEY				; determine if we are at offset zero
			CPY #$FF		; did we wrap around?
			BNE step3		; repeat until Y overflows back to FF
	
		DEC mu_ptr+1		; decrement the page
		LDA mu_ptr+1
		CMP mu_page_st		; compare with the first page, which can't be zero
			BCS step3		; if not there yet (mu_ptr+1>=mu_page_st so carry set), loop again
	
; step 4; down - r1,w0
		LDA mu_page_end
		STA mu_ptr+1
		DEC mu_ptr+1		; start at the end page minus one
step4:	
			TXA				; get the test value
			EOR #$FF		; invert
			EOR (mu_ptr),Y	; r1 - read and compare with inverted test value (by XOR'ing with accumulator)
			CHECKBAD
			TXA				; get the test value
			STA (mu_ptr),Y	; w0 - write the test value
			DEY				; determine if we are at offset zero
			CPY #$FF		; did we wrap around?
			BNE step4		; repeat until Y overflows back to FF	
		DEC mu_ptr+1		; decrement the page
		LDA mu_ptr+1
		CMP mu_page_st		; compare with the first page, which can't be zero
			BCS step4		; if not there yet (mu_ptr+1>=mu_page_st so carry set), loop again
	
; now, determine whether to repeat with a new test value
		LDX mu_test_idx
		DEX
		STX mu_test_idx
	
	BMI show_report			; we're done with all values, so show results
	
		JMP init			; else go to next test value
.)

; mark this page as faulty
markbad:
.(
	STY mu_ysave
	LDY mu_ptr+1			; get the page number as index into results array
	ORA results,Y			; collect any bad bits
	STA results,Y			; store the accumulated errors back to the results array
	ORA all_errs			; also store one value that collects all of the bad bits found
	STA all_errs
	LDY mu_ysave
	RTS
.)

; display test results
show_report:
.(
; *** Durango-X specifics, init vectored iterrupts, just in case ***
	LDX #>reset
	LDY #<reset
	STY nmi_ptr				; NMI button will reset the test, not the bootloader
	STX nmi_ptr+1
	LDX #>null
	LDY #<null
	STY irq_ptr				; IRQ will do nothing, for extra safety
	STX irq_ptr+1
; ***
;	sta TXTSET 		; turn on text
	JSR show_banner
;	puts_at 1,0, "GITHUB.COM/MISTERBLACK1/APPLEII_DEADTEST"
	PRINT(page_head,$6240)

	LDX #15
next_head_line:
		TXA					; go to the correct line
		CLC
		ADC #4				; start on this line
		TAY
		LDA #0
		JSR con_goto
		TXA
		STX con_xsave		; *** hope this works!
		JSR con_put_hex
		LDA #'_'
		JSR dx_display
		INC con_loc
		INC con_loc
		LDA #':'
  		JSR dx_display
		LDX con_xsave		; *** must retrieve!
		DEX
		BPL next_head_line

	LDX #0
next_page:
		TXA					; calculate the column
		LSR					; get the high nybble
		LSR
		LSR
		LSR
		TAY					; get the column offset from table
		LDA columns,Y
		PHA					; save the column number on the stack
		
		LDY #2				; print the heading row
		JSR con_goto
		TXA
		STX con_xsave		; *** hope this works!
		JSR con_put_hex
		INC con_loc
		LDA #'_'
		JSR dx_display
	
		LDX con_xsave		; *** must retrieve!
		TXA					; calculate the line number on the screen
		AND #$0F			; 16 lines of results
		CLC
		ADC #4				; offset by starting line
		TAY					; put line into Y
	
		PLA					; retrieve column into A
		JSR con_goto		; move to that location on screen
	
		LDA results,X		; get the value to print
		STX con_xsave		; *** hope this works!
		BNE	hex				; see if there's an error
			LDA #'-'		; if not, print dashes
			JSR dx_display
			INC con_loc
			LDA #'-'
			JSR dx_display	; put two dashes there
		JMP next
	
hex:						; print a hex value
		JSR con_put_hex
	
next:
		LDX con_xsave		; *** must retrieve!
		INX					; look for the next page
		TXA					; compare to the last page to test
		CMP mu_page_end
		BNE next_page		; continue if there are more to print

	LDA all_errs
		BEQ good
	JSR beep_bad
	JMP done
good:
		JSR beep_good

done:
	LDA #8
	JSR delay_seconds
	JMP marchU
.)

; ************************
; *** support routines ***
; ************************
; standard beep (A is period, X is cycles, destroys Y)
beep:
.(
outer:
		PHA
		TAY
inner:
			JSR delay
			JSR delay
			JSR delay
			DEY
			BNE inner
		STX IOBeep
		PLA
		DEX
		BNE outer
	STY IOBeep				; safer for Durango
delay:
	RTS
.)

delay_seconds:
.(
;		sta KBDSTRB
;	ASL						; double the number, since we actually count in half-seconds
loop:
		PHA
		LDX #7
repeat:
			DELAY(500000)
			DEX
			BNE repeat
		PLA
		SEC
		SBC #1
		BNE loop
	RTS
.)

; clear screen (within working ZP) *** revamped for Durango-X
con_cls:
.(
	LDX #$60				; screen 3 page address
	LDY #0
	TYA						; will clear screen
	STY mu_ptr				; pointer LSB
p_loop:
		STX mu_ptr+1		; update pointer MSB
c_loop:
			STA (mu_ptr), Y
			INY
			BNE c_loop
		INX
		BPL p_loop			; just up to $8000
	RTS
.)

; display A in hex at current position, destroys ALL registers
con_put_hex:
.(
	STA con_asave
	LSR						; shift high nybble into low
	LSR
	LSR
	LSR
	TAY						; use as index
	LDA hex_tbl, Y			; into the hex table
	JSR dx_display			; show char in A at current poition
	LDA con_asave			; get another copy
	AND #$0F				; get low nybble
	TAY						; as index
	LDA hex_tbl, Y			; into table
	INC con_loc				; advance to next position (supposedly no wrap expected)
	JSR dx_display			; call, a bit less efficient
	DEC con_loc				; *** needs to be this way for compatibility
	RTS
.)
; display char in A at current position *** new, destroys ALL registers
dx_display:
.(
	ASL
	ASL						; inject ASCII into 64-char font
 	ORA #3					; go for bottom raster first!
	TAX						; as index into 8x4 font
	LDY #96					; bottom raster offset
con_dloop:
 		LDA font, X			; get font data
		STA (con_loc), Y	; store on screen
		DEX
		TYA
		SEC
		SBC #32				; one raster up
		TAY
		BPL con_dloop
	RTS
.)

; set con_loc from A=column, Y=row, doesn't touch X *** revamped for Durango-X
con_goto:
.(
	CLC
	ADC #$40				; raster offset
	STA con_loc				; LSB is ready
	TYA						; this is row
	CLC
	ADC #$60				; screen 3 base
	STA con_loc+1			; MSB is ready, and we're done!
	RTS
.)

; display banner
show_banner:
.(
	JSR con_cls
	PRINT(ban1txt,$7D46)
	PRINT(ban2txt,$7E40)
	PRINT(ban3txt,$7F44)
	LDX #$7F
	LDY #$51				; position of start page on display
	STY con_loc
	STX con_loc+1
	LDA mu_page_st			; no need to subtract from start page
	JSR con_put_hex
	RTS
.)

; tune when error
beep_bad:
.(
	LDX #$20				; cycles
	LDA #$80				; period
	JSR beep
	LDX #$FF				; cycles
	LDA #$FF				; period
	JSR beep
	LDX #$FF				; cycles
	LDA #$FF				; period
	JSR beep
	RTS
.)

; tune when OK
beep_good:
.(
	LDX #$20				; cycles
	LDA #$80				; period
	JSR beep
	LDX #$40				; cycles
	LDA #$40				; period
	JSR beep
	LDX #$00				; cycles
	LDA #$20				; period
	JSR beep
	RTS
.)

; *** Durango-X specific, just in case an IRQ is executed ***
null:
	RTI

; ************
; *** data ***
; ************
tst_tbl:
	.byt	$80,$40,$20,$10, $08,$04,$02,$01,$00,$FF,$A5,$5A 
tst_tbl_end:
hex_tbl:
	.asc	"0123456789ABCDEF"
columns:
	.byt	5, 8, 11, 14, 17, 20, 23, 26

; *** bad ZP/SP and other messages ***
bad_msg:
	.asc	"ZP/SP ERR", 0
bad_msg_len = * - bad_msg
bad_page_msg:
	.asc	"PAGE ERR", 0
zp_msg:
	.asc	"TEST ZERO PAGE", 0
zp_ok:
	.asc	"ZERO/STACK PAGES OK", 0
ban1txt:
	.asc	"DURANGO-X DEAD TEST", 0
ban2txt:
	.asc	"BY ZUIKO21, KI3V & ADRIAN BLACK", 0
ban3txt:
	.asc	"TESTING RAM $0200-$7FFF", 0
page_head:
	.asc	"PAGE", 0

; *** 8x4 font (ASCII 32-95) for quick inline display ***
; notice MOD 64!
font:
	.byt	24, 44, 32, 24,		24, 36, 60, 36,		56, 36, 56, 60,		28, 32, 32, 28	; @ABC
	.byt	56, 36, 36, 56,		60, 32, 56, 60,		60, 32, 56, 32,		24, 32, 44, 56	; DEFG
	.byt	36, 36, 60, 36,		28, 8, 8, 28,		4, 4, 36, 24,		32, 40, 48, 40	; HIJK
	.byt	32, 32, 32, 60,		34, 54, 42, 34,		36, 52, 44, 36,		60, 36, 36, 60	; LMNO
	.byt	56, 36, 56, 32,		24, 36, 36, 26,		56, 36, 56, 36,		28, 32, 28, 56	; PQRS
	.byt	62, 8, 8, 8,		36, 36, 36, 24,		34, 34, 20, 8,		34, 34, 42, 20	; TUVW
	.byt	36, 24, 24, 36,		34, 20, 8, 16,		60, 8, 16, 60,		24, 16, 16, 24	; XYZ[
	.byt	32, 16, 8, 4,		24, 8, 8, 24,		8, 20, 0, 0,		0, 0, 0, 255	; \] caret _
	.byt	0, 0, 0, 0,			8, 8, 0, 8,			20, 20, 0, 0,		36, 126, 36,126	;  !"#
	.byt	8, 30, 60, 8,		36, 8, 16, 36,		48, 42, 68, 122,	8, 8, 0, 0		; $%&'
	.byt	8, 16, 16, 8,		16, 8, 8, 16,		8, 42, 28, 42,		0, 8, 28, 8		; ()*+
	.byt	0, 0, 8, 16,		0, 0, 28, 0,		0, 0, 0, 8,			4, 8, 16, 32	; ,-./
	.byt	24, 36, 36, 24,		8, 24, 8, 28,		24, 36, 8, 30,		60, 4, 24, 60	; 0123
	.byt	8, 24, 56, 8,		60, 32, 28, 56,		28, 32, 62, 28,		60, 8, 16, 32	; 4567
	.byt	24, 36, 24, 60,		24, 60, 4, 56,		0, 8, 0, 8,			8, 0, 8, 16 	; 89:;
	.byt	0, 8, 16, 8,		0, 60, 0, 60,		0, 16, 8, 16,		24, 36, 8, 8	; <=>?

#ifndef	POCKET
; ***************************
; *** ROM padding and end ***
; ***************************
	.dsb	$FFD6-*, $FF	; ROM fill
; standard ROM tail
	.asc	"DmOS"			; minimOS-compliant signature
; interrupt handlers fit here
irq_hndl:
	JMP (irq_ptr)			; standard IRQ handler @ $FFDA
nmi_hndl:
	JMP (nmi_ptr)			; standard NMI handler @ $FFDD
	.byt	$FF				; some padding
switch:
	JMP ($FFFC)				; devCart switching support $FFE1

	.dsb	$FFFA-*, $FF	; ROM fill, not using checksum
; 6502 hardware vectors
	.word	nmi_hndl		; NMI as warm reset
	.word	reset
	.word	irq_hndl
#endif
file_end:					; should be $10000 for ROM images
