; march-U test for ShadowRAM in Durango home retrocomputers!
; (c) 2025 Carlos J. Santisteban
; based on https://github.com/misterblack1/appleII_deadtest/
; last modified 20250708-1213

; xa shadow_m.s
; add -DPOCKET for non-cartridge version

; *** macros ***
#define	BEEP(l,p)	LDX#l:LDY#p:DEY:NOP:NOP:BNE *-3:DEX:STX IOBeep:BNE *-11
#define DELAY(c)	LDA#>(c/9):LDY#<(c/9):CPY#1:DEY:SBC#0:BCS *-5
#define PRINT(m,d)	LDY#0:LDA m,Y:BEQ *+32:ASL:ASL:TAX:LDA font,X:STA d,Y:LDA font+1,X:STA d+32,Y:LDA font+2,X:STA d+64,Y:LDA font+3,X:STA d+96,Y:INY:BNE *-33
#define CHAR_Y(d)	ASL:ASL:TAX:LDA font,X:STA d,Y:LDA font+1,X:STA d+32,Y:LDA font+2,X:STA d+64,Y:LDA font+3,X:STA d+96,Y
#define	CHECKBAD	BEQ *+5:JSR markbad

; *** hardware definitions ***
IO8mode	=	$DF80
IOAie	=	$DFA0
IOBeep	=	$DFB0
IOCart	=	$DFC0

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
con_str		= con_loc+2		; temporary pointer as well
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
dest_exe	= $0800			; destination address for ROM-to-RAM
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
	.asc	"ShadowRAM marchtest", 0	; C-string with filename @ [8]
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
	.word	$6180			; time, 12.12		0110 0-001 100-0 0000
	.word	$5AE8			; date, 2025/7/8	0101 101-0 111-0 1000
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
	TXS
; Durango specific stuff
	STX IOAie				; turn error LED off
	LDA #%10110000			; HIRES mode as usual
	STA IO8mode
; *** Durango-X specifics, init vectored iterrupts, just in case ***
	LDX #>reset
	LDY #<reset
	STY nmi_ptr				; NMI button will reset the test, not the bootloader
	STX nmi_ptr+1
	LDX #>null
	LDY #<null
	STY irq_ptr				; IRQ will do nothing, for extra safety
	STX irq_ptr+1

#ifndef	POCKET
; ROM code must be copied and executed from standard RAM
.(
pay_hi	= >pay_end
	LDX #>dest_exe
	LDY #<dest_exe			; code will run at dest_exe ($0800), Y must be zero
	STY con_str
	STX con_str+1			; use as temporary destination pointer
	LDX #>start
	LDA #<start				; not necessarily zero!
	STA mu_ptr				; but Y index will start at zero, for both pointes
page:
		STX mu_ptr+1		; update page
loop:
			LDA (mu_ptr), Y	; get source
			STA (con_str), Y; write into destination
			INY
			BNE loop
		INC con_str+1		; next page
		INX					; on both indexes
		CPX #pay_hi+1		; check whether last page is over
		BNE page			; copying a few extra bytes won't matter
	JMP dest_exe			; *** launch rest of code from RAM ***
.)
start:						; payload starts here
* =		dest_exe			; now into RAM addressing space
#endif
; *******************************************************************************
; *** let's assume ZP, stack and normal RAM are fine, let's go for the Shadow ***
; *******************************************************************************
; *** switch into ShadowRAM, ROM is no longer needed
	LDA #%01011100			; ROM disabled, UNprotected RAM, and SD disabled just in case
	STA IOCart				; switch into ShadowRAM!

 	JSR count_ram			; count how much ShadowRAM is installed -- usually 32 KiB
	JSR show_banner			; init screen

; some beep
	BEEP($20,$C0)
.(
page_test:
		; ldx #$F0			; simulate error
		; jmp page_error

	LDA #0					; write zeros to first shadow page
	TAY
wz:
		STA $8000,Y
		DEY
		BNE wz

wr:
		STA $8100,Y			; write to the pages
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $8200,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $8400,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $8800,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $9000,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $A000,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		STA $C000,Y
		LDX $8000,Y			; check the zp address
		BNE page_error
		INY
		BNE wr
	JMP page_ok
.)
page_error:
.(
	; TAX					; bad bit mask is in A, save it to X *** actually was in X
	STX con_xsave			; then save it *** now we should have available RAM, don't mess with stack

	PRINT(bad_page_msg,$7040)

	LDA con_xsave			; retrieve the test value *** now from RAM
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

; find the bit to beep out
	LDA con_xsave			; get the bad bit mask back into A ***
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
	STX con_xsave			; save the bit index of the top set bit ***
beeploop:
		LDX #5
type_beep:					; beep an annoying chirp to indicate page err
			DELAY(105000)
			TXA
			BEEP($40,$40)
			TAX
			DEX
			BNE type_beep
	
		LDA con_xsave		; fetch the bit number ***
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
; count useable RAM *** complete revamp for Durango-X ShadowRAM
count_ram:
.(
	LDA #$80				; first possible page
	LDY #$7F				; safe offset
	STY mu_ptr
page:
		STA mu_ptr+1		; select page
		STA (mu_ptr)		; store page number (all CMOS)
		INC					; try next page
		BNE page
	LDA mu_ptr+1			; this should be first shadow page (upper mirroring)
	STA mu_page_st			; start page
	STZ mu_page_end			; ShadowRAM "end"
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

		LDX mu_test_idx		; get the index to the test value pages
		LDA tst_tbl,X		; get the test value into A
		TAX					; X will contain the test val throughout marchU

		LDY #31				; *** write value pattern on some line
mul:
			STA $6FE0, Y
			DEY
			BPL mul


		LDA mu_page_st
		STA mu_ptr+1
		LDY #$00			; Y will be the pointer into the page

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
	
		INC mu_ptr+1		; increment the page *** up to zero
;		LDA mu_ptr+1
;		CMP mu_page_end		; compare with (one page past) the last page
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
	
		INC mu_ptr+1		; increment the page *** up to zero
;		LDA mu_ptr+1
;		CMP mu_page_end		; compare with (one page past) the last page
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
	
		INC mu_ptr+1		; increment the page *** up to zero
;		LDA mu_ptr+1
;		CMP mu_page_end		; compare with (one page past) the last page
			BNE step2		; if not there yet, loop again
	
; step 3; down - r1,w0,r0,w1
		LDA mu_page_end
  		DEC					; start at the end page minus one
		STA mu_ptr+1

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
  		DEC					; start at the end page minus one
		STA mu_ptr+1
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
	STA con_asave			; *** needs to remove A15 from address
	LDA mu_ptr+1			; get the page number as index into results array ***
	AND #$7F				; *** remove topmost bit from index
	TAY
	LDA con_asave			; *** retrieve A, using Y as index
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
		ORA #$80			; *** these in are ROM address space
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
		EOR #$80			; *** ROM address space (if reaches $80 will turn into zero)
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
	PRINT(ban1txt,$7D45)
	PRINT(ban2txt,$7E40)
	PRINT(ban3txt,$7F44)
	LDX #$7F
	LDY #$51				; position of start page on display
	STY con_loc
	STX con_loc+1
	LDA mu_page_st			; no need to subtract from start page
	ORA #$80				; *** into ROM address space
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
ban1txt:
	.asc	"DURANGO-X SHADOW TEST", 0
ban2txt:
	.asc	"BY ZUIKO21, KI3V & ADRIAN BLACK", 0
ban3txt:
	.asc	"TESTING RAM $8000-$FFFF", 0
page_head:
	.asc	"PAGE", 0

; *** 8x4 font (ASCII 32-95) for quick inline display ***
font:
#include	"8x4font.s"

#ifndef	POCKET
pay_end:					; end of payload
* =		start+pay_end-dest_exe		; recompute current address into ROM space

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
