;	@com.wudsn.ide.asm.mainsourcefile=_boot.s



//
//
//

.proc SetUpDisplay
	jsr WaitForSync
	jsr set_colours			; set up the colors
	mva #0 NMIEN
	sei
	mwa #DisplayList SDLSTL
	mwa #DLI VDSLST
	mwa VVBLKI OSVBI
	mwa #VBI VVBLKI
	mva #$40 NMIEN			; DLIs disabled for the moment
	ldx #MenuLines+1
	lda #$10
@
	sta ColourTable-1,x
	dex
	bne @-
	cli
	rts	
	.endp
	
	
.proc	set_colours
	mva #$10 color2		; background
	mva #$0E color1		; foreground (luma)
	mva #$10 color4		; border
	rts
	.endp
	
	
	
.proc	DLI
	pha
	txa
	pha
	ldx DLICount
	lda ColourTable,x
	sta wsync
	sta ColPf2
	inc DLICount
	pla
	tax
	pla
	rti
	.endp


//
//	VBI Handler
//

	.local VBI
	sta NMIRES
	mva #$0 DLICount	; sync DLIs
	lda Timer		; maintain joystick debounce timer
	beq @+
	dec Timer
@
	jmp (OSVBI)
	.endl



//
//	Clear screen
//

.proc clear_screen
	ldy #0
	tya
Loop
	sta FrameBuffer,y
	sta FrameBuffer+$0100,y
	sta FrameBuffer+$0200,y
	sta FrameBuffer+$0300,y
	iny
	bne Loop
	sta cx			; reset screen coordinates
	sta cy
	rts
	.endp
	
	
	
//
//	Open pop-up notification window
//

.proc OpenWindow
	mva #10 cy
Loop
	ldy cy
	lda LineTable.Lo,y
	sta ScrPtr
	lda LineTable.Hi,y
	sta ScrPtr+1
	ldy #6
	lda #$80
@
	sta (ScrPtr),y
	iny
	cpy #34
	bcc @-
	inc cy
	lda cy
	cmp #14
	bcc Loop
	mva #$80 RevFlag	; set up reverse video printing
	mva #11 cy
	mva #8 cx
	rts
	.endp


//
//	Pad to end of line
//

.proc PadLine
	lda #32
Loop
	ldx cx
	cpx #40
	bcs Done
	jsr PutChar
	jmp Loop
Done
	mva #0 RevFlag
	rts
	.endp



//
//	Convert ASCII to internal
//
	
	.proc AStoIN
	cmp #$60
	bcc @+
	rts
@
	cmp #$20
	bcs @+
	adc #$40
	rts
@
	sbc #$20
	rts
	.endp
	


//
//	Put character to the screen
//	X, Y and A are all preserved
//

	.proc PutChar
	pha		; save character
	sty tmp1	; and Y
	jsr ASToIN	; convert to internal code
	eor RevFlag	; handle selection bit
	pha
	ldy cy
	lda LineTable.Lo,y
	sta ScrPtr
	lda LineTable.Hi,y
	sta ScrPtr+1
	ldy cx
	pla
	sta (ScrPtr),y
	inc cx
	ldy tmp1	; restore Y
	pla		; and A
	rts
	.endp




//
//	Put 32 byte filename
//

	.proc PutFilename
	stax text_out_ptr
	ldx #31
	ldy #0
Loop
	lda (text_out_ptr),y
	beq Done
	jsr PutChar
	iny
	dex
	bne Loop
Done
	rts
	.endp

//
//	Put String (pass string address in A,X)
//

	.proc PutString
	stax text_out_ptr
	ldy #0
Loop
	lda (text_out_ptr),y
	beq Done
	jsr PutChar
	iny
	bne Loop
Done
	rts
	.endp
	

//
//	Table of line addresses
//
	
	.proc LineTable
Lo
?Address = FrameBuffer
	.rept 24
	.byte <?Address
?Address = ?Address + $28
	.endr

Hi
?Address = FrameBuffer
	.rept 24
	.byte >?Address
?Address = ?Address + $28
	.endr
	.endp
	
	
	
	.if 0
	
// Player missile setup


	.local InitPMGs
	mva #>PMBuffer PMBase
	
	lda #0 ; clear pmg memory
	ldy #127
@
	sta MissileData,y
	sta Player0Data,y
	sta Player1Data,y
	sta Player2Data,y
	sta Player3Data,y
	dey
	bpl @-
	
	ldy PMGStart
	ldx #3
	lda #255 ; create bitmaps
@
	sta MissileData,y
	sta Player0Data,y
	sta Player1Data,y
	sta Player2Data,y
	sta Player3Data,y
	iny
	dex
	bpl @-
	
	ldy #28
@
	lda PMGData,y
	sta HPosP0,y
	dey
	bpl @-
	
	mva #$2E sdmctl
	mva #1+32 gprior
	mva #$03 gractl
	
	mva #0 Color3
	
	lda $14
	clc
	adc #8
@
	cmp $14
	bcc @-
	rts
	.endl

	
	
	.local DisablePMGs
	mva #34 sdmctl
	lda #0
	sta gractl
	ldy #$0c
@
	sta $D000,y
	dey
	bpl @-
	jmp WaitForSync
	.endl
	
	
PMGStart
	.byte 108
	
	
PMGData ; 28 bytes of PMG setup data
	.byte $30,$50,$70,$90,$B0,$B8,$C0,$C8 ; player/missile horizontal positions
	.byte $03,$03,$03,$03,$FF ; player/missile widths
	.byte $00,$00,$00,$00,$00 ; graphics shapes (unused)
	.byte $00,$00,$00,$00,$00,$00,$00,$00 ; player/missile colours
	.byte $00 ; colbak
	.byte $05 ; priority
VDelayVal
	.byte $00 ; vdelay

	.endif


//
//	Display List
//

DisplayList
	.rept 2
	.byte DL.Blank8
	.endr
	
	.byte DL.ModeF+DL.LMS
	.word Logo
	
	.rept 18
	.byte DL.ModeF
	.endr
	
	.byte DL.Mode2+DL.LMS+DL.NMI
	.word FrameBuffer
	
	.rept 21 ; 23
	.byte DL.Mode2+DL.NMI
	.endr
	
	.byte DL.VBL
	.word DisplayList
	
	
Logo
	ins 'logo.bin'
	

	