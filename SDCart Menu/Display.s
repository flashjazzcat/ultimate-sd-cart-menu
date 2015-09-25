;	@com.wudsn.ide.asm.mainsourcefile=_boot.s



//
//
//

.proc SetUpDisplay
	jsr WaitForSync
	jsr set_colours			; set up the colors
	mva #0 NMIEN
	sei
	mva #>FontData CHBAS
	mwa #DisplayList SDLSTL
	mwa #DLI VDSLST
	mva #0 DLICount
	mwa VVBLKI OSVBI
	mwa #VBI VVBLKI
	mva #$C0 NMIEN
	ldx #MenuLines+2
	lda #$0E
@
	sta ColourTable,x
	dex
	bpl @-
	mva #10 ColourTable+20
	cli
	rts	
	.endp
	
	
.proc	set_colours
	mva #148 color2		; background
	mva #10 color1		; foreground (luma)
	mva #148 color4		; border
	rts
	.endp
	
	
	
.proc	DLI
DLI0
	pha
	lda #158
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	lda #130
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	lda #218
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
;	sta wsync
	lda #148
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	jmp Exit
DLI1
	pha
	lda #218
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
;	sta wsync
	lda #130
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	lda #158
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	lda #148
	eor colrsh
	and drkmsk
	sta wsync
	sta ColBak
	lda ColourTable
	eor colrsh
	and drkmsk
	sta ColPf1	
	jmp Exit


	
DLI2
DLI3
DLI4
DLI5
DLI6
DLI7
DLI8
DLI9
DLI10
DLI11
DLI12
DLI13
DLI14
DLI15
DLI16
DLI17
DLI18
DLI19
DLI20
DLI21
DLI22
DLI23
DLI24
DLI25
	pha
	txa
	pha
	ldx DLICount
	lda ColourTable,x
	sta wsync
	eor colrsh
	and drkmsk
	sta ColPf1
	jmp Exit2
	
Exit
	txa
	pha
Exit2
	inc DLICount
	ldx DLICount
	lda StateTableLo,x
	sta VDSLST
	lda StateTableHi,x
	sta VDSLST+1
	
	pla
	tax
	pla
	rti
	
StateTableLo
	.byte <DLI0, <DLI1, <DLI2, <DLI3, <DLI4, <DLI5
	.byte <DLI6, <DLI7
	.byte <DLI8, <DLI9
	.byte <DLI10, <DLI11, <DLI12, <DLI13, <DLI14, <DLI15, <DLI16, <DLI17, <DLI18, <DLI19, <DLI20, <DLI21, <DLI22, <DLI23, <DLI24, <DLI25
StateTableHi
	.byte >DLI0, >DLI1, >DLI2, >DLI3, >DLI4, >DLI5
	.byte >DLI6, >DLI7
	.byte >DLI8, >DLI9
	.byte >DLI10, >DLI11, >DLI12, >DLI13, >DLI14, >DLI15, >DLI16, >DLI17, >DLI18, >DLI19, >DLI20, >DLI21, >DLI22, >DLI23, >DLI24, >DLI25
	.endp


//
//	VBI Handler
//

	.local VBI
	sta NMIRES
	mva #$0 DLICount	; sync DLIs
	mwa #DLI.DLI0 VDSLST
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
//	Pad filename
//

.proc PadFileName
	lda #32
Loop
	ldx cx
	cpx #37
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
	pha
	and #128
	sta tmp
	pla
	and #127
	cmp #$60
	bcc @+
	ora tmp
	rts
@
	cmp #$20
	bcs @+
	adc #$40
	ora tmp
	rts
@
	sbc #$20
	ora tmp
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
	adw dir_ptr #1 text_out_ptr
	ldx #127
	ldy #0
	lda (dir_ptr),y
	cmp #EntryType.Dir
	beq @+
	ldx #32
@
	txa
	jsr PutChar
NotDir
	ldy #0			; figure out length of string
	sty tmp3		; ellipsis flag
@
	lda (text_out_ptr),y
	beq FoundEOS
	iny
	cpy #31
	bcc @-
	ror tmp3		; say the name is truncated
	ldx #28
	bne ShortString
FoundEOS			; if we end up here, filename fits on the screen
	ldx #31
ShortString
	ldy #0
Loop
	lda (text_out_ptr),y
	beq Done
	jsr PutChar
	iny
	dex
	bne Loop
	bit tmp3		; are we to display an ellipsis?
	bpl Done
	ldax #txtEllipsis
	bne PutString
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
//	Display scroll indicators
//
	
	.proc DisplayScrollIndicators
	lda ULTIMATE_CART_LIST_FLAG
	pha
	ldy #' '
	and #ListFlags.FilesBefore
	seq
	ldy #28
	mva #39 cx
	mva #0 cy
	tya
	jsr PutChar
	pla
	ldy #' '
	and #ListFlags.FilesAfter
	seq
	ldy #29
	mva #39 cx
	mva #19 cy
	tya
	jmp PutChar
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
	.byte DL.Blank8
	.byte DL.Blank5+DL.NMI
	.byte DL.Blank4
		
	.byte DL.ModeF+DL.LMS
	.word Logo
	
	.rept 18
	.byte DL.ModeF
	.endr
	
	.byte DL.Blank1+DL.NMI
	
	.byte DL.Blank5
	
	.byte DL.Mode2+DL.LMS
	.word FrameBuffer

	.rept 19
	.byte DL.Mode2+DL.NMI
	.endr
	
	.byte DL.Blank2
	
	.byte DL.Mode2
	.byte DL.Blank1
	.byte DL.Mode2
	
	.byte DL.VBL
	.word DisplayList
	
	
Logo
	ins 'logo.bin'
	

	