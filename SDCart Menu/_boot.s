/* Ultimate SD Cartridge - Atari 400/800/XL/XE Multi-Cartridge
   Copyright (C) 2015 Robin Edwards

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

   Boot.asm 
   --------
   This file builds with WUDSN/MADS into an 8K Atari ROM.
   It needs converted to a hex file for inclusion in the Quartus project.
   This can be done with avr-objcopy.exe -I binary -O ihex boot.rom boot_rom.hex
*/

;@com.wudsn.ide.asm.outputfileextension=.rom

	icl 'boot.inc'
	icl 'macros.inc'
	

	opt h-				;Disable Atari COM/XEX file headers

	org $a000			;RD5 cartridge base
	opt f+				;Activate fill mode

init	.proc 				;Cartridge initalization
	rts				;Only the minimum of the OS initialization is complete, you don't want to code here normally.
.endp
	
start	.proc 				;Cartridge start, RAM, graphics 0 and IOCB no for the editor (E:) are ready


//
//	Main
//

main	.proc 
	lda #0
	sta RevFlag
	sta WaitCmdFlag
	sta DirLevel			; start in root directory
	mva #$FF CH			; set last key pressed to none
	jsr copy_wait_for_reboot
	mva #3 BOOT			; patch reset - from mapping the atari (revised) appendix 11
	mwa #reset_routine CASINI
	jsr InitJoystick
	jsr SetUpDisplay
	jsr clear_screen
	jsr DisplayHeader
	jsr DisplayFooter
	jsr HomeSelection
main_loop
	jsr HighlightCurrentEntry
PollLoop
	lda ULTIMATE_CART_CMD
	cmp #CMD.Refresh
	beq display_cmd
	cmp #CMD.Error
	beq display_error
	cmp #CMD.Reboot
	beq reboot_cmd
	bit WaitCmdFlag
	bmi PollLoop
	jmp Read_keyboard
	
display_cmd
;	jsr HomeSelection
	jsr RefreshList
	jsr send_fpga_ack_wait_clear	; wait for the FPGA to clear the cmd byte
	jmp main_loop

display_error
	jsr show_error
	jsr send_fpga_ack_wait_clear	; wait for the FPGA to clear the cmd byte
	jsr clear_screen
	jsr set_colours			; restore normal colours
	jmp main_loop
	
reboot_cmd
	jsr send_fpga_ack_wait_clear
	jsr CleanUp
;	sei				; prevent GINTLK check in deferred VBI
	jsr $100

read_keyboard
	jsr CheckJoyStick
	cmp #$FF
	bne @+
	jsr GetKey
	beq Main_Loop
@
	jsr ToUpper
	cmp #'T'+1			; check for slot shortcut
	bcs @+
	cmp #'A'			
	bcc @+
	sbc #'A'			; carry is set
	cmp Entries			; see if we overshot the end of the list
	bcs Main_Loop
	jmp DoShortcut
@
	pha
	lsr MessageFlag
	lda KeyList			; number of entries
	asl @
	clc
	adc KeyList			; multiply entries by 3
	tax
	pla
ScanLoop
	cmp KeyList,x
	beq KeyFound
	dex
	dex
	dex
	bne ScanLoop
	jmp Main_Loop
KeyFound
	lda #>[Main_Loop-1]
	pha
	lda #<[Main_Loop-1]
	pha
	lda KeyList-1,x
	pha
	lda KeyList-2,x
	pha
	rts
KeyList
	.byte 10
	Target LaunchItem,Key.Return
	Target CursorUp,Key.Up
	Target CursorDown,Key.Down
	Target CursorLeft,Key.Left
	Target CursorRight,Key.Right
	Target Reboot,Key.X
	Target UpDir,Key.U
	Target NextPage,Key.Space
	Target NextPage,Key.CtrlDn
	Target PageUp,Key.CtrlUp
	.endp
	

.proc Reboot				; Reboot
	lda #CCTL.DISABLE
	jmp send_fpga_cmd
	.endp
	
	
.proc UpDir				; Back up one level in directory tree
	lda DirLevel
	beq Done
	jsr change_dir_message
	jsr HomeSelection
	lda #CCTL.UP_DIR
	jsr send_fpga_cmd
	sec
	ror WaitCmdFlag
	dec DirLevel			; say we backed up the directory tree
Done
	rts
	.endp
	
	
.proc PrevPage
	lda ULTIMATE_CART_LIST_FLAG
	and #ListFlags.FilesBefore	; see if there are any prior entries
	beq Done
	jsr EndSelection
	lda #CCTL.PREV_PAGE
	jsr send_fpga_cmd
	sec
	ror WaitCmdFlag
Done
	rts
	.endp
	

	
.proc NextPage				; Display next page of entries
	lda ULTIMATE_CART_LIST_FLAG
	and #ListFlags.FilesAfter	; see if there are any more entries
	beq Done
	jsr HomeSelection
	lda #CCTL.NEXT_PAGE
	jsr send_fpga_cmd
	sec
	ror WaitCmdFlag
Done
	rts
	.endp
	
	
	
.proc PageUp
	jsr PrevPage
	beq @+				; if PrevPage didn't do anything
	jsr HomeSelection		; otherwise home the cursor
@
	rts
	.endp
	
	

	
.proc DoShortcut			; Launch item via shortcut (pass item 0-19 in A)
	sta tmp3			; save item
Loop
	jsr HighlightCurrentEntry	; move highlight bar
	jsr WaitForSync			; give us time to see it
	lda tmp3
	cmp CurrEntry			; see if we need to move the selection bar
	beq Done
	bcs MoveDown
UpLoop					; move the cursor up
	jsr PrevItem
	jmp Loop
MoveDown
	jsr NextItem
	jmp Loop
Done
	jsr LaunchItem			; now the target item is selected, launch it
	jmp Main.Main_Loop
	.endp
	
	
	
.proc CursorUp
	lda CurrEntry
	bne PrevItem
	jmp PrevPage
	.endp
	
	
.proc CursorDown
	lda Entries
	cmp #2
	bcc @+
	sbc #1
	cmp CurrEntry			; is Entry < Entries - 1 ?
	beq IsLastEntry
	bcs NextItem
@
	rts
IsLastEntry				; if we're at the final entry, load next page of list
	jmp NextPage
	.endp
	
	
.proc CursorLeft
	jmp UpDir
	rts
	.endp
	
	
.proc CursorRight
	rts
	.endp
	
	
.proc PrevItem				; select previous item in list
	dec CurrEntry
	sbw CurrEntryPtr #$20
	rts
	.endp
	
	
.proc NextItem				; select next item in list
	inc CurrEntry
	adw CurrEntryPtr #$20
	rts
	.endp
	
	
//
//	Launch highlighted item
//
	
.proc LaunchItem			
	ldy #0
	lda (CurrEntryPtr),y		; find out what the item is
	beq Abort
	cmp #EntryType.Dir
	beq IsDir
	jsr starting_cartridge_message	; not Nul and not Dir, so must be a file
	ldy CurrEntry
	jmp SendSelection
IsDir
	jsr change_dir_message		; it's a directory
	inc DirLevel			; keep track of where we are in the tree
	lda CurrEntry
	pha
	jsr HomeSelection
	pla
	tay
SendSelection
	iny				; FPGA expects 1-20, so bump value
	tya
	jsr send_fpga_cmd
Abort
	rts
	.endp

	.endp	; proc start




//
//	Block waiting for key
//

.proc	WaitKey
	jsr GetKey
	beq WaitKey
	rts
	.endp
	

//
//	Scan keyboard (returns N = 1 for no key pressed, else ASCII in A)
//

.proc	GetKey
	ldx CH
	cpx #$FF
	beq NoKey
	mva #$FF CH		; set last key pressed to none
	lda scancodes,x
	cmp #$FF
NoKey
	rts
	.endp
	
	
//
//	Initialize Joystick
//

.proc InitJoystick
	mva #$0F StickState
	mva #$01 TriggerState
	rts
	.endp
	
	
//
//	Read Joystick
//

.proc CheckJoyStick
	lda Trig0
	cmp TriggerState
	bne TriggerChange	; trigger change
;	lda PORTA
	lda STICK0
	and #$0F
	cmp StickState
	bne StickChange		; stick change
	ldy Timer
	beq StickChange
Return
	lda #$FF
	rts

StickChange			; stick direction changed
	sta StickState
	ldy #6
	sty Timer
	cmp #$0F		; is stick centred?
	beq Return		; if yes, do nothing
	pha
	lda TriggerState
	tay
	lsr @
	lsr @
	eor #$80
	sta MotionFlag		; if we moved stick with trigger down, set flag to prevent button action on release
	tya
	asl @
	asl @
	tay
	pla			; get stick direction bits
	lsr @			; test up bit
	bcc @+
	iny			; down
	lsr @
	bcc @+
	iny			; left
	lsr @
	bcc @+
	iny			; right
@
	lda StickTable,y
	rts
StickTable
	.byte Key.Return,Key.Return,Key.Return,Key.Return
	.byte Key.Up,Key.Down,Key.Left,Key.Right


TriggerChange			; trigger has either gone up or down
	sta TriggerState
	cmp #0
	beq Done
	bit MotionFlag		; if button has come up without any stick movement, issue a return key
	bmi Done
	lda #Key.Return
	rts
Done
	lsr MotionFlag
	lda #$FF
	rts
	.endp
	
	


//
// Send a byte to the FPGA (byte in A)
//

.proc	send_fpga_cmd
	lsr WaitCmdFlag
	sta FPGA_CTRL
	rts
	.endp


.proc send_fpga_ack_wait_clear
	lda #$FF	; ack
	jsr send_fpga_cmd
wait_clear
	lda ULTIMATE_CART_CMD
	bne wait_clear
	rts
	.endp
	
	
	
//
//	Tell FPGA we've done a reset
//

.proc	reset_routine
	mva #3 BOOT
	lda #CCTL.RESET
	jmp send_fpga_cmd
	.endp




.proc	show_error
	mva #$31 color2		; set background to red
	jsr clear_screen
	jsr DisplayHeader
	ldax #ErrorMsg
	jsr ShowMsg
	ldax #ERROR_MSG_BUFFER
	jsr PutString
	jmp WaitKey
	.endp
	


.proc	starting_cartridge_message
	jsr OpenWindow
	ldax #StartCartMsg
	jmp ShowMsg
	.endp
	

.proc	change_dir_message
	jsr OpenWindow
	ldax #ChangeDirMsg
	jmp ShowMsg
	.endp
	
	
	
.proc	ShowMsg
	jsr PutString
	mva #0 RevFlag
	rts
	.endp



.proc	DisplayHeader
	lda #0
	sta cx
	sta cy
	ldax #txtHeader
	jmp PutString
	.endp


.proc	DisplayFooter
	mva #23 cy
	mva #0 cx
	ldax #txtFooter
	jmp PutString
	.endp


.proc	Cleanup				; clean up prior to launching cart
	sei
	mva #$0 NMIEN			; disable interrupts
	mwa OSVBI VVBLKI		; restore OS VBL
	lda #0
	sta SDMCTL
	sta DMACTL			; make sure screen blanks out immediately
;	mva #$40 NMIEN			; enable VBI, disable DLI
;	cli
	rts
	.endp

	
//
//	Display page of FAT filenames
//
	
.proc RefreshList
	mva #'A' tmp2		; shortcut key
	mwa #DIR_BUFFER dir_ptr
	lda #0
	sta cx
	sta Entry
	sta Entries
	mva #2 cy
Loop
	ldy #0
	lda (dir_ptr),y
	beq Done
	cmp #EntryType.File
	beq OutName
	cmp #EntryType.Dir
	beq OutDir
OutName
	jsr DisplayEntry
	jmp Next
OutDir
	jsr DisplayEntry
Next
	adw dir_ptr #$20	; bump filename pointer
	inc Entry		; bump entry number
	inc Entries		; bump total entries
	inc tmp2		; bump shortcut key
	inc cy
	lda cy
	cmp #22
	bcc Loop
Done
	lda cy
	cmp #22			; did we fill the screen?
	bcs Finished
	mva #0 cx
	jsr PadLine
	inc cy
	bne Done
Finished
	jmp DisplayScrollIndicators
	.endp
	
	
//
//	Display file list entry
//
	
.proc DisplayEntry
	lda #0
	sta RevFlag
	sta cx
	lda #1
	jsr PutChar
	lda tmp2
	eor #$80
	jsr PutChar
	lda #2
	jsr PutChar
	cpb Entry CurrEntry
	bne @+
	lda #1
	jsr PutChar
	ldx #128
	stx RevFlag
	jsr GetNamePtr
	jsr PutFileName
	jsr PadFileName
	lda #2
	jmp PutChar
@
	lda #$20
	jsr PutChar
	jsr GetNamePtr
	jsr PutFileName
	jsr PadFileName
	lda #$20
	jmp PutChar
	.endp
	
	
//
//	Highlight current entry
//

.proc HighlightCurrentEntry
	lda PrevEntry		; see if we need to un-highlight old entry
	cmp CurrEntry
	beq Done
	ldx #0
	jsr ReverseItem
	lda CurrEntry
	ldx #128
	jsr ReverseItem
	mva CurrEntry PrevEntry
Done
	rts
	.endp
	
	
//
//	Reverse out an entry (pass entry in A)
//

.proc	ReverseItem
	stx tmp1
	clc
	adc #2
	tay
	lda LineTable.Lo,y
	clc
	adc #3
	sta ScrPtr
	lda LineTable.Hi,y
	adc #0
	sta ScrPtr+1
	ldy #34
	lda #0
	bit tmp1
	spl
	lda #66
	sta (ScrPtr),y
	dey
@
	lda (ScrPtr),y
	eor #$80
	sta (ScrPtr),y
	dey
	bne @-
	lda #0
	bit tmp1
	spl
	lda #65
	sta (ScrPtr),y
	rts
	.endp
	
//
//	Cursor home
//

.proc HomeSelection
	mwa #DIR_BUFFER CurrEntryPtr
	lda #0
	sta CurrEntry
	sta PrevEntry
	rts
	.endp
	
//
//	Cursor home
//

.proc EndSelection
	mwa #DIR_BUFFER+[19*32] CurrEntryPtr
	lda #19
	sta CurrEntry
	sta PrevEntry
	rts
	.endp	


//
//	Get dir_ptr + 1 in a,x
//
	
.proc GetNamePtr
	ldax dir_ptr
	clc
	adc #1
	scc
	inx
	rts
	.endp
	
	
//
//	Wait for sync
//
	
.proc WaitForSync
	lda VCount
	rne
	lda VCount
	req
	rts
	.endp
	
	
//
//	Convert ATASCII to uppercase
//
	
.proc ToUpper
	cmp #'z'+1
	bcs @+
	cmp #'a'
	bcc @+
	sbc #32
@
	rts
	.endp

//
//	Copy the wait and reboot routing to RAM so we're not running from ROM when the FPGA switches it
//

.proc	copy_wait_for_reboot
	ldy #.len[RebootCode]
@
	lda RebootCode-1,y
	sta $100-1,y
	dey
	bne @-
	rts
	.endp
	
.proc RebootCode
	ldx #1
@
	lda VCount
	rne
	lda VCount
	req
	dex
	bpl @-
	jmp $E477
	.endp
	
	
//
//	Force OS to cold boot
//

	
	.local ForceColdStart
	lda #0
	sta NMIEN
	tax
@
	sta $0000,x
	sta $0200,x
	sta $0300,x
	sta $0400,x
	sta $0500,x
	inx
	bne @-
@
	mva #$FF $0244
	rts
	.endl
	
	
	
; ************************ DATA ****************************
	
txtHeader
	.byte '  Ultimate Cartridge Menu',0
txtFooter
	.byte 28+128,29+128,'-Move  ',30+128,'-Up Dir  ','R'+128,'e'+128,'t'+128,'-Select  ','X'+128,'-Boot',0
	
StartCartMsg
	.byte 'Starting Cartridge...',0
ChangeDirMsg
	.byte 'Changing Directory...',0
NextPageMsg
	.byte 'Next page...',0
PrevPageMsg
	.byte 'Previous page...',0
txtEllipsis	equ *-4
ErrorMsg
	.byte 'Error:',0
	
	.align $100

	

	icl 'Display.s'
		
	
scancodes
	ins 'keytable.bin'
	

	org COMMAND_BUFFER
	.byte 0,0,0,0
	
	org DIR_BUFFER
	.local dir_entries
	.rept 32*20			; dir entries are 32 bytes long
	.byte 0
	.endr
	.endl
	
	org ERROR_MSG_BUFFER
	.byte 'Test error message',0

	.align $0200	
	
FontData
	ins 'sdcart.fnt'
	
; ************************ CARTRIDGE CONTROL BLOCK *****************

	org $bffa			;Cartridge control block
	.word start			;CARTCS
	.byte 0				;CART
	.byte CARTFG_START_CART		;CARTFG
	.word init			;CARTAD

