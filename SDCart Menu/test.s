
VCOUNT	equ	$D40B
COLBAK	equ $D01A

	org $2000
	
start
	lda vcount
	sta colbak
	jmp start
	
	run start
	