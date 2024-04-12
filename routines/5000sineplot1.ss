
;		*****************************************	
;		*	    5000 SINEPLOTTER		*
;		*	 ----------------------		*
;		*      	  Coding on 10.11.1992		*
;		*	  by  KANE  of SUSPECT		*
;		*****************************************

org	$20000
load	$20000

raster:	macro
	cmpi.b	#$ff,$dff006
	bne.s	*-10
	cmpi.b	#$ff,$dff006
	beq.s	*-10
	endm
waitblt:macro
	btst.b	#6,$dff002
	bne.s	*-10
	endm

s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,oldregs
	move	$dff01c,oldregs+2
	ori.l	#$80008000,oldregs
;	move	#$7fff,$dff09a
	move	#$7fff,$dff096
	move	#$83c0,$dff096

	bsr.s	pl_plotter

	move	oldregs,$dff096
	move	oldregs+2,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;-------------------------------------------------------------------

pl_plotter:
	move.l	#copper,$dff080
	bsr	makesine
	move.l	#$dff000,a0
	move.l	#$ffffffff,$44(a0)
control:
	cmpi.b	#$ff,6(a0)
	bne	control
	bsr	plotter
	btst.b	#6,$bfe001
	bne	control
quit:	rts

plotter:btst.b	#14,$2(a0)
	bne	plotter
	clr	$42(a0)
	clr	$66(a0)
	move	#$100,$40(a0)
	move.l	scron,$54(a0)
	move	#[heith*64]+[row/2],$58(a0)
	move.l	scron,d0		;zmien ekrany
	move.l	scroff,d1
	move.l	d0,scroff
	move.l	d1,scron
	move	d1,screen+6
	swap	d1
	move	d1,screen+2

wait1:	btst.b	#14,$2(a0)
	bne	wait1
	lea	sine,a1
	move.l	scroff,a2
	move	#128,d0
	mulu	#row,d0
	adda.l	d0,a2
	addi	#6,anglex		;szybkosc calosci
	addi	#2,angley
	andi	#andval,anglex
	andi	#andval,angley
	move	anglex,d0
	move	angley,d1
	move	#1201,d7
plloop:
	addi	#4,d0			;szybkosc jednostkowa
	addi	#8,d1			;tu wstaw '4'!!!
	andi	#andval,d0
	andi	#andval,d1
	move	(a1,d0.w),d2
	move	(a1,d1.w),d3
	lsr	#1,d3

	addi	#256,d2
	mulu	#row,d3
	move	d2,d4
	lsr	#3,d2
	andi	#7,d4
	eori	#7,d4
	addi	d2,d3
	bset	d4,(a2,d3.w)
	dbf	d7,plloop
	rts

makesine:			;tworzenie tablicy sinusa
	lea	datatab-2,a2
	lea	sine,a3
mloop:	addq	#2,a2
	move	(a2)+,d0	;czy koniec?
	beq	quit
	move	(a2)+,d1
	lea	sinus,a1
mloop2:	move	(a1),d2
	cmpi	#$aaaa,d2	;koniec wzoru sinusa?
	beq	mloop
	muls	d0,d2
	divs	d1,d2
	move	d2,(a3)+
	clr.l	d2
	move	(a2),d2
	lsl	#1,d2
	add.l	d2,a1
	bra	mloop2

datatab:
dc.w	11,11,1,10,11,1,9,11,1,8,11,1,7,11,1,6,11,1,5,11,1,4,11,1
dc.w	3,11,1,4,11,1,5,11,1,6,11,1,7,11,1,8,11,1,9,11,1,10,11,1,0

sinus:
	dc.w	$0000,$0006,$000C,$0012,$0019,$001F,$0025,$002B
	dc.w	$0032,$0038,$003E,$0044,$004A,$0050,$0056,$005C
	dc.w	$0062,$0068,$006D,$0073,$0079,$007E,$0084,$0089
	dc.w	$008E,$0093,$0099,$009E,$00A2,$00A7,$00AC,$00B1
	dc.w	$00B5,$00B9,$00BE,$00C2,$00C6,$00CA,$00CE,$00D1
	dc.w	$00D5,$00D8,$00DC,$00DF,$00E2,$00E5,$00E7,$00EA
	dc.w	$00EC,$00EF,$00F1,$00F3,$00F5,$00F7,$00F8,$00FA
	dc.w	$00FB,$00FC,$00FD,$00FE,$00FE,$00FF,$00FF,$00FF
	dc.w	$00FF,$00FF,$00FF,$00FF,$00FE,$00FD,$00FC,$00FB
	dc.w	$00FA,$00F9,$00F7,$00F6,$00F4,$00F2,$00F0,$00EE
	dc.w	$00EB,$00E9,$00E6,$00E3,$00E0,$00DD,$00DA,$00D7
	dc.w	$00D3,$00D0,$00CC,$00C8,$00C4,$00C0,$00BC,$00B7
	dc.w	$00B3,$00AE,$00AA,$00A5,$00A0,$009B,$0096,$0091
	dc.w	$008C,$0086,$0081,$007B,$0076,$0070,$006A,$0065
	dc.w	$005F,$0059,$0053,$004D,$0047,$0041,$003B,$0035
	dc.w	$002F,$0028,$0022,$001C,$0016,$000F,$0009,$0003
	dc.w	$FFFD,$FFF7,$FFF1,$FFEA,$FFE4,$FFDE,$FFD8,$FFD1
	dc.w	$FFCB,$FFC5,$FFBF,$FFB9,$FFB3,$FFAD,$FFA7,$FFA1
	dc.w	$FF9B,$FF96,$FF90,$FF8A,$FF85,$FF7F,$FF7A,$FF74
	dc.w	$FF6F,$FF6A,$FF65,$FF60,$FF5B,$FF56,$FF52,$FF4D
	dc.w	$FF49,$FF44,$FF40,$FF3C,$FF38,$FF34,$FF30,$FF2D
	dc.w	$FF29,$FF26,$FF23,$FF20,$FF1D,$FF1A,$FF17,$FF15
	dc.w	$FF12,$FF10,$FF0E,$FF0C,$FF0A,$FF09,$FF07,$FF06
	dc.w	$FF05,$FF04,$FF03,$FF02,$FF01,$FF01,$FF01,$FF01,$ff01
	dc.w	$FF01,$FF01,$FF01,$FF02,$FF02,$FF03,$FF04,$FF05
	dc.w	$FF06,$FF08,$FF09,$FF0B,$FF0D,$FF0F,$FF11,$FF14
	dc.w	$FF16,$FF19,$FF1B,$FF1E,$FF21,$FF24,$FF28,$FF2B
	dc.w	$FF2F,$FF32,$FF36,$FF3A,$FF3E,$FF42,$FF47,$FF4B
	dc.w	$FF4F,$FF54,$FF59,$FF5E,$FF62,$FF67,$FF6D,$FF72
	dc.w	$FF77,$FF7C,$FF82,$FF87,$FF8D,$FF93,$FF98,$FF9E
	dc.w	$FFA4,$FFAA,$FFB0,$FFB6,$FFBC,$FFC2,$FFC8,$FFCE
	dc.w	$FFD5,$FFDB,$FFE1,$FFE7,$FFEE,$FFF4,$FFFA,$aaaa


;-------------------------------------------------------------------
copper:
dc.l	$1800000,$1820f0f
dc.l	$920048,$9400c0,$8e0171,$9037d1
screen:
dc.l	$e00007,$e20000
dc.l	$1020000,$1080000,$10a0000
dc.l	$3001ff00,$01009300
dc.l	$ffdffffe
dc.l	$3001ff00,$01000300
dc.l	$fffffffe

;-------------------------------------------------------------------
scron:		dc.l	$50000
scroff:		dc.l	$60000
scrclr:		dc.l	$70000

heith=256
row=64

anglex:	dc.w	128
angley:	dc.w	768
andval=$ffe

oldregs:	dc.l	0
even
;-------------------------------------------------------------------

sine:
blk.w	10000,0
