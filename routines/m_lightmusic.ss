
;		*****************************************
;		*       QUICK LIGTSOURCED VECTORS	*
;		*	-------------------------	*
;		*					*
;		*	   Coding on 20.09.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $20000
load $20000

waitblt: macro
wait?0:	btst.b	#14,$dff002
	bne	wait?0
	endm
raster: macro
wait?0:	cmp.b	#$fe,$dff006
	bne	wait?0
	cmp.b	#$ff,$dff006
	bne	wait?0+12
	endm


s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
;	move	#$7fff,$dff09a
;	move	#$7fff,$dff096
move	#$20,$dff096
	move	#$83c0,$dff096

	bsr	m_glenz

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	olddma,$dff096
	move.l	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;-------------------------------------------------------------------
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
;-------------------------------------------------------------------

m_glenz:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		bsr	m_chgscr
		bsr	m_chgscr
		lea	m_szescian,a6		;tu nazwe figury
		bsr	m_turn
		bsr	m_vector
		move.l	#m_copper,$dff080
m_control:	raster
		bsr	m_putc
		bsr	m_chgscr
		bsr	m_vector
		subi	#10,4(a6)
		cmpi	#270,4(a6)
		bne.s	m_control
m_control1:	raster
		bsr	m_putc
		bsr	m_chgscr
		bsr	m_vector

m_control9:	btst.b	#6,$bfe001
		bne	m_control1
		rts

;-------------------------------------------------------------------
m_putc:		lea	m_colmatrix,a1
		lea	m_copper,a2
		bsr.s	m_plo
		lsl	#4,d0
		move	d0,2(a2)		;green
		bsr.s	m_plo
		move	d0,6(a2)
		lsl	#4,d0
		ori	d0,6(a2)		;blue-green
		bsr.s	m_plo
		move	d0,10(a2)
		lsl	#4,d0
		ori	d0,10(a2)
		lsl	#4,d0
		ori	d0,10(a2)		;white
		rts
m_plo:		move	(a1)+,d0
		bpl	m_pl2
		neg	d0
m_pl2:		rts

;-------------------------------------------------------------------
m_chgscr:waitblt
	move.l	scron,d0
	move.l	scroff,d1
	move.l	d0,scroff
	move.l	d1,scron
	lea	m_screen,a1
	move	#1,d2
m_chgl:	move	d1,6(a1)
	swap	d1
	move	d1,2(a1)
	swap	d1
	addi.l	#[m_heith*m_row],d1
	addq.l	#8,a1
	dbf	d2,m_chgl
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	d0,$54(a0)
	move	#[2*m_heith*64]+[m_row/2],$58(a0)
	rts

m_vector:move	20(a6),d7		;ilosc plaszczyzn
	lea	m_matrix,a4
	move.l	26(a6),a3
m_obr1:
	move	(a3)+,m_col
	move	(a3)+,d6
	movem	(a3),d3-d5
	lsl	#2,d3
	lsl	#2,d4
	lsl	#2,d5
	movem	(a4,d3.w),d0/d1
	movem	(a4,d4.w),d2/d3
	subi	d0,d2
	subi	d1,d3			;d2,d3-wsp.wek.B
	movem	(a4,d4.w),d0/d1
	movem	(a4,d5.w),d4/d5
	subi	d0,d4
	subi	d1,d5			;d4,d5-wsp.wek.A
	muls	d4,d3
	muls	d5,d2
	sub.l	d2,d3
	bpl	m_obr2
	addi	#2,d6
	lsl	#1,d6
	lea	(a3,d6.w),a3
	bra	m_pomin
m_obr2:	move	(a3)+,d4
	move	(a3),d5
	lsl	#2,d4
	lsl	#2,d5
	movem	(a4,d4.w),d0/d1
	movem	(a4,d5.w),d2/d3
	move	m_col,d4
	move.l	scroff,a5
	btst	#0,d4
	beq	m_bpl2
	bsr	m_draw
m_bpl2:	addi.l	#[m_heith*m_row],a5
	move	m_col,d4
	btst	#1,d4
	beq	m_nobpl
	bsr	m_draw
m_nobpl:dbf	d6,m_obr2
	lea	2(a3),a3
m_pomin:dbf	d7,m_obr1

	waitblt					;fill
	clr.l	$64(a0)
	move.l	#$09f00012,$40(a0)
	move.l	scroff,d4
	addi	#[2*m_heith*m_row]-2,d4
	move.l	d4,$54(a0)
	move.l	d4,$50(a0)
	move	#[2*m_heith*64]+[m_row/2],$58(a0)

m_turn:	movem	6(a6),d0-d2
	add	d0,12(a6)
	add	d1,14(a6)
	add	d2,16(a6)
	lea	m_sinus,a1
	lea	m_sinus+128,a3		;cosinus
	lea	m_matrix,a4
	move.l	22(a6),a2		;tablica punktow
	move	18(a6),d7		;ilosc punktow-1

m_twodim:	move	16(a6),d0
	move	4(a2),d1
	move	(a2),d2		;zxy
	bsr	m_rotate
	move	14(a6),d0
	move	d2,d3
	move	2(a2),d2	;zyx
	bsr	m_rotate
	move	12(a6),d0
	exg	d1,d3		;xyz
	bsr	m_rotate

	move	#512,d4		;zooming wstepny
	sub	d3,d4
	muls	d4,d1
	asr.l	#8,d1
	asr.l	#1,d1
	muls	d4,d2
	asr.l	#8,d2
	asr.l	#1,d2

	move	4(a6),d3	;dod.srodek z
	move	#1024,d4		;zooming wstepny
	sub	d3,d4
	muls	d4,d1
	asr.l	#8,d1
	asr.l	#2,d1
	muls	d4,d2
	asr.l	#8,d2
	asr.l	#2,d2

	addi	(a6),d1		;dodaj srodek
	addi	2(a6),d2
	move	d1,(a4)+
	move	d2,(a4)+
	addq	#6,a2
	dbf	d7,m_twodim

	move	30(a6),d7
	lea	m_colmatrix,a4
m_td2:	move	16(a6),d0		;light vectors
	move	4(a2),d1
	move	(a2),d2		;zxy
	bsr	m_rotate
	move	14(a6),d0
	move	d2,d3
	move	2(a2),d2	;zyx
	bsr	m_rotate
	move	12(a6),d0
	exg	d1,d3		;xyz
	bsr	m_rotate

	move	d3,(a4)+
	addq	#6,a2
	dbf	d7,m_td2
	rts

m_draw:	movem.l	d0-d7/a2,-(sp)
	cmpi	d1,d3
	beq	m_noline
	bpl	m_line
	exg	d0,d2
	exg	d1,d3
m_line:	clr	d4
	addi	#1,d1
	move	d0,d5
	move	d1,d6
	subi	d2,d0
	bpl	m_dr1
	ori	#%010,d4
	neg	d0
m_dr1:	subi	d3,d1
	bpl	m_dr2
	ori	#%001,d4
	neg	d1
m_dr2:	cmpi	d0,d1
	bmi	m_dr3
	exg	d0,d1
	ori	#%100,d4
m_dr3:	move	d5,d7
	and.l	#$f,d7
	ror	#4,d7
	swap	d7
	lea	m_octant,a2
	move.b	(a2,d4.w),d7
	lsl	#1,d1
	or.l	#$0b4a0003,d7
	mulu	#m_row,d6
	and.l	#$fff0,d5
	lsr	#3,d5
	addi	d6,d5
	adda.l	a5,d5
	waitblt
	move.l	#$ffff8000,$72(a0)
	move.l	#-1,$44(a0)
	move	#m_row,$60(a0)
	move	d1,$62(a0)
	move.l	d5,$48(a0)
	move.l	d5,$54(a0)
	subi	d0,d1
	bpl	m_dr4
	ori	#$40,d7
m_dr4:	move	d1,$52(a0)
	move.l	d7,$40(a0)
	subi	d0,d1
	move	d1,$64(a0)
	addi	#1,d0
	lsl	#6,d0
	addi	#2,d0
	move	d0,$58(a0)
m_noline:movem.l (sp)+,d0-d7/a2
	rts

m_rotate:	andi	#$1fe,d0	;obroc punkty
	move	d1,d4
	move	d2,d5
	mulu	(a3,d0.w),d4
	mulu	(a1,d0.w),d5
	subi	d4,d5
	asr	#8,d5
	mulu	(a1,d0.w),d1
	mulu	(a3,d0.w),d2
	addi	d2,d1
	asr	#8,d1
	move	d5,d2
	rts

m_sinus:
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
	dc.w	$FFD5,$FFDB,$FFE1,$FFE7,$FFEE,$FFF4,$FFFA
	dc.w	$0000,$0006,$000C,$0012,$0019,$001F,$0025,$002B
	dc.w	$0032,$0038,$003E,$0044,$004A,$0050,$0056,$005C
	dc.w	$0062,$0068,$006D,$0073,$0079,$007E,$0084,$0089
	dc.w	$008E,$0093,$0099,$009E,$00A2,$00A7,$00AC,$00B1
	dc.w	$00B5,$00B9,$00BE,$00C2,$00C6,$00CA,$00CE,$00D1
	dc.w	$00D5,$00D8,$00DC,$00DF,$00E2,$00E5,$00E7,$00EA
	dc.w	$00EC,$00EF,$00F1,$00F3,$00F5,$00F7,$00F8,$00FA
	dc.w	$00FB,$00FC,$00FD,$00FE,$00FE,$00FF,$00FF,$00FF

;-------------------------------------------------------------------
m_copper:
dc.l	$1820000,$1840000,$1860000
	dc.l	$1800000
dc.l	$920058,$9400b0,$8e0171,$9037d1
m_screen:
dc.l	$e00007,$e20000,$e40007,$e60000+[m_heith*[m_row]]
dc.l	$1020000,$1080000,$10a0000
	dc.l	$3401ff00,$1800a40
	dc.l	$3501ff00,$1800000
dc.l	$4001ff00,$01002300
	dc.l	$180000f
dc.l	$fe01ff00,$01000300
	dc.l	$1800000
dc.l	$ffdffffe
	dc.l	$2001ff00,$1800a40
	dc.l	$2101ff00,$1800000
dc.l	$fffffffe

;-------------------------------------------------------------------
scron:		dc.l	$71000
scroff:		dc.l	$71000+[2*m_heith*m_row]

m_heith=190
m_row=24	;16
m_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
m_col:			dc.w	0
m_colmatrix:		blk.w	4,0
m_matrix:		blk.l	50,0
;-------------------------------------------------------------------
m_szescian:
dc.w	96,98,1020		;min 270
dc.w	-6,-4,2
dc.w	110,100,128
dc.w	7,5
dc.l	m_szdots,m_szline
dc.w	2
m_szdots:
dc.w	-38,38,-38
dc.w	38,38,-38
dc.w	38,-38,-38
dc.w	-38,-38,-38
dc.w	-38,38,38
dc.w	38,38,38
dc.w	38,-38,38
dc.w	-38,-38,38

dc.w	0,0,15
dc.w	15,0,0
dc.w	0,15,0

m_szline:
dc.w	1,3,0,1,2,3,0
dc.w	1,3,4,7,6,5,4
dc.w	2,3,0,3,7,4,0
dc.w	2,3,5,6,2,1,5
dc.w	3,3,2,6,7,3,2
dc.w	3,3,5,1,0,4,5

end:
