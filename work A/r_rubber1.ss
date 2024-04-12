
;		*****************************************
;		*       RUBBER LIGTSOURCED VECTOR	*
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
	move	#$83c0,$dff096

	bsr	r_rubbervec

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	olddma,$dff096
	move	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;-------------------------------------------------------------------
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
;-------------------------------------------------------------------
r_rubbervec:
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	lea	r_shade,a1
	move.l	#$7001ff00,d0
	move.l	#$e00007,(a1)+
	move.l	#$e20000,(a1)+
	move.l	#$e40007,(a1)+
	move.l	#$e60000,(a1)+
	move.l	#$1820000,(a1)+
	move.l	#$1840000,(a1)+
	move.l	#$1860000,(a1)+
	move	#29,d7
r_crlop:addi.l	#$4000000,d0
	move.l	d0,(a1)+
	move.l	#$e00007,(a1)+
	move.l	#$e20000,(a1)+
	move.l	#$e40007,(a1)+
	move.l	#$e60000,(a1)+
	move.l	#$1820000,(a1)+
	move.l	#$1840000,(a1)+
	move.l	#$1860000,(a1)+
	dbf	d7,r_crlop
		raster
		move.l	#r_copper,$dff080
		lea	r_krup,a1
		lea	r_krdn,a2
r_krlop1:	raster
		subi.b	#8,1(a1)
		addi.b	#8,1(a2)
		cmpi.b	#$ff,1(a2)
		bne.s	r_krlop1

		lea	r_szescian,a6
		bsr.L	r_turn
		move	#250,l_counter		;300
r_control:					;create animation
		move.l	r_adres,scroff
		bsr.L	r_clrscr
		bsr.L	r_vector
		addi.l	#[2*r_heith*r_row],r_adres
		subi	#1,r_klatki
		bne.s	r_control
	
	raster
	move	#$2300,r_on+6
	clr	r_adres+2
r_cont2:raster
	addi	#1,r_klatki
	cmpi	#32,r_klatki
	bne.s	r_co2
	clr	r_klatki
r_co2:	move	r_klatki,d6
	move	#30,d7
	lea	r_shade,a1
	clr.l	d5
r_rubber:
	move	d6,d0
	mulu	#[2*r_heith*r_row],d0
	addi.l	#r_start,d0
	add.l	d5,d0
	move	#1,d1				;set address
r_rub1:	move	d0,6(a1)
	swap	d0
	move	d0,2(a1)
	swap	d0
	addi.l	#[r_heith*r_row],d0
	addi.l	#8,a1
	dbf	d1,r_rub1

	move	d6,d0
	mulu	#6,d0
	addi.l	#r_animlight,d0
	move.l	d0,a2
	move	(a2)+,d0		;set colors
	move	d0,d1
	lsl	#8,d1
	ori	d1,d0
	move	d0,2(a1)
	move	(a2)+,d0
	lsl	#4,d0
	move	d0,6(a1)
	move	(a2),d0
	lsl	#8,d0
	move	d0,10(a1)
	addi.l	#16,a1

	addi	#1,d6
	cmpi	#32,d6
	bne.s	r_rub2
	clr	d6
r_rub2:	addi.l	#4*r_row,d5
	dbf	d7,r_rubber

	move	r_klatki,d0
	subi	#1,d0
	bpl.s	r_co3
	move	#31,d0
r_co3:	move	d0,r_adres
	mulu	#[2*r_heith*r_row],d0
	addi.l	#r_start,d0
	move.l	d0,scroff
	bsr.s	r_clrscr
	bsr.L	r_vector

	move	r_adres,d0
	mulu	#6,d0
	addi.l	#r_animlight,d0
	move.l	d0,a2
	lea	l_colmatrix,a1			;copy colors
	move	#2,d7
r_c3:	move	(a1)+,d0
	bpl.s	r_c4
	neg	d0
r_c4:	tst	r_adres+2
	beq.s	r_c5
	clr	(a2)+
	bra.s	r_c6
r_c5:	move	d0,(a2)+
r_c6:	dbf	d7,r_c3

	tst	r_adres+2
	bne	r_timer
	subi	#1,l_counter
	bne.L	r_cont2
	addi	#1,r_adres+2
	bra.L	r_cont2
r_timer:addi	#1,r_adres+2
	cmpi	#33,r_adres+2
	bne.L	r_cont2
	rts


r_clrscr:waitblt
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	scroff,$54(a0)
	move	#[2*r_heith*64]+[r_row/2],$58(a0)
	rts

r_vector:move	20(a6),d7		;ilosc plaszczyzn
	lea	l_matrix,a4
	move.l	26(a6),a3
r_obr1:
	move	(a3)+,l_col
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
	bpl	r_obr2
	addi	#2,d6
	lsl	#1,d6
	lea	(a3,d6.w),a3
	bra	r_pomin
r_obr2:	move	(a3)+,d4
	move	(a3),d5
	lsl	#2,d4
	lsl	#2,d5
	movem	(a4,d4.w),d0/d1
	movem	(a4,d5.w),d2/d3
	move	l_col,d4
	move.l	scroff,a5
	btst	#0,d4
	beq	r_bpl2
	bsr	r_draw
r_bpl2:	addi.l	#[r_heith*r_row],a5
	move	l_col,d4
	btst	#1,d4
	beq	r_nobpl
	bsr	r_draw
r_nobpl:dbf	d6,r_obr2
	lea	2(a3),a3
r_pomin:dbf	d7,r_obr1

	waitblt					;fill
	clr.l	$64(a0)
	move.l	#$09f00012,$40(a0)
	move.l	scroff,d4
	addi.l	#[2*r_heith*r_row],d4
	move.l	d4,$54(a0)
	move.l	d4,$50(a0)
	move	#[2*r_heith*64]+[r_row/2],$58(a0)

r_turn:	movem	6(a6),d0-d2
	add	d0,12(a6)
	add	d1,14(a6)
	add	d2,16(a6)
	lea	l_sinus,a1
	lea	l_sinus+128,a3		;cosinus
	lea	l_matrix,a4
	move.l	22(a6),a2		;tablica punktow
	move	18(a6),d7		;ilosc punktow-1

r_twodim:	move	16(a6),d0
	move	4(a2),d1
	move	(a2),d2		;zxy
	bsr	r_rotate
	move	14(a6),d0
	move	d2,d3
	move	2(a2),d2	;zyx
	bsr	r_rotate
	move	12(a6),d0
	exg	d1,d3		;xyz
	bsr	r_rotate

	addi	4(a6),d3	;dod.srodek z
	move	#256,d4		;zooming wstepny
	sub	d3,d4
	mulu	d4,d1
	asr	#8,d1
	mulu	d4,d2
	asr	#8,d2

	addi	(a6),d1		;dodaj srodek
	addi	2(a6),d2
	move	d1,(a4)+
	move	d2,(a4)+
	addq	#6,a2
	dbf	d7,r_twodim

	move	30(a6),d7
	lea	l_colmatrix,a4
r_td2:	move	16(a6),d0		;light vectors
	move	4(a2),d1
	move	(a2),d2		;zxy
	bsr	r_rotate
	move	14(a6),d0
	move	d2,d3
	move	2(a2),d2	;zyx
	bsr	r_rotate
	move	12(a6),d0
	exg	d1,d3		;xyz
	bsr	r_rotate

	move	d3,(a4)+
	addq	#6,a2
	dbf	d7,r_td2
	rts

r_draw:	movem.l	d0-d7/a2,-(sp)
	cmpi	d1,d3
	beq	r_noline
	bpl	r_line
	exg	d0,d2
	exg	d1,d3
r_line:	clr	d4
	addi	#1,d1
	move	d0,d5
	move	d1,d6
	subi	d2,d0
	bpl	r_dr1
	ori	#%010,d4
	neg	d0
r_dr1:	subi	d3,d1
	bpl	r_dr2
	ori	#%001,d4
	neg	d1
r_dr2:	cmpi	d0,d1
	bmi	r_dr3
	exg	d0,d1
	ori	#%100,d4
r_dr3:	move	d5,d7
	and.l	#$f,d7
	ror	#4,d7
	swap	d7
	lea	l_octant,a2
	move.b	(a2,d4.w),d7
	lsl	#1,d1
	or.l	#$0b4a0003,d7
	mulu	#r_row,d6
	and.l	#$fff0,d5
	lsr	#3,d5
	addi	d6,d5
	adda.l	a5,d5
	waitblt
	move.l	#$ffff8000,$72(a0)
	move.l	#-1,$44(a0)
	move	#r_row,$60(a0)
	move	d1,$62(a0)
	move.l	d5,$48(a0)
	move.l	d5,$54(a0)
	subi	d0,d1
	bpl	r_dr4
	ori	#$40,d7
r_dr4:	move	d1,$52(a0)
	move.l	d7,$40(a0)
	subi	d0,d1
	move	d1,$64(a0)
	addi	#1,d0
	lsl	#6,d0
	addi	#2,d0
	move	d0,$58(a0)
r_noline:movem.l (sp)+,d0-d7/a2
	rts

r_rotate:	andi	#$1fe,d0	;obroc punkty
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

;-------------------------------------------------------------------
r_copper:
	dc.l	$1800000
dc.l	$920060,$9400a0,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
r_krup:
	dc.l	$34fffffe,$1800a40
	dc.l	$3501ff00,$1800000
r_on:
dc.l	$7001ff00,$01000300
r_shade:
blk.l	[8*31]-1,0
dc.l	$db01ff00,$01000300
dc.l	$ffdffffe
	dc.l	$1807fffe,$1800a40
r_krdn:
	dc.l	$1807fffe,$1800000
dc.l	$fffffffe

;-------------------------------------------------------------------
r_heith=123
r_row=18
r_klatki:		dc.w	32
r_adres:		dc.l	r_start
r_start=$5d000
;-------------------------------------------------------------------
r_szescian:
dc.w	81,62,80
dc.w	4,2,4
dc.w	220,100,50
dc.w	7,5
dc.l	r_szdots,r_szline
dc.w	2
r_szdots:
dc.w	-40,60,-40,40,60,-40,40,-60,-40,-40,-60,-40
dc.w	-40,60,40,40,60,40,40,-60,40,-40,-60,40
dc.w	0,0,15,15,0,0,0,15,0
r_szline:
dc.w	1,3,0,1,2,3,0
dc.w	1,3,4,7,6,5,4
dc.w	2,3,0,3,7,4,0
dc.w	2,3,5,6,2,1,5
dc.w	3,3,2,6,7,3,2
dc.w	3,3,5,1,0,4,5

r_animlight:	blk.w	[33*3],0
end:
;-------------------------------------------------------------------

scroff:		dc.l	$700000
l_counter:	dc.w	0
l_sinus=$30000
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_col:			dc.w	0
l_colmatrix:		blk.w	4,0
l_matrix:		blk.l	50,0
>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1
