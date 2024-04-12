
;		*****************************************
;		*    RUBBER VECTORS + ESCAPING FRACTAL	*
;		*	-------------------------	*
;		*					*
;		*	   fixing on 22.10.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $20000
load $20000

waitblt: macro
wait?0:	btst.b	#14,$dff002
	bne.s	wait?0
	endm
raster: macro
wait?0:	cmp.b	#$fe,$dff006
	bne.s	wait?0
	cmp.b	#$ff,$dff006
	bne.s	wait?0+10
	endm


s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
	move	#$7fff,$dff09a
	move	#$7fff,$dff096
	move.l	$6c,oldlev
	move.l	#newlev3,$6c
	move	#$c020,$dff09a
	move	#$83c0,$dff096

	bsr	r_rubbervec

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	#$7fff,$dff09a
	move.l	oldlev,$6c
	move	olddma,$dff096
	move	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts


newlev3:	movem.l	d0-d7/a0-a6,-(sp)
		tst	v_start
		beq.s	new_novsin
		bsr	v_sineit
new_novsin:	movem.l	(sp)+,d0-d7/a0-a6
		move	#$20,$dff09c
		rte

;-------------------------------------------------------------------
oldint:		dc.w	0
olddma:		dc.w	0
oldlev:		dc.l	0
gfxname:	dc.b	'graphics.library',0,0
;-------------------------------------------------------------------
r_rubbervec:
	bsr	r_rvec
		move	#20,d7
r_loop00:	raster
		dbf	d7,r_loop00
	bsr	v_extraglass
	bsr	u_ghost
		lea	u_krup,a1
		lea	u_krdn,a2
r_krlop2:	raster
		subi.b	#8,1(a2)
		addi.b	#8,1(a1)
		cmpi.b	#$ff,1(a1)
		bne.s	r_krlop2
	bsr	g_glenz
	rts

r_rvec:	lea	$dff000,a0
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
;-------------------------------------------------------------------

v_extraglass:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#v_plane,$54(a0)	
		move	#[3*[v_heith+1]*64]+[v_row],$58(a0)
		waitblt
		bsr	v_initcop
		move.l	#v_plane,scron
		move.l	#v_plane+[3*v_heith*v_row],scroff
		lea	v_szescian,a6		;tu nazwe figury
		bsr	v_turn
		waitblt
		move	#1,v_start
		raster
		move.l	#v_copper,$dff080

v_control:	raster
		bsr.L	v_putc
		bsr.L	v_chgscr
		bsr.L	v_vector
		subi	#5,4(a6)
		cmpi	#45,4(a6)
		bne	v_control
		move	#200,l_counter
v_control2:	raster
		bsr.s	v_putc
		bsr.L	v_chgscr
		bsr.L	v_vector
		subi	#1,l_counter
		bne	v_control2
v_control3:	raster
		bsr.s	v_putc
		bsr.L	v_chgscr
		bsr.L	v_vector
		addi	#5,4(a6)
		cmpi	#260,4(a6)
		bne	v_control3
		clr	v_start
		rts

;-------------------------------------------------------------------
v_putc:
		lea	l_colmatrix,a1
		lea	v_copper,a2
		lea	v_copper+16,a3
		lea	v_coltab,a4
		move	#2,d7
v_plo:		move	(a1)+,d0
		bpl.s	v_pl2
		neg	d0
v_pl2:		lsl	#1,d0
		move	32(a4,d0.w),d1
		move	d1,2(a2)
		move	(a4,d0.w),d1
		move	d1,2(a3)
		addi	#4,a2
		addi	#4,a3
		dbf	d7,v_plo
		rts

;-------------------------------------------------------------------
v_chgscr:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	v_screen,a1
		move	#2,d2
v_chgl:		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi	#[v_heith*v_row],d1
		addq	#8,a1
		dbf	d2,v_chgl
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	scroff,$54(a0)
		move	#[3*[v_heith+1]*64]+[v_row/2],$58(a0)
		rts

v_vector:	move	20(a6),d7		;ilosc plaszczyzn
		lea	l_matrix,a4
		move.l	26(a6),a3
v_obr1:
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
		bpl.s	v_obr2
		move	#4,l_col
v_obr2:		move	(a3)+,d4
		bmi.s	v_nobpl
		move	(a3),d5
		bmi.s	v_nobpl
		lsl	#2,d4
		lsl	#2,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3

		move	l_col,d4
		move.l	scroff,a5
		btst	#0,d4
		beq.s	v_bpl2
		bsr.L	v_draw
v_bpl2:		addi	#[v_heith*v_row],a5
		move	l_col,d4
		btst	#1,d4
		beq.s	v_bpl3
		bsr.L	v_draw
v_bpl3:		addi	#[v_heith*v_row],a5
		move	l_col,d4
		btst	#2,d4
		beq.s	v_nobpl
		bsr.L	v_draw
v_nobpl:	dbf	d6,v_obr2
		lea	2(a3),a3
v_pomin:	dbf	d7,v_obr1

		waitblt					;fill
		clr.l	$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	scroff,d4
		addi	#[3*v_heith*v_row],d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[3*v_heith*64]+[v_row/2],$58(a0)

v_turn:		movem	6(a6),d0-d2
		add	d0,12(a6)
		add	d1,14(a6)
		add	d2,16(a6)
		lea	l_sinus,a1
		lea	l_sinus+128,a3		;cosinus
		lea	l_matrix,a4
		move.l	22(a6),a2		;tablica punktow
		move	18(a6),d7		;ilosc punktow-1

v_twodim:	move	16(a6),d0
		move	4(a2),d1
		move	(a2),d2		;zxy
		bsr.L	r_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr.L	r_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr.L	r_rotate

		move	#256,d4		;zooming wstepny
		sub	d3,d4
		mulu	d4,d1
		asr	#8,d1
		mulu	d4,d2
		asr	#8,d2

		move	4(a6),d3	;dod.srodek z
		move	#256,d4		;zooming calosci
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
		dbf	d7,v_twodim

		move	30(a6),d7
		lea	l_colmatrix,a4
v_td2:		move	16(a6),d0		;light vectors
		move	4(a2),d1
		move	(a2),d2		;zxy
		bsr.L	r_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr.L	r_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr.L	r_rotate

		move	d3,(a4)+
		addq	#6,a2
		dbf	d7,v_td2
		rts

v_draw:		movem.l	d0-d7/a2,-(sp)
		cmpi	d1,d3
		beq.L	v_noline
		bpl.s	v_line
		exg	d0,d2
		exg	d1,d3
v_line:		clr	d4
		addi	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl.s	v_dr1
		ori	#%010,d4
		neg	d0
v_dr1:		subi	d3,d1
		bpl.s	v_dr2
		ori	#%001,d4
		neg	d1
v_dr2:		cmpi	d0,d1
		bmi.s	v_dr3
		exg	d0,d1
		ori	#%100,d4
v_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	l_octant,a2
		move.b	(a2,d4.w),d7
		lsl	#1,d1
		or.l	#$0b4a0003,d7
		mulu	#v_row,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move.l	#-1,$44(a0)
		move	#v_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	v_dr4
		ori	#$40,d7
v_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
v_noline:	movem.l (sp)+,d0-d7/a2
		rts

;---------------------------------------------------------------------
v_initcop:	lea	v_copsin,a1
		move.l	#$3f01ff00,d0
		move.l	#$1020000,(a1)+
		move	#v_heith-2,d1
v_init1:	add.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1020000,(a1)+
		dbf	d1,v_init1
		rts

;---------------------------------------------------------------------
v_sineit:	lea	v_copsin,a1
		lea	v_sine1,a2
		addi	#-3,v_axid
		andi	#$3f,v_axid
		move	v_axid,d0
		move	#v_heith-2,d7
v_pion:		move.b	(a2,d0.w),d1
		move	d1,d2
		lsl	#4,d2
		ori.b	d1,d2
		move.b	d2,3(a1)
		addi	#1,d0
		andi	#$3f,d0
		lea	8(a1),a1
		dbf	d7,v_pion
		rts

;---------------------------------------------------------------------
v_sine1:
dc.b	$08,$09,$0A,$0B,$0B,$0C,$0D,$0D,$0E,$0E,$0F,$0F,$0F,$0F,$0F
dc.b	$0F,$0F,$0F,$0F,$0F,$0E,$0E,$0D,$0D,$0C,$0C,$0B,$0A,$09,$09,$08
dc.b	$08,$07,$07,$06,$05,$04,$04,$03,$03,$02,$02,$01,$01,$01,0,0,0
dc.b	0,0,0,$01,$01,$01,$02,$02,$03,$03,$04,$05,$05,$06,$07,$08

;-------------------------------------------------------------------
v_plane=$78000

v_heith=192
v_row=26
v_axid:			dc.w	0
v_start:		dc.w	0

v_coltab:	dc.w	0,$110,$220,$330,$440,$550,$660,$770,$880
		dc.w	$990,$aa0,$bb0,$cc0,$add0,$ee0,$ff0
v_coltab2:	dc.w	0,0,$110,$110,$220,$220,$330,$330,$440
		dc.w	$440,$550,$660,$770,$880,$990,$aa0
;-------------------------------------------------------------------
v_copper:
dc.l	$1820000,$1840000,$1860000,$1880880,$18a0000,$18c0000,$18e0000
	dc.l	$1800000
dc.l	$920050,$9400b0,$8e0171,$9037d1
v_screen:
dc.l	$e00000+[v_plane/$10000],$e20000+[v_plane&$ffff]
dc.l	$e40000+[[v_plane+[v_heith*v_row]]/$10000],$e60000+[[v_plane+[v_heith*v_row]]&$ffff]
dc.l	$e80000+[[v_plane+[2*v_heith*v_row]]/$10000],$ea0000+[[v_plane+[2*v_heith*v_row]]&$ffff]
dc.l	$10200ff,$1080000,$10a0000
	dc.l	$3401ff00,$1800a40
	dc.l	$3501ff00,$1800000
dc.l	$3f01ff00,$01003300
v_copsin:	blk.l	[2*v_heith]-1,0
dc.l	$ff01ff00,$01000300
dc.l	$ffdffffe
	dc.l	$1801ff00,$1800a40
	dc.l	$1901ff00,$1800000
dc.l	$fffffffe

;-------------------------------------------------------------------
v_szescian:
dc.w	112,94,255
dc.w	-4,2,-6
dc.w	120,128,128
dc.w	31,5
dc.l	v_szdots,v_szline
dc.w	2
v_szdots:
dc.w	-60,60,-60,60,60,-60,60,-60,-60,-60,-60,-60,-60,60,60
dc.w	60,60,60,60,-60,60,-50,-60,60
dc.w	-40,40,-50,40,40,-50,40,-40,-50,-40,-40,-50
dc.w	-40,40,50,40,40,50,40,-40,50,-40,-40,50
dc.w	-50,40,-40,-50,40,40,-50,-40,40,-50,-40,-40
dc.w	50,40,-40,50,40,40,50,-40,40,50,-40,-40
dc.w	40,-50,-40,40,-50,40,-40,-50,40,-40,-50,-40
dc.w	40,50,-40,40,50,40,-40,50,40,-40,50,-40
dc.w	0,0,15,15,0,0,0,15,0
v_szline:
dc.w	1,9,0,1,2,3,0,-1,8,9,10,11,8
dc.w	1,9,4,7,6,5,4,-1,12,15,14,13,12
dc.w	2,9,0,3,7,4,0,-1,16,19,18,17,16
dc.w	2,9,5,6,2,1,5,-1,20,21,22,23,20
dc.w	3,9,2,6,7,3,2,-1,24,25,26,27,24
dc.w	3,9,5,1,0,4,5,-1,28,31,30,29,28

;-------------------------------------------------------------------
u_ghost:	bsr	u_init
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#u_obrazek-10,$54(a0)
		move	#[350*64],$58(a0)
		move.l	#u_planee,scron
		move.l	#u_planee+[3*u_heigth*u_row],scroff
		bsr	u_setscr
		raster
		waitblt
		move.l	#u_copper,$dff080
		move	#15,d7
r_loop01:	raster
		dbf	d7,r_loop01
		bsr	u_fraktal1
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
u_fd1_loop:	raster
		bsr	u_colout
		tst	u_collicz
		bpl	u_fd1_loop
		bsr	u_show
		bsr	u_fraktal2
		move.l	#u_cot2,u_collicz+4
		lea	u_data,a3
		move.l	#$640008,(a3)
		move	#2,4(a3)
		move	#-4,6(a3)
		move	#-2,8(a3)
		move	#2,10(a3)

		lea	$dff000,a0
		bsr	u_show
		rts

u_show:		move.l	#$100002,u_collicz
u_control:	raster
		bsr	u_chg
		bsr	u_move
		bsr	u_colin
		tst	u_collicz
		bne	u_control
		move	#150,u_collicz
u_contr2:	raster
		bsr	u_chg
		bsr	u_move
		subi	#1,u_collicz
		bne	u_contr2
		move	#15,u_collicz
u_contr3:	raster
		bsr	u_chg
		bsr	u_move
		bsr	u_colout
		tst	u_collicz
		bne	u_contr3
		rts

u_chg:		waitblt
		move.l	scron,d0		;double screen mode
		move.l	scroff,d1
		move.l	d1,scron
		move.l	d0,scroff
		lea	u_screen,a1
		move	#2,d2
u_chg0:		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		add.l	#[u_heigth*u_row],d1
		addq.l	#8,a1
		dbf	d2,u_chg0
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[3*u_heigth*64]+[u_row/2],$58(a0)
		rts

u_init:		lea	u_cop2,a1		;create copperlist
		move.l	#$5801ff00,d0
		move.l	d0,(a1)+
		move.l	#$1003300,(a1)+
		move.l	#$1020000,(a1)+
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		move	#u_heigth-2,d1
u_zoom_c1:	add.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1020000,(a1)+
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		dbf	d1,u_zoom_c1
		rts

;----------------------------------------------------------------------
u_move:		lea	u_cop2+8,a1
		lea	u_data,a3
		lea	u_sine,a5
		move	2(a3),d1
		clr	d3
		move	6(a3),d5
		move	#u_heigth-1,d7
u_pion_loop:
		move	(a5,d1.w),d0
		addi	#40,d0
		move	d0,d2
		lsr	#4,d0
		lsl	#1,d0
		neg	d0
		subi	d3,d0
		addi	d0,d3
		andi	#$f,d2
		move	d0,6(a1)
		move	d0,10(a1)

		lea	16(a1),a1
		move.b	d2,3(a1)
		lsl	#4,d2
		or.b	d2,3(a1)
		add.b	d5,d1
		dbf	d7,u_pion_loop


		lea	u_obrazek+[50*u_row],a1
		move.l	scroff,a2
		movem	8(a3),d0/d1
		add.b	d0,1(a3)
		add.b	d1,3(a3)
		move	(a3),d1
		move	#$c000,d4		;for B mask
		move	#44,d7
		move	4(a3),d5
		waitblt
		move.l	#$be20000,$40(a0)
		move	#u_row-2,$60(a0)
		move	#u_row-2,$66(a0)
		move	#u_row-2,$64(a0)
u_hloop:	move	(a5,d1.w),d0
		addi	#40,d0
		lsl	#1,d0			;*22; 50 cykli,mulu- 70
		move	d0,d3
		lsl	#2,d0
		move	d0,d2
		lsl	#1,d2
		addi	d2,d0
		subi	d3,d0
		lea	(a2,d0.w),a4
		move.l	a1,a6
		waitblt
		move	d4,$72(a0)
		move.l	a6,$50(a0)		;copy 3 bitplanes
		move.l	a4,$48(a0)
		move.l	a4,$54(a0)
		move	#[70*64]+1,$58(a0)
		waitblt
		lea	u_heigth*u_row(a4),a4
		lea	u_heigth*u_row(a6),a6
		move.l	a6,$50(a0)
		move.l	a4,$48(a0)
		move.l	a4,$54(a0)
		move	#[70*64]+1,$58(a0)
		waitblt
		lea	u_heigth*u_row(a4),a4
		lea	u_heigth*u_row(a6),a6
		move.l	a6,$50(a0)
		move.l	a4,$48(a0)
		move.l	a4,$54(a0)
		move	#[70*64]+1,$58(a0)

		add.b	d5,d1
		ror	#2,d4
		bpl	u_hcont
		lea	2(a1),a1
		lea	2(a2),a2
u_hcont:	dbf	d7,u_hloop
		rts

;----------------------------------------------------------------------
;---------------------------FRAKTAL------------------------------------
;----------------------------------------------------------------------

u_setscr:	move	#2,d1
		lea	u_screen,a1
		move.l	#u_obrazek-5,d0
u_mkbp2:	move	d0,6(a1)
		swap	d0
		move	d0,2(a1)
		swap	d0
		add.l	#u_heigth*u_row,d0
		addq.l	#8,a1
		dbf	d1,u_mkbp2
		rts

u_fraktal1:	bsr	u_clear
		bsr	u_setscr
		bsr	u_makeVAR
		bsr	u_fractal
		move.w	#$1250,u_wartosc
		rts
u_fraktal2:	bsr	u_clear
		move.w	#$e792,u_res
		bsr	u_makeVAR
		bsr	u_fractal
		rts

u_clear:	lea	u_obrazek-8,a1
		move	#50*u_row*3,d0
u_clear_loop:	clr.l	(a1)+
		dbf	d0,u_clear_loop
		rts

u_ItMax = 15		;maximum iteration or colors
u_XS = 90		;Window width
u_YS = 70		;Window height

u_plane		dc.l	u_obrazek+[119*u_row]
u_wartosc:	dc.w	$1400

u_t:		dc.w	15500 	;change this
u_ysize:	dc.w	0
u_xpos:		dc.w	0
u_ypos:		dc.w	0
u_delta:	dc.w	0
u_xmax:		dc.w	0
u_ymin:		dc.w	0

u_makeVAR:	lea	u_t(pc),a0	;set variables using SIZE+POS
		move	(a0),d0
		mulu	#4,d0
		divu	#5,d0
		move	d0,u_ysize-u_t(a0)
		moveq	#0,d0
		moveq	#0,d1
		move	(a0),d0
		move	u_ysize-u_t(a0),d1
		divu	#u_xs,d0
		move	d0,u_delta-u_t(a0)
		move	u_xpos-u_t(a0),d0
		move	u_ypos-u_t(a0),d1
		add	(a0),d0
		sub	u_ysize-u_t(a0),d1
		asr	#1,d0
		asr	#1,d1
		move	d0,u_xmax-u_t(a0)
		move	d1,u_ymin-u_t(a0)
		rts
;--------------------------Fractal Routine-------------------------
u_fractal:	move	u_xMax(pc),a0
		move	u_yMin(pc),a1
		move	u_delta(pc),a2
		move	a0,a4
		move.l	u_plane(pc),a5
		move	#u_YS-1,d1
u_ForY:		move	#u_XS-1,d0
u_ForX:		move	a0,d2		;x=0
		move	a1,d3		;y=0
		moveq	#u_ItMax,d7	;itr=max
u_CheckIt:	move	d2,d4		;xq=x*x
		move	d3,d5		;yq=y*y
		muls	d4,d4
		muls	d5,d5
		move.l	d5,d6
		add.l	d4,d6		;xq+yq>>>
		swap	d6
		cmp	u_wartosc,d6
		bgt.s	u_OutOfRange	;yes ->Out of Range
		tst	d2
		bpl.s	u_JumpIn
		cmp	#$60,d6		;skip part
		bge.s	u_JumpIn
		moveq	#0,d7		;replace this if you want to
		bra.s	u_OutOfRange	;see what is skipped

u_Iteration:	move	d2,d4		;xq=x*x
		move	d3,d5		;yq=y*y
		muls	d4,d4
		muls	d5,d5
		move.l	d5,d6
		add.l	d4,d6		;xq+yq	
		swap	d6
		cmp	u_wartosc,d6
		bgt.s	u_OutOfRange	;yes ->Out of Range
u_JumpIn:	exg	d4,d2
		sub.l	d5,d2		;x=xq-yq
u_res:		dc.w	$e982
		swap	d2
		addi	a0,d2		;x=x+xc  zwrot: sub lub add
		bvs.s	u_OutOfRange
		muls	d4,d3		;y=z*y
		asl.l	#5,d3		;rescale number
		swap	d3
		add	a1,d3
		dbvs	d7,u_Iteration

		bvc.s	u_StaysBlack
u_OutOfRange:	move.l	a5,a3
		move	d0,d2
		eor	#7,d2
		move	d0,d3
		lsr	#3,d3
		add	d3,a3
u_Plane1:	btst	#0,d7
		beq.s	u_Plane2
	  	bset	d2,(a3)
u_Plane2:	lea	u_heigth*u_row(a3),a3
		btst	#1,d7
		beq.s	u_Plane3
		bset	d2,(a3)
u_Plane3:	lea	u_heigth*u_row(a3),a3
		btst	#2,d7
		beq.s	u_staysblack
		bset	d2,(a3)

u_StaysBlack:	sub	a2,a0
		dbf	d0,u_ForX
		move	a4,a0
		add	a2,a1
		sub	#u_row,a5
		dbf	d1,u_ForY
u_outofit:	rts
;----------------------------------------------------------------------
u_collicz:	dc.w	15,2
		dc.l	u_cot1

u_colin:	tst	u_collicz+2
		beq	u_colin2
		subi	#1,u_collicz+2
		rts
u_colin2:	move	#2,u_collicz+2
		subi	#1,u_collicz
		move.l	u_collicz+4,a2
		lea	u_copper,a1
		move	#7,d3				;ilosc kolorow
u_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq	u_scol2
		addi	#1,2(a1)
u_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq	u_scol3
		addi	#$10,2(a1)
u_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq	u_scol4
		addi	#$100,2(a1)
u_scol4:	adda	#4,a1
		dbf	d3,u_scol1
		rts

u_colout:	tst	u_collicz+2
		beq	u_fadcol
		subi	#1,u_collicz+2
		rts
u_fadcol:	move	#1,u_collicz+2
		subi	#1,u_collicz
		lea	u_copper,a1
		move	#7,d3
u_fad1:		move	2(a1),d1
		andi	#$f,d1
		beq	u_fad2
		subi	#1,2(a1)
u_fad2:		move	2(a1),d1
		andi	#$f0,d1
		beq	u_fad3
		subi	#$10,2(a1)
u_fad3:		move	2(a1),d1
		andi	#$f00,d1
		beq	u_fad4
		subi	#$100,2(a1)
u_fad4:		adda	#4,a1
		dbf	d3,u_fad1
		rts

;----------------------------------------------------------------------
u_planee=$78000
u_obrazek=$74000	;fractal
u_heigth=160
u_row=22
;----------------------------------------------------------------------
u_data:
dc.w	0,10			;axids
dc.w	4,-2			;add values
dc.w	-2,6			;global add
;----------------------------------------------------------------------
u_sine:
	dc.w	$0000,$0001,$0003,$0005,$0007,$0009,$000B,$000D
	dc.w	$000F,$0011,$0012,$0014,$0016,$0017,$0019,$001B
	dc.w	$001C,$001D,$001F,$0020,$0021,$0022,$0023,$0024
	dc.w	$0025,$0025,$0026,$0026,$0027,$0027,$0027,$0027
	dc.w	$0027,$0027,$0027,$0027,$0027,$0026,$0026,$26,$0025
	dc.w	$0024,$0023,$0022,$0021,$0020,$001F,$001E,$001D
	dc.w	$001B,$001A,$0018,$0017,$0015,$0013,$0012,$0010
	dc.w	$000E,$000C,$000A,$0008,$0006,$0004,$0002
	dc.w	$0000,$FFFE,$FFFC,$FFFA,$FFF8,$FFF6,$FFF4,$FFF2
	dc.w	$FFF0,$FFEE,$FFED,$FFEB,$FFE9,$FFE8,$FFE6,$FFE5
	dc.w	$FFE3,$FFE2,$FFE1,$FFE0,$FFDF,$FFDE,$FFDD,$FFDC
	dc.w	$FFDB,$FFDA,$FFDA,$FFD9,$FFD9,$FFD9,$FFD9,$FFD9
	dc.w	$FFD9,$FFD9,$FFD9,$FFD9,$FFDA,$FFDA,$ffda,$FFDB,$FFDB
	dc.w	$FFDC,$FFDD,$FFDE,$FFDF,$FFE0,$FFE1,$FFE3,$FFE4
	dc.w	$FFE5,$FFE7,$FFE9,$FFEA,$FFEC,$FFEE,$FFEF,$FFF1
	dc.w	$FFF3,$FFF5,$FFF7,$FFF9,$FFFB,$FFFD,$FFFF

u_cot1:	dc.w	0,$ee0,$ec0,$ea0,$e80,$e60,$e40,$e20
u_cot2:	dc.w	0,$2f,$4d,$6b,$89,$a7,$c5,$e3

;----------------------------------------------------------------------
u_copper:
dc.l	$1800000
dc.l	$1820ee0,$1840ec0,$1860ea0,$1880e80,$18a0e60,$18c0e40,$18e0e20
dc.l	$920058,$9400a8,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
u_krup:
	dc.l	$3407fffe,$1800a40
	dc.l	$3501ff00,$1800000
u_screen:
dc.l	$e00007,$e20000,$e40007,$e60000,$e80007,$ea0000
u_cop2:
ds.l	[4*160]+1,0
dc.l	$01000300
dc.l	$1020000,$1080000,$10a0000
dc.l	$ffdffffe
	dc.l	$1807fffe,$1800a40
u_krdn:
	dc.l	$18fffffe,$1800000
dc.l	-2

;-------------------------------------------------------------------
g_glenz:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		move.l	#g_plane,scron
		move.l	#g_plane+[3*g_heith*g_row],scroff
		lea	g_szescian,a6		;tu nazwe figury
		bsr	g_turn
		bsr	g_chgscr
		bsr	g_chgscr
		bsr	g_vector
		waitblt
		raster
		move.l	#g_copper,$dff080
		move	#10,d7
g_wloop:	raster
		dbf	d7,g_wloop

		clr	l_counter
g_nie1:		raster
		bsr	g_setcol
		bsr	g_chgscr
		bsr	g_vector
		raster
		bsr	g_chgscr
		bsr	g_vector
		lea	g_nieb,a1
		move	#30,d6
		move	l_counter,d5
		lsl	#1,d5
		subi	d5,d6
		move	l_counter,d5
		beq.s	g_nie2
g_nie0:		cmpi	#$180,(a1)+
		bne.s	g_nie0
		dbf	d5,g_nie0
g_nie2:		cmpi	#$180,(a1)+
		bne.L	g_nie2
		addi	#1,(a1)+
		dbf	d6,g_nie2
		addi	#1,l_counter
		cmpi	#15,l_counter
		bne.L	g_nie1

		move	#100,l_counter
g_control1:	raster
		bsr	g_chgscr
		bsr	g_vector
		subi	#1,l_counter
		bne.s	g_control1

		clr	l_counter
g_nie01:	raster
		bsr	g_chgscr
		bsr	g_vector
		addi	#3,4(a6)
		raster
		bsr	g_chgscr
		bsr	g_vector
		addi	#3,4(a6)
		lea	g_nieb,a1
		move	#30,d6
		move	l_counter,d5
		lsl	#1,d5
		subi	d5,d6
		move	l_counter,d5
		beq.s	g_nie02
g_nie00:	cmpi	#$180,(a1)+
		bne.s	g_nie00
		dbf	d5,g_nie00
g_nie02:	cmpi	#$180,(a1)+
		bne.L	g_nie02
		subi	#1,(a1)+
		dbf	d6,g_nie02
		addi	#1,l_counter
		cmpi	#15,l_counter
		bne.L	g_nie01

		move	#20,l_counter
g_control3:	raster
		bsr	g_fadcol
		bsr	g_chgscr
		bsr	g_vector
		addi	#3,4(a6)
		subi	#1,l_counter
		bne.s	g_control3
		rts

;-------------------------------------------------------------------
g_chgscr:waitblt
	move.l	scron,d0
	move.l	scroff,d1
	move.l	d0,scroff
	move.l	d1,scron
	lea	g_screen,a1
	move	#2,d2
g_chgl:	move	d1,6(a1)
	swap	d1
	move	d1,2(a1)
	swap	d1
	addi	#[g_heith*g_row],d1
	addq	#8,a1
	dbf	d2,g_chgl
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	d0,$54(a0)
	move	#[3*g_heith*64]+[g_row/2],$58(a0)
	rts

g_vector:move	20(a6),d7		;ilosc plaszczyzn
	lea	l_matrix,a4
	move.l	26(a6),a3
	lea	g_shadetab+4,a1
	lea	g_shadetab,a2
	move.l	#-1,(a2)
g_obr1:
	move	(a3)+,d6
	addi	#1,(a2)
	move.l	scroff,a5
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
	bpl	g_obr2
	addi	#[g_heith*g_row],a5
	move	(a2),(a1)+
	addi	#1,2(a2)
g_obr2:	move	(a3)+,d4
	move	(a3),d5
	lsl	#2,d4
	lsl	#2,d5
	movem	(a4,d4.w),d0/d1
	movem	(a4,d5.w),d2/d3
	bsr	g_draw
	dbf	d6,g_obr2
	lea	2(a3),a3
g_pomin:dbf	d7,g_obr1

	move.l	scroff,a5
	addi	#[2*g_heith*g_row],a5
	move	2(a2),d7			;ile
	lea	g_shadetab+4,a1
	lea	g_szshade,a2
g_cien1:move	(a1)+,d0
	mulu	#10,d0
	lea	(a2,d0.w),a3
	move	#3,d6
g_cien2:	move	(a3)+,d4
	move	(a3),d5
	lsl	#2,d4
	lsl	#2,d5
	movem	(a4,d4.w),d0/d1
	movem	(a4,d5.w),d2/d3
	bsr	g_draw
	dbf	d6,g_cien2
	dbf	d7,g_cien1

	waitblt					;fill
	clr.l	$64(a0)
	move.l	#$09f00012,$40(a0)
	move.l	scroff,d4
	addi	#[3*g_heith*g_row],d4
	move.l	d4,$54(a0)
	move.l	d4,$50(a0)
	move	#[3*g_heith*64]+[g_row/2],$58(a0)

g_turn:	movem	6(a6),d0-d2
	add	d0,12(a6)
	add	d1,14(a6)
	add	d2,16(a6)
	lea	l_sinus,a1
	lea	l_sinus+128,a3		;cosinus
	lea	l_matrix,a4
	move.l	22(a6),a2		;tablica punktow
	move	18(a6),d7		;ilosc punktow-1

g_twodim:	move	16(a6),d0
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
	dbf	d7,g_twodim
	rts

g_draw:	cmpi	d1,d3
	beq	g_noline
	bpl	g_line
	exg	d0,d2
	exg	d1,d3
g_line:	movem.l	d6/d7/a2,-(sp)
	clr	d4
	addi	#1,d1
	move	d0,d5
	move	d1,d6
	subi	d2,d0
	bpl	g_dr1
	ori	#%010,d4
	neg	d0
g_dr1:	subi	d3,d1
	bpl	g_dr2
	ori	#%001,d4
	neg	d1
g_dr2:	cmpi	d0,d1
	bmi	g_dr3
	exg	d0,d1
	ori	#%100,d4
g_dr3:	move	d5,d7
	and.l	#$f,d7
	ror	#4,d7
	swap	d7
	lea	l_octant,a2
	move.b	(a2,d4.w),d7
	lsl	#1,d1
	or.l	#$0b4a0003,d7
	mulu	#g_row,d6
	and.l	#$fff0,d5
	lsr	#3,d5
	addi	d6,d5
	adda.l	a5,d5
	waitblt
	move.l	#$ffff8000,$72(a0)
	move.l	#-1,$44(a0)
	move	#g_row,$60(a0)
	move	d1,$62(a0)
	move.l	d5,$48(a0)
	move.l	d5,$54(a0)
	subi	d0,d1
	bpl	g_dr4
	ori	#$40,d7
g_dr4:	move	d1,$52(a0)
	move.l	d7,$40(a0)
	subi	d0,d1
	move	d1,$64(a0)
	addi	#1,d0
	lsl	#6,d0
	addi	#2,d0
	move	d0,$58(a0)
	movem.l	(sp)+,d6/d7/a2
g_noline:	rts

;-------------------------------------------------------------------

g_setcol:	lea	g_copper,a1
		lea	g_coltab,a2
		move	#6,d3
g_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq	g_scol2
		addi	#1,2(a1)
g_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq	g_scol3
		addi	#$10,2(a1)
g_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq	g_scol4
		addi	#$100,2(a1)
g_scol4:	adda	#4,a1
		dbf	d3,g_scol1
		rts

g_fadcol:	lea	g_copper,a1
		move	#6,d3
g_fad1:		move	2(a1),d1
		andi	#$f,d1
		beq	g_fad2
		subi	#1,2(a1)
g_fad2:		move	2(a1),d1
		andi	#$f0,d1
		beq	g_fad3
		subi	#$10,2(a1)
g_fad3:		move	2(a1),d1
		andi	#$f00,d1
		beq	g_fad4
		subi	#$100,2(a1)
g_fad4:		adda	#4,a1
		dbf	d3,g_fad1
		rts

g_coltab:	dc.w	$f,$a40,$f0,$f00,$4a0,$a40,$f0
;-------------------------------------------------------------------
g_copper:
dc.l	$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
	dc.l	$1800000     ;3		4	5	6	7
dc.l	$920058,$9400b0,$8e0171,$9037d1
g_screen:
dc.l	$e00007,$e20000,$e40007,$e60000,$e80007,$ea0000
dc.l	$1020000,$1080000,$10a0000
g_nieb:
	dc.l	$3001ff00,$1800000
	dc.l	$3801ff00,$1800000
dc.l	$3f01ff00,$01003300
	dc.l	$4001ff00,$1800000
	dc.l	$4801ff00,$1800000
	dc.l	$5001ff00,$1800000
	dc.l	$5801ff00,$1800000
	dc.l	$6001ff00,$1800000
	dc.l	$6801ff00,$1800000
	dc.l	$7001ff00,$1800000
	dc.l	$7801ff00,$1800000
	dc.l	$8001ff00,$1800000
	dc.l	$8801ff00,$1800000
	dc.l	$9001ff00,$1800000
	dc.l	$9801ff00,$1800000
	dc.l	$a001ff00,$1800000

	dc.l	$b001ff00,$1800000
	dc.l	$b801ff00,$1800000
	dc.l	$c001ff00,$1800000
	dc.l	$c801ff00,$1800000
	dc.l	$d001ff00,$1800000
	dc.l	$d801ff00,$1800000
	dc.l	$e001ff00,$1800000
	dc.l	$e801ff00,$1800000
	dc.l	$f001ff00,$1800000
	dc.l	$f801ff00,$1800000
dc.l	$ff01ff00,$01000300
dc.l	$ffdffffe
	dc.l	$0001ff00,$1800000
	dc.l	$0801ff00,$1800000
	dc.l	$1001ff00,$1800000
	dc.l	$1801ff00,$1800000
	dc.l	$2001ff00,$1800000
	dc.l	$2801ff00,$1800000
dc.l	$fffffffe

;-------------------------------------------------------------------
g_plane=$78000

g_heith=192
g_row=24
g_shadetab:		blk.w	8,0
;-------------------------------------------------------------------
g_szescian:
dc.w	102,100,26
dc.w	2,-6,8
dc.w	120,128,128
dc.w	43,5
dc.l	g_szdots,g_szline
g_szdots:
dc.w	-25,-50,-50,-25,50,-50,0,50,-50,0,-50,-50	;front
dc.w	25,-50,-50,25,50,-50,50,50,-50,50,25,-50,-50,25,-50
dc.w	-50,0,-50,50,0,-50,50,-25,-50,-50,-25,-50,-50,-50,-50
dc.w	-25,-50,50,-25,50,50,0,50,50,0,-50,50		;back
dc.w	25,-50,50,25,50,50,50,-50,50,50,25,50,-50,25,50,-50,0,50
dc.w	50,0,50,50,-25,50,-50,-25,50,-50,50,50
dc.w	50,-50,-50,50,-50,-25,50,-50,0,50,-50,25	;right
dc.w	50,50,-25,50,50,0,50,50,25,50,50,50
dc.w	-50,-50,-25,-50,-50,0,-50,-50,25,-50,-50,50	;left
dc.w	-50,50,-50,-50,50,-25,-50,50,0,-50,50,25
g_szline:
dc.w	13,0,1,2,3,4,5,6,7,8,9,10,11,12,13,0			;front
dc.w	13,14,17,16,19,18,20,25,26,23,24,21,22,27,15,14		;back
dc.w	13,29,32,33,30,31,34,35,21,7,10,24,25,11,28,29		;right
dc.w	13,36,37,42,43,38,39,26,12,9,23,22,8,40,41,36		;left
dc.w	13,37,30,31,38,39,14,0,3,17,18,4,28,29,36,37		;top
dc.w	13,40,41,32,33,42,43,34,35,19,5,2,16,15,1,40		;bottom
g_szshade:
dc.w	13,28,6,40,13
dc.w	39,20,35,27,39
dc.w	28,20,35,6,28
dc.w	13,39,27,40,13
dc.w	39,20,28,13,39
dc.w	40,27,35,6,40

;-------------------------------------------------------------------

scron:		dc.l	v_plane
scroff:		dc.l	$70000
l_counter:	dc.w	0
l_sinus=$30000
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_col:			dc.w	0
l_colmatrix:		blk.w	4,0
l_matrix:		blk.l	50,0
>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1

end:

