
;		*****************************************	
;		*	     GLENZ 96 FACES		*
;		*	 ----------------------		*
;		*					*
;		*	   Coding on 18.09.1992		*
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
	bne.s	wait?0
	cmp.b	#$ff,$dff006
	bne.s	wait?0+10
	endm

s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
;	move	#$7fff,$dff09a
;	move	#$7fff,$dff096
	move	#$83c0,$dff096

	bsr	g_glenz

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
g_glenz:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		move.l	#g_plane,scron
		move.l	#g_plane+[3*g_heith*g_row],scroff
		lea	g_szescian,a6		;tu nazwe figury
		bsr	g_turn
		bsr	g_chgscr
		bsr	g_chgscr
		waitblt
		raster
		move.l	#g_copper,$dff080

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

		move	#120,l_counter
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


l_sinus=$30000
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_matrix:		blk.l	50,0
scron:		dc.l	$70000
scroff:		dc.l	$70000+[3*g_heith*g_row]
l_counter:		dc.w	0

>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1

end:
