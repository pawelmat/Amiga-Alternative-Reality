;		*****************************************
;		*     LIGTSOURCED OVERSCREEN VECTORS	*
;		*	-------------------------	*
;		*					*
;		*	   Coding on 14.10.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $40000
load $40000

waitblt: macro
wait?0:	btst.b	#14,$dff002
	bne	wait?0
	endm
raster2: macro
wait?0:	cmp.b	#$30,$dff006
	bne	wait?0
	cmp.b	#$31,$dff006
	bne	wait?0+12
	endm


s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
	move	#$7fff,$dff09a
	move	#$7fff,$dff096
	move	#$83c0,$dff096

	bsr	l_glenz

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
l_glenz:
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	bsr	l_chgscr
	bsr	l_chgscr
	raster2
	move.l	#l_copper,$dff080
	lea	l_we,a6
	bsr	l_turn

l_control0:
		raster2
		bsr	l_chgscr
		bsr	l_vector
		btst.b	#6,$bfe001
		bne	l_control0
		rts

;-------------------------------------------------------------------
l_chgscr:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_screen,a1
		move	#0,d2
l_chgl:		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi	#[l_heith*l_row],d1
		addq	#8,a1
		dbf	d2,l_chgl
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)
		rts

l_vector:	move	20(a6),d7		;ilosc plaszczyzn
		lea	l_matrix,a4
		move.l	26(a6),a3
l_obr1:		move	(a3)+,d6
l_obr2:		move	(a3)+,d4
		move	(a3),d5
		lsl	#2,d4
		lsl	#2,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr	l_drawline
		dbf	d6,l_obr2
		lea	2(a3),a3

		lea	l_prtab,a2
		tst	l_prpnt
l_koryglin:	beq.s	l_pomin
		movem	(a2)+,d0-d3
		cmpi	ubor,d1
		bpl.s	l_kor1
		cmpi	ubor,d3
		bmi.s	l_nokor
		move	ubor,d1
		bra.s	l_kord
l_kor1:		cmpi	ubor,d3
		bpl.s	l_kord
		move	ubor,d3
l_kord:		cmpi	dbor,d1
		bmi.s	l_kor2
		cmpi	dbor,d3
		bpl.s	l_nokor
		move	dbor,d1
		bra.s	l_kor3
l_kor2:		cmpi	dbor,d3
		bmi.s	l_kor3
		move	dbor,d3
l_kor3:		bsr.L	l_checkcol
l_nokor:	subq	#8,l_prpnt
		bra.s	l_koryglin

l_pomin:	dbf	d7,l_obr1

		waitblt					;fill
		clr.l	$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	scroff,d4
		addi.l	#[l_heith*l_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)

l_turn:		movem	6(a6),d0-d2
		add	d0,12(a6)
		add	d1,14(a6)
		add	d2,16(a6)
		lea	l_sinus,a1
		lea	l_sinus+128,a3		;cosinus
		lea	l_matrix,a4
		move.l	22(a6),a2		;tablica punktow
		move	18(a6),d7		;ilosc punktow-1

l_twodim:	move	16(a6),d0
		clr	d1
		move	(a2),d2		;zxy
		bsr	l_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr	l_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr	l_rotate

		move	#4096,d4		;zooming wstepny
		sub	d3,d4
		muls	d4,d1
		asr.l	#8,d1
		asr.l	#4,d1
		muls	d4,d2
		asr.l	#8,d2
		asr.l	#4,d2
		move	4(a6),d3		;zoom calosci
		move	#1024,d4
		sub	d3,d4
		muls	d4,d1
		asr.l	#8,d1
		asr.l	#2,d1
		muls	d4,d2
		asr.l	#8,d2
		asr.l	#2,d2

		addi	(a6),d1			;dodaj srodek
		addi	2(a6),d2
		move	d1,(a4)+
		move	d2,(a4)+
		addq.l	#4,a2
		dbf	d7,l_twodim
		waitblt
		rts

l_drawline:	cmpi	rbor,d0			;cut to frame
		bmi.s	l_prawo2
		cmpi	rbor,d2
		bpl.L	l_noline
		move	d3,d4
		subi	d1,d4
		move	rbor,d5
		subi	d2,d5
		muls	d5,d4
		move	d0,d5
		subi	d2,d5
		divs	d5,d4
		move	rbor,d0
		move	d3,d1
		subi	d4,d1
		move	d1,d4
l_prdory:	move.l	a1,-(sp)
		lea	l_prtab,a1
		move	l_prpnt,d5
		move	rbor,(a1,d5.w)
		move	d4,2(a1,d5.w)
		addq	#4,d5
		move	d5,l_prpnt
		move.l	(sp)+,a1
		bra.s	l_lewo
l_prawo2:	cmpi	rbor,d2
		bmi.s	l_lewo
		move	d1,d4
		subi	d3,d4
		move	rbor,d5
		subi	d0,d5
		muls	d5,d4
		move	d2,d5
		subi	d0,d5
		divs	d5,d4
		move	rbor,d2
		move	d1,d3
		subi	d4,d3
		move	d3,d4
		bra.s	l_prdory
l_lewo:		tst	d0
		bpl.s	l_lewo2
		tst	d2
		bmi.l	l_noline
		move	d3,d4
		subi	d1,d4
		muls	d2,d4
		move	d2,d5
		subi	d0,d5
		divs	d5,d4
		clr	d0
		move	d3,d1
		subi	d4,d1
		bra.s	l_gora
l_lewo2:	tst	d2
		bpl.s	l_gora
		move	d1,d4
		subi	d3,d4
		muls	d0,d4
		move	d0,d5
		subi	d2,d5
		divs	d5,d4
		clr	d2
		move	d1,d3
		subi	d4,d3
l_gora:		cmpi	ubor,d1
		bpl.s	l_gora2
		cmpi	ubor,d3
		bmi.L	l_noline
		move	d2,d4
		subi	d0,d4
		move	d3,d5
		subi	ubor,d5
		muls	d5,d4
		move	d3,d5
		subi	d1,d5
		divs	d5,d4
		move	ubor,d1
		move	d2,d0
		subi	d4,d0
		bra.s	l_dol
l_gora2:	cmpi	ubor,d3
		bpl.s	l_dol
		move	d0,d4
		subi	d2,d4
		move	d1,d5
		subi	ubor,d5
		muls	d5,d4
		move	d1,d5
		subi	d3,d5
		divs	d5,d4
		move	ubor,d3
		move	d0,d2
		subi	d4,d2
l_dol:		cmpi	dbor,d1
		bmi.s	l_dol2
		cmpi	dbor,d3
		bpl.L	l_noline
		move	d2,d4
		subi	d0,d4
		move	dbor,d5
		subi	d3,d5
		muls	d5,d4
		move	d1,d5
		subi	d3,d5
		divs	d5,d4
		move	dbor,d1
		move	d2,d0
		subi	d4,d0
		bra.s	l_checkcol
l_dol2:		cmpi	dbor,d3
		bmi.s	l_checkcol
		move	d0,d4
		subi	d2,d4
		move	dbor,d5
		subi	d1,d5
		muls	d5,d4
		move	d3,d5
		subi	d1,d5
		divs	d5,d4
		move	dbor,d3
		move	d0,d2
		subi	d4,d2

l_checkcol:	move.l	scroff,a5
		cmpi	d1,d3
		bne.s	l_lineok
l_noline:	rts
l_lineok:	movem.l	d0-d7/a1,-(sp)		;set octants,etc
		cmpi	d1,d3
		bpl.s	l_line
		exg	d0,d2
		exg	d1,d3
l_line:		clr	d4
		addi	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl	l_dr1
		ori	#%010,d4
		neg	d0
l_dr1:		subi	d3,d1
		bpl	l_dr2
		ori	#%001,d4
		neg	d1
l_dr2:		cmpi	d0,d1
		bmi	l_dr3
		exg	d0,d1
		ori	#%100,d4
l_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	l_octant,a1
		move.b	(a1,d4.w),d7
		lsl	#1,d1
		or.l	#$0b4a0003,d7
		mulu	#l_row,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move.l	#-1,$44(a0)
		move	#l_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	l_dr4
		ori	#$40,d7
l_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
		movem.l (sp)+,d0-d7/a1
		rts

l_rotate:	andi	#$1fe,d0
		cmpi	#128,d0
		beq.s	l_norotate
		move	d1,d4
		move	d2,d5
		muls	(a3,d0.w),d4	; x*cos
		muls	(a1,d0.w),d5	; y*sin
		sub.l	d4,d5		;y'=y*sin-x*cos
		move.l	d5,d6
		andi.l	#$80000000,d6
		lsr.l	#8,d6
		or.l	d6,d5
		lsr.l	#8,d5
		muls	(a1,d0.w),d1
		muls	(a3,d0.w),d2
		add.l	d2,d1		;x'=x*sin+y*cos
		move.l	d1,d6
		andi.l	#$80000000,d6
		lsr.l	#8,d6
		or.l	d6,d1
		lsr.l	#8,d1
		move	d5,d2		;y' do d2
l_norotate:	rts

;-------------------------------------------------------------------
l_copper:
dc.l	$18200f0,$18400f0,$18600f0
	dc.l	$1800444
dc.l	$920030,$9400d8,$8e0171,$9037d1
l_screen:
dc.l	$e00007,$e20000,$e40007,$e60000+[l_heith*[l_row]]
dc.l	$1020000,$1080000,$10a0000
dc.l	$2a01ff00,$01001300
aa:
	dc.l	$d101ff00
	dc.l	$1080000![[-[3*l_row]]&$ffff],$10a0000+[[-[3*l_row]]&$ffff]
	dc.l	$d201ff00,$1820900,$1800a00
	dc.l	$dc01ff00,$1820800
	dc.l	$e601ff00,$1820700
	dc.l	$f001ff00,$1820600
	dc.l	$fa01ff00,$1820500
dc.l	$ffdffffe
	dc.l	$0401ff00,$1820400
	dc.l	$0e01ff00,$1820300
	dc.l	$1801ff00,$1820200
	dc.l	$2201ff00,$1820200
dc.l	$3001ff00,$01000300
dc.l	$fffffffe

;-------------------------------------------------------------------
l_sinus=$70000
l_plane=$71000
scron:		dc.l	l_plane
scroff:		dc.l	l_plane+[l_heith*l_row]

l_heith=256
l_row=44
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_colmatrix:		blk.w	4,0
l_matrix:		blk.l	50,0

ubor:	dc.w	0
dbor:	dc.w	197
lbor:	dc.w	10
rbor:	dc.w	351
l_prpnt:	dc.w	0
l_prtab:	blk.l	20,0
l_drawtab:	blk.w	16,0
;-------------------------------------------------------------------
l_we:
dc.w	175,120,980
dc.w	-2,4,2
dc.w	128,128,128
dc.w	13,1
dc.l	l_wedots,l_weline
l_wedots:
dc.w	-2000,-1000,-1600,1000,-1000,-200,-400,1000,0,-1000,-400,200,-1000,-1000,-1600,200
dc.w	2000,-1000,200,0,2000,1000,1000,200,1900,0,1000,-200
l_weline:
dc.w	7,0,1,2,3,4,5,6,7,0
dc.w	5,8,9,10,11,12,13,8
;-------------------------------------------------------------------
>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1

end:
