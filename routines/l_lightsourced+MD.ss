;		*****************************************
;		*     LIGTSOURCED VECTORS + MD		*
;		*	-------------------------	*
;		*					*
;		*	   Coding on 23.10.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $40000
load $40000

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

	bsr	l_glenz2

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

l_vector:	move	20(a6),d7		;ilosc plaszczyzn
		lea	l_matrix,a4
		move.l	26(a6),a3
l_obr1:
		clr	l_prpnt
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
		bpl	l_obr2
		addi	#2,d6
		lsl	#1,d6
		lea	(a3,d6.w),a3
		bra.L	l_pomin
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
		addi.l	#[2*l_heith*l_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[2*l_heith*64]+[l_row/2],$58(a0)

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
		move	4(a2),d1
		move	(a2),d2		;zxy
		bsr	l_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr	l_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr	l_rotate

		move	#1024,d4		;zooming wstepny
		sub	d3,d4
		muls	d4,d1
		asr.l	#8,d1
		asr.l	#2,d1
		muls	d4,d2
		asr.l	#8,d2
		asr.l	#2,d2
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
		addq.l	#6,a2
		dbf	d7,l_twodim

		move	30(a6),d7
		lea	l_colmatrix,a4
l_td2:		move	16(a6),d0		;light vectors
		move	4(a2),d1
		move	(a2),d2		;zxy
		bsr	l_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr	l_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr	l_rotate
		move	d3,(a4)+
		addq.l	#6,a2
		dbf	d7,l_td2
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

l_checkcol:	cmpi	d1,d3
		bne.s	l_lineok
		rts
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
		lea	l_drawtab,a1
		move	d1,(a1)+
		move.l	d5,(a1)+
		subi	d0,d1
		bpl.s	l_dr4
		ori	#$40,d7
l_dr4:		move	d1,(a1)+
		move.l	d7,(a1)+
		subi	d0,d1
		move	d1,(a1)+
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,(a1)+
		movem.l (sp)+,d0-d7/a1

		move	l_col,d4		;check color
		move.l	scroff,a5
		btst	#0,d4
		beq.s	l_bpl2
		bsr.s	l_draw
l_bpl2:		addi.l	#[l_heith*l_row],a5
		btst	#1,d4
		beq.s	l_noline
		bsr.L	l_draw
l_noline:	rts

l_draw:		movem.l	a1/d1,-(sp)		;draw set line
		lea	l_drawtab,a1
		waitblt
		move.l	#$ffff8000,$72(a0)
		move.l	#-1,$44(a0)
		move	#l_row,$60(a0)
		move	(a1)+,$62(a0)
		move.l	(a1)+,d1
		adda.l	a5,d1
		move.l	d1,$48(a0)
		move.l	d1,$54(a0)
		move	(a1)+,$52(a0)
		move.l	(a1)+,$40(a0)
		move	(a1)+,$64(a0)
		move	(a1),$58(a0)
		movem.l	(sp)+,a1/d1
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
l_sinus=$60000
scron:		dc.l	0
scroff:		dc.l	0

l_heith=256
l_row=44
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_col:			dc.w	0
l_counter:		dc.w	30
l_colmatrix:		blk.w	4,0
l_matrix:		blk.l	50,0

ubor:	dc.w	0
dbor:	dc.w	255
lbor:	dc.w	10
rbor:	dc.w	351
l_prpnt:	dc.w	0
l_prtab:	blk.l	20,0
l_drawtab:	blk.w	16,0


;-------------------------------------------------------------------
;------------------------dane 2 animacji----------------------------
;-------------------------------------------------------------------
l_glenz2:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		move.l	#l_plane2,scron
		move.l	#l_plane2+[2*l_heith*l_row],scroff
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_md,$54(a0)
		move	#[3*l_heith*64]+[l_row/2],$58(a0)
		waitblt
		lea	l_mdload,a1
		lea	l_md+[30*l_row]+2,a2
		move	#194,d7
l_copcol01:	move	#9,d6
l_copcol02:	move.l	(a1),(a2)
		move.l	7800(a1),11264(a2)
		move.l	2*7800(a1),2*11264(a2)
		addi.l	#4,a1
		addi.l	#4,a2
		dbf	d6,l_copcol02
		addi.l	#4,a2
		dbf	d7,l_copcol01
		bsr	l_chgscr0
		bsr	l_chgscr0
		lea	l_piramida,a6		;tu nazwe figury
		bsr	l_turn
		waitblt
		raster
		move.l	#l_copper01,$dff080

l_control0:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		bsr	l_vector
		subi	#10,(a6)
		cmpi	#150,(a6)
		bpl.s	l_control0
		move	#-4,8(a6)
		move	#6,10(a6)
		bsr	l_cocop00
l_control1:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		bsr	l_vector
		addi	#10,4(a6)
		cmpi	#1000,4(a6)
		bmi.s	l_control1
		clr	l_prioryt+2
l_control2:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		bsr	l_vector
		addi	#2,2(a6)
		subi	#8,4(a6)
		addi	#4,(a6)
		cmpi	#500,(a6)
		bmi.s	l_control2
l_control3:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		bsr	l_vector
		addi	#4,(a6)
		cmpi	#550,(a6)
		bmi.s	l_control3

		move	#16,d7
ll_fadcol:	lea	l_copper01+16,a1
		move	#6,d6
ll_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq	ll_fad2
		subi	#1,2(a1)
ll_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq	ll_fad3
		subi	#$10,2(a1)
ll_fad3:	move	2(a1),d1
		andi	#$f00,d1
		beq	ll_fad4
		subi	#$100,2(a1)
ll_fad4:	adda	#4,a1
		dbf	d6,ll_fad1
		raster
		raster
		dbf	d7,ll_fadcol
		rts

;-------------------------------------------------------------------
l_putc0:	lea	l_colmatrix,a1
		lea	l_copper01,a2
		move	#2,d7
l_plo00:	move	(a1)+,d0
		bpl	l_pl02
		neg	d0
l_pl02:		move	d0,d1
		lsl	#8,d1
		ori	d1,d0
		move	d0,2(a2)
		addi	#4,a2
		dbf	d7,l_plo00
		rts

l_chgscr0:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_screen2,a1
		move	#1,d2
l_chgl00:	move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#[l_heith*l_row],d1
		addq.l	#8,a1
		dbf	d2,l_chgl00
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[2*l_heith*64]+[l_row/2],$58(a0)
		rts

l_cocop00:	lea	l_col2tab,a1
		lea	l_copper01+16,a2
		move	#6,d7
l_cocop01:	move	(a1)+,2(a2)
		addi	#4,a2
		dbf	d7,l_cocop01
		rts
;-------------------------------------------------------------------
l_plane2=$74000
l_md=$6a000
l_col2tab:		dc.w	$660,$771,$882,$994,$aa5,$cc7,$dd9
;-------------------------------------------------------------------
l_copper01:
dc.l	$1920000,$1940000,$1960000,$1900000
dc.l	$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1800000
dc.l	$920030,$9400d8,$8e0171,$9037d1
dc.l	$102000a,$1080000,$10a0000
dc.l	$e00000+[l_md/$10000],$e20000+[l_md&$ffff]
dc.l	$e80000+[[l_md+11264]/$10000],$ea0000+[[l_md+11264]&$ffff]
dc.l	$f00000+[[l_md+22528]/$10000],$f20000+[[l_md+22528]&$ffff]
l_screen2:
dc.l	$e40007,$e60000,$ec0007,$ee0000+[l_heith*[l_row]]
l_prioryt:
dc.l	$1040040
dc.l	$2a01ff00,$01005700
dc.l	$ffdffffe
dc.l	$2a01ff00,$01000300
dc.l	$fffffffe

;-------------------------------------------------------------------
l_piramida:
dc.w	520,110,400
dc.w	8,0,0
dc.w	128,128,128
dc.w	7,5
dc.l	l_pidots,l_piline
dc.w	2
l_pidots:
dc.w	-200,200,-100,200,200,-100,200,-200,-100,-200,-200,-100
dc.w	-200,200,100,200,200,100,200,-200,100,-200,-200,100
dc.w	0,0,15,15,0,0,0,15,0
l_piline:
dc.w	1,3,0,1,2,3,0,1,3,4,7,6,5,4,2,3,0,3,7,4,0,2,3,5,6,2,1,5
dc.w	3,3,2,6,7,3,2,3,3,5,1,0,4,5

;-------------------------------------------------------------------
l_mdload=$64000		;23400
>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1
>extern	"df0:.store/susMD.raw",l_mdload,-1

end:
