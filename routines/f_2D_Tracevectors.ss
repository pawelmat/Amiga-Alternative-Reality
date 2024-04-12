;		*****************************************
;		*     ALTERNATIVE REALITY 2D VECTORS	*
;		*	-------------------------	*
;		*					*
;		*	   Coding on 21.10.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $40000
load $40000

waitblt: macro
wait?0:	btst.b	#14,$dff002
	bne	wait?0
	endm
raster: macro
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
;	move	#$7fff,$dff09a
;	move	#$7fff,$dff096
	move	#$83c0,$dff096

	bsr	f_tracevec

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
f_tracevec:
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	move	#5,scrpnt
	move.l	#f_screen1,f_screen
	tst	f_ktore
	beq	f_ktok
	move.l	#f_screen2,f_screen
f_ktok:	bsr	f_chgscr
	bsr	f_chgscr
	bsr	f_chgscr
	bsr	f_chgscr
	bsr	f_chgscr
	bsr	f_chgscr
	clr	ubor
	move	#187,dbor
	move	#10,lbor
	move	#351,rbor
	tst	f_ktore
	bne	f_glenz
	lea	f_we,a6
	bsr	f_turn
	waitblt
	raster
	move.l	#f_copper,$dff080

f_control0:	raster
		bsr	f_chgscr
		bsr	f_vector
		lea	l_sinus,a1
		move	f_jumppnt,d0
		addi	#6,f_jumppnt
		andi	#$1fe,f_jumppnt
		move	#100,d1
		move	(a1,d0.w),d0
		asr	#1,d0
		addi	d0,d1
		move	d1,2(a6)
		subi	#3,(a6)
		cmpi	#-100,(a6)
		bpl.s	f_control0

		lea	f_browntab,a1
		bsr	f_setcols
		lea	f_are,a6
		clr	f_jumppnt
f_control1:	raster
		bsr	f_chgscr
		bsr	f_vector

		lea	l_sinus,a1
		move	f_jumppnt,d0
		addi	#2,f_jumppnt
		andi	#$fe,f_jumppnt
		move	#500,d1
		move	(a1,d0.w),d0
		lsl	#1,d0
		subi	d0,d1
		move	d1,2(a6)
		addi	#3,(a6)
		cmpi	#400,(a6)
		bmi.s	f_control1

		lea	f_whitetab,a1
		bsr	f_setcols
		lea	f_back,a6
		move	#30,f_jumppnt
f_control2:	raster
		bsr	f_chgscr
		bsr	f_vector

		lea	l_sinus,a1
		move	f_jumppnt,d0
		addi	#2,f_jumppnt
		andi	#$fe,f_jumppnt
		move	#600,d1
		move	(a1,d0.w),d0
		lsl	#1,d0
		subi	d0,d1
		move	d1,2(a6)
		subi	#4,(a6)
		cmpi	#176,(a6)
		bpl.s	f_control2

f_control3:	raster
		bsr	f_chgscr
		bsr	f_vector
		subi	#4,4(a6)
		cmpi	#700,4(a6)
		bpl.s	f_control3
f_control4:	raster
		bsr	f_chgscr
		bsr	f_vector
		subi	#6,4(a6)
		lea	f_copper,a2
		bsr.L	f_colin
		lea	f_cols2,a2
		bsr.L	f_colin
		cmpi	#580,4(a6)
		bpl.s	f_control4
		raster
		move.l	#f_copper0,$80(a0)
		move	#14,d7
f_control5:	raster
		raster
		subi	#$111,f_copper0+2
		dbf	d7,f_control5
		rts

;-------------------------------------------------------------------
f_glenz:	lea	f_glen,a6
		bsr	f_turn
		waitblt
		raster
		move.l	#f_copper1,$dff080

f_control6:	raster
		bsr	f_chgscr
		bsr	f_vector
		subi	#5,(a6)
		cmpi	#-100,(a6)
		bpl.s	f_control6

		lea	f_96,a6
		lea	f_whitetab,a1
		bsr	f_setcols2
f_control7:	raster
		bsr	f_chgscr
		bsr	f_vector
		addi	#1,4(a6)
		addi	#6,(a6)
		cmpi	#420,(a6)
		bmi.s	f_control7

		lea	f_faces,a6
		lea	f_bluetab,a1
		bsr	f_setcols2
		move	#$300,f_scrr+6
f_control8:	raster
		bsr	f_chgscr
		bsr	f_vector
		subi	#2,4(a6)
		cmpi	#1010,4(a6)
		bpl.s	f_control8
		move	#$5300,f_scrr+6
f_control9:	raster
		bsr	f_chgscr
		bsr	f_vector
		subi	#2,4(a6)
		cmpi	#930,4(a6)
		bpl.s	f_control9
		move	#16,8(a6)
f_control10:	raster
		bsr	f_chgscr
		bsr	f_vector
		addi	#2,4(a6)
		cmpi	#1030,4(a6)
		bmi.s	f_control10
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		rts

;-------------------------------------------------------------------

f_colin:	lea	(a2),a1
		move	#31,d3
f_fad1:		move	2(a1),d1
		andi	#$f,d1
		cmpi	#$f,d1
		beq	f_fad2
		addi	#1,2(a1)
f_fad2:		move	2(a1),d1
		andi	#$f0,d1
		cmpi	#$f0,d1
		beq	f_fad3
		addi	#$10,2(a1)
f_fad3:		move	2(a1),d1
		andi	#$f00,d1
		cmpi	#$f00,d1
		beq	f_fad4
		addi	#$100,2(a1)
f_fad4:		adda	#4,a1
		dbf	d3,f_fad1
		rts

;-------------------------------------------------------------------
f_chgscr:	waitblt
		move	scrpnt,d0
		bpl	f_chg2
		move	#5,scrpnt
		moveq	#5,d0
f_chg2:		move	#4,d1
		move.l	f_screen,a1
		lea	2(a1),a1
f_chg3:		move	d0,d2
		mulu	#[f_row*f_heith],d2
		add.l	#f_plane,d2
		tst	f_ktore
		bne.s	f_chgnoadd
		add.l	#20*f_row,d2
f_chgnoadd:	move	d2,4(a1)
		swap	d2
		move	d2,(a1)
		addq	#8,a1
		addq	#1,d0
		cmpi	#6,d0
		bne	f_chg4
		clr	d0
f_chg4:		dbf	d1,f_chg3
		mulu	#[f_row*f_heith],d0
		add.l	#f_plane,d0
		move.l	d0,scroff
		subi	#1,scrpnt

		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[f_heith*64]+[f_row/2],$58(a0)
		rts

f_vector:	move	20(a6),d7		;ilosc plaszczyzn
		lea	l_matrix,a4
		move.l	26(a6),a3
f_obr1:		move	(a3)+,d6
f_obr2:		move	(a3)+,d4
		move	(a3),d5
		lsl	#2,d4
		lsl	#2,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr	f_drawline
		dbf	d6,f_obr2
		lea	2(a3),a3

		lea	l_prtab,a2
		tst	l_prpnt
f_koryglin:	beq.s	f_pomin
		movem	(a2)+,d0-d3
		cmpi	ubor,d1
		bpl.s	f_kor1
		cmpi	ubor,d3
		bmi.s	f_nokor
		move	ubor,d1
		bra.s	f_kord
f_kor1:		cmpi	ubor,d3
		bpl.s	f_kord
		move	ubor,d3
f_kord:		cmpi	dbor,d1
		bmi.s	f_kor2
		cmpi	dbor,d3
		bpl.s	f_nokor
		move	dbor,d1
		bra.s	f_kor3
f_kor2:		cmpi	dbor,d3
		bmi.s	f_kor3
		move	dbor,d3
f_kor3:		bsr.L	f_checkcol
f_nokor:	subq	#8,l_prpnt
		bra.s	f_koryglin

f_pomin:	dbf	d7,f_obr1

		waitblt					;fill
		clr.l	$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	scroff,d4
		addi.l	#[f_heith*f_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[f_heith*64]+[f_row/2],$58(a0)

f_turn:		movem	6(a6),d0-d2
		add	d0,12(a6)
		add	d1,14(a6)
		add	d2,16(a6)
		lea	l_sinus,a1
		lea	l_sinus+128,a3		;cosinus
		lea	l_matrix,a4
		move.l	22(a6),a2		;tablica punktow
		move	18(a6),d7		;ilosc punktow-1

f_twodim:	move	16(a6),d0
		clr	d1
		move	(a2),d2		;zxy
		bsr	f_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr	f_rotate
		move	12(a6),d0
		exg	d1,d3		;xyz
		bsr	f_rotate

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
		dbf	d7,f_twodim
		waitblt
		rts

f_drawline:	cmpi	rbor,d0			;cut to frame
		bmi.s	f_prawo2
		cmpi	rbor,d2
		bpl.L	f_noline
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
f_prdory:	move.l	a1,-(sp)
		lea	l_prtab,a1
		move	l_prpnt,d5
		move	rbor,(a1,d5.w)
		move	d4,2(a1,d5.w)
		addq	#4,d5
		move	d5,l_prpnt
		move.l	(sp)+,a1
		bra.s	f_lewo
f_prawo2:	cmpi	rbor,d2
		bmi.s	f_lewo
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
		bra.s	f_prdory
f_lewo:		tst	d0
		bpl.s	f_lewo2
		tst	d2
		bmi.l	f_noline
		move	d3,d4
		subi	d1,d4
		muls	d2,d4
		move	d2,d5
		subi	d0,d5
		divs	d5,d4
		clr	d0
		move	d3,d1
		subi	d4,d1
		bra.s	f_gora
f_lewo2:	tst	d2
		bpl.s	f_gora
		move	d1,d4
		subi	d3,d4
		muls	d0,d4
		move	d0,d5
		subi	d2,d5
		divs	d5,d4
		clr	d2
		move	d1,d3
		subi	d4,d3
f_gora:		cmpi	ubor,d1
		bpl.s	f_gora2
		cmpi	ubor,d3
		bmi.L	f_noline
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
		bra.s	f_dol
f_gora2:	cmpi	ubor,d3
		bpl.s	f_dol
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
f_dol:		cmpi	dbor,d1
		bmi.s	f_dol2
		cmpi	dbor,d3
		bpl.L	f_noline
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
		bra.s	f_checkcol
f_dol2:		cmpi	dbor,d3
		bmi.s	f_checkcol
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

f_checkcol:	move.l	scroff,a5
		cmpi	d1,d3
		bne.s	f_lineok
f_noline:	rts
f_lineok:	movem.l	d0-d7/a1,-(sp)		;set octants,etc
		cmpi	d1,d3
		bpl.s	f_line
		exg	d0,d2
		exg	d1,d3
f_line:		clr	d4
		addi	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl	f_dr1
		ori	#%010,d4
		neg	d0
f_dr1:		subi	d3,d1
		bpl	f_dr2
		ori	#%001,d4
		neg	d1
f_dr2:		cmpi	d0,d1
		bmi	f_dr3
		exg	d0,d1
		ori	#%100,d4
f_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	l_octant,a1
		move.b	(a1,d4.w),d7
		lsl	#1,d1
		or.l	#$0b4a0003,d7
		mulu	#f_row,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move.l	#-1,$44(a0)
		move	#f_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	f_dr4
		ori	#$40,d7
f_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
		movem.l (sp)+,d0-d7/a1
		rts

f_rotate:	andi	#$1fe,d0
		cmpi	#128,d0
		beq.s	f_norotate
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
f_norotate:	rts

;-------------------------------------------------------------------
f_setcols:	lea	f_copper,a2
		move	#30,d7
f_setloop:	move	(a1)+,2(a2)
		addq	#4,a2
		dbf	d7,f_setloop
		rts

f_setcols2:	lea	f_copper1,a2
		move	#30,d7
f_setloop2:	move	(a1)+,2(a2)
		addq	#4,a2
		dbf	d7,f_setloop2
		rts

;-------------------------------------------------------------------
f_copper0:	dc.l	$1800fff,$1000300,$fffffffe
f_copper:
dc.l	$1820370,$1840370,$1860390,$1880370,$18a0390,$18c0390,$18e03b0
dc.l	$1900370,$1920390,$1940390,$19603b0,$1980390,$19a03b0,$19c03b0,$19e03d0
dc.l	$1a00370,$1a20390,$1a40390,$1a603b0,$1a80390,$1aa03b0,$1ac03b0,$1ae03d0
dc.l	$1b00390,$1b203b0,$1b403b0,$1b603d0,$1b803b0,$1ba03d0,$1bc03d0,$1be03f0
dc.l	$1800444
dc.l	$920030,$9400d8,$8e0171,$9037d1
f_screen1:
dc.l	$e00007,$e20000,$e40007,$e60000
dc.l	$e80007,$ea0000,$ec0007,$ee0000
dc.l	$f00007,$f20000
dc.l	$1020000,$1080000,$10a0000
dc.l	$2a01ff00,$01005300
	dc.l	$d101ff00
	dc.l	$1080000![[-[3*f_row]]&$ffff],$10a0000+[[-[3*f_row]]&$ffff]
	dc.l	$d201ff00
f_cols2:
dc.l	$1800a00
dc.l	$1820100,$1840100,$1860300,$1880100,$18a0300,$18c0300,$18e0500
dc.l	$1900100,$1920300,$1940300,$1960500,$1980300,$19a0500,$19c0500,$19e0700
dc.l	$1a00100,$1a20300,$1a40300,$1a60500,$1a80300,$1aa0500,$1ac0500,$1ae0700
dc.l	$1b00300,$1b20500,$1b40500,$1b60700,$1b80500,$1ba0700,$1bc0700,$1be0900
dc.l	$ffdffffe
dc.l	$3001ff00,$01000300
dc.l	$fffffffe

f_copper1:
dc.l	$1820700,$1840700,$1860900,$1880700,$18a0900,$18c0900,$18e0b00
dc.l	$1900700,$1920900,$1940900,$1960b00,$1980900,$19a0b00,$19c0b00,$19e0d00
dc.l	$1a00700,$1a20900,$1a40900,$1a60b00,$1a80900,$1aa0b00,$1ac0b00,$1ae0d00
dc.l	$1b00900,$1b20b00,$1b40b00,$1b60d00,$1b80b00,$1ba0d00,$1bc0d00,$1be0f00
dc.l	$1800000
dc.l	$920030,$9400d8,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
f_screen2:
dc.l	$e00007,$e20000,$e40007,$e60000
dc.l	$e80007,$ea0000,$ec0007,$ee0000
dc.l	$f00007,$f20000
f_scrr:
dc.l	$4a01ff00,$01005300
dc.l	$ffdffffe
dc.l	$0501ff00,$01000300
dc.l	$fffffffe
;-------------------------------------------------------------------
l_sinus=$70000
f_plane=$71000
scroff:		dc.l	0
scrpnt:		dc.w	5

f_heith=188
f_row=44
l_octant:		dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
l_matrix:		blk.l	50,0

ubor:	dc.w	0
dbor:	dc.w	187
lbor:	dc.w	10
rbor:	dc.w	351
l_prpnt:	dc.w	0
l_prtab:	blk.l	20,0
l_drawtab:	blk.w	16,0
f_jumppnt:	dc.w	0
f_ktore:	dc.w	0
f_screen:	dc.l	f_screen1

;-------------------------------------------------------------------
f_browntab:
dc.w	$710,$710,$930,$710,$930,$930,$b50
dc.w	$710,$930,$930,$b50,$930,$b50,$b50,$d70
dc.w	$710,$930,$930,$b50,$930,$b50,$b50,$d70
dc.w	$930,$b50,$b50,$d70,$b50,$d70,$d70,$f90
f_whitetab:
dc.w	$777,$777,$999,$777,$999,$999,$bbb
dc.w	$777,$999,$999,$bbb,$999,$bbb,$bbb,$ddd
dc.w	$777,$999,$999,$bbb,$999,$bbb,$bbb,$ddd
dc.w	$999,$bbb,$bbb,$ddd,$bbb,$ddd,$ddd,$fff
f_bluetab:
dc.w	$007,$007,$009,$007,$009,$009,$00b
dc.w	$007,$009,$009,$00b,$009,$00b,$00b,$00d
dc.w	$007,$009,$009,$00b,$009,$00b,$00b,$00d
dc.w	$009,$00b,$00b,$00d,$00b,$00d,$00d,$00f
;-------------------------------------------------------------------
f_we:
dc.w	450,600,970
dc.w	2,0,10
dc.w	128,128,100
dc.w	13,1
dc.l	f_wedots,f_weline
f_wedots:
dc.w	-2000,-1000,-1600,1000,-1000,-200,-400,1000,0,-1000,-400,200,-1000,-1000,-1600,200
dc.w	2000,-1000,200,0,2000,1000,1000,200,1900,0,1000,-200
f_weline:
dc.w	7,0,1,2,3,4,5,6,7,0
dc.w	5,8,9,10,11,12,13,8

f_are:
dc.w	-10,700,965
dc.w	10,0,0
dc.w	128,128,128
dc.w	20,3
dc.l	f_ardots,f_arline
f_ardots:
dc.w	-3000,1000,-1500,0,-1500,750,-2500,1000,-1000,1000,-1000,-1000
dc.w	-750,1000,-250,500,750,1000,0,250,750,-250,-750,-1000
dc.w	-250,-500,-250,0,250,-250
dc.w	1000,0,3000,1000,2000,250,2750,0,2000,-250,3000,-1000
f_arline:
dc.w	5,0,1,2,3,4,5,0
dc.w	5,6,7,8,9,10,11,6
dc.w	2,12,13,14,12
dc.w	5,15,16,17,18,19,20,15

f_back:
dc.w	420,700,990
dc.w	12,0,0
dc.w	128,128,128
dc.w	27,4
dc.l	f_badots,f_baline
f_badots:
dc.w	-3500,1000,-2000,500,-2500,0,-2000,-500,-3500,-1000
dc.w	-3000,500,-2500,250,-3000,0,-2500,-250,-3000,-500
dc.w	-2000,1000,-750,0,-750,750,-1500,1000,-250,1000,-250,-1000
dc.w	250,0,1500,1000,750,0,1500,-1000
dc.w	1750,0,2250,1000,2250,250,3500,1000,2500,0,3500,-1000,2250,-250,2250,-1000
f_baline:
dc.w	4,0,1,2,3,4,0
dc.w	4,5,6,7,8,9,5
dc.w	5,10,11,12,13,14,15,10
dc.w	3,16,17,18,19,16
dc.w	7,20,21,22,23,24,25,26,27,20


;---------------------------------------------------------------------
f_glen:
dc.w	480,94,980
dc.w	0,0,-2
dc.w	128,128,230
dc.w	29,4
dc.l	f_gldots,f_glline
f_gldots:
dc.w	-4000,0,-3000,1300,-2500,0,-3250,0,-3000,250,-3250,500,-3500,0,-2500,-1300
dc.w	-2750,1300,-1000,1300,-2000,750,-1500,-1300
dc.w	-1250,0,500,1300,-250,250,500,0,-250,-250,500,-1300
dc.w	750,1300,1500,-250,2250,1300,2500,-1300,2000,0,1250,-1300
dc.w	2750,-1300,3500,-750,2500,1300,4000,1300,3250,750,4000,-1300
f_glline:
dc.w	7,0,1,2,3,4,5,6,7,0
dc.w	3,8,9,10,11,8
dc.w	5,12,13,14,15,16,17,12
dc.w	5,18,19,20,21,22,23,18
dc.w	5,24,25,26,27,28,29,24

f_96:
dc.w	-80,94,920
dc.w	14,0,0
dc.w	128,128,128
dc.w	17,3
dc.l	f_96dots,f_96line
f_96dots:
dc.w	-800,600,0,-200,-400,-600,-800,-200,-600,100
dc.w	-400,0,-200,-200,-400,-400,-600,-200
dc.w	0,200,400,600,800,200,600,-100,800,-600
dc.w	400,400,600,200,400,0,200,200
f_96line:
dc.w	4,0,1,2,3,4,0
dc.w	3,5,6,7,8,5
dc.w	4,9,10,11,12,13,9
dc.w	3,14,15,16,17,14

f_faces:
dc.w	180,84,1022
dc.w	0,0,0
dc.w	128,128,128
dc.w	28,4
dc.l	f_fadots,f_faline
f_fadots:
dc.w	-3000,1000,-2500,250,-2000,0,-2500,0,-2250,-500,-1250,-1000,-2500,-1000
dc.w	-2250,1000,-1000,0,-1000,750,-1750,1000,-750,1000,-750,-1000
dc.w	-500,0,750,1000,0,0,750,-1000
dc.w	500,0,1750,1000,1000,100,1500,0,1000,-100,1750,-1000
dc.w	2000,1000,3000,250,2500,-250,3000,-1000,2000,0,2750,250
f_faline:
dc.w	6,0,1,2,3,4,5,6,0
dc.w	5,7,8,9,10,11,12,7
dc.w	3,13,14,15,16,13
dc.w	5,17,18,19,20,21,22,17
dc.w	5,23,24,25,26,27,28,23

;---------------------------------------------------------------------
>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1

end:
