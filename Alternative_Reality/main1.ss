;		*****************************************
;		*     ALTERNATIVE REALITY - MAIN CODE	*
;		*	-------------------------	*
;		*					*
;		*	   Coding on xx.10.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org $28000
load $28000

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
type:	macro
	move.l	#\1,tp_coladres			;adres set-u
	move	#1,tp_permit
	endm

s:	move.l	a7,return
	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
	move	#$7fff,$dff096
	move	#$7fff,$dff09a
	bsr	mt_init
	move.l	$6c,oldlev
	move.l	#newlev,$6c
	move	#$c020,$dff09a
	move	#$83c0,$dff096

	bsr	l_zaczynaj

quit:	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	olddma,$dff096
	move	#$7fff,$dff09a
	move.l	oldlev,$6c
	bsr	mt_end
	move.l	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	move.l	return,a7
	rts

newlev:		movem.l	d0-d7/a0-a6,-(sp)
		bsr.L	tp_typer
		tst	v_start
		beq.s	new_novsin
		jsr	v_sineit
new_novsin:	bsr	mt_music
		movem.l	(sp)+,d0-d7/a0-a6
		btst	#6,$bfe001
		bne.s	nopress
		move	(sp)+,storee
		move.l	(sp)+,adress
		move.l	#quit,-(sp)
		move	storee,-(sp)
nopress:	move	#$20,$dff09c
		rte

;-------------------------------------------------------------------
storee:		dc.w	0
adress:		dc.l	0

oldint:		dc.w	0
olddma:		dc.w	0
return:		dc.l	0
oldlev:		dc.l	0
gfxname:	dc.b	'graphics.library',0,0
;-------------------------------------------------------------------
l_zaczynaj:
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	bsr	l_chgscr
	bsr	l_chgscr
	lea	l_szescian,a6		;tu nazwe figury
	bsr	l_turn
	bsr	l_vector
	bsr	l_chgscr
	bsr	l_vector

;bra	qq
	bsr	l_putc
	bsr	ww_set_logosy
	raster
	move.l	#l_copper,$dff080
		move	#60,l_counter
ww_set5:	raster
		subi	#1,l_counter
		bne.s	ww_set5

	move	#30,l_counter
l_control0:	raster
		bsr	l_putc
		bsr	l_chgscr
		bsr	l_vector
		subi	#1,l_counter
		bne.s	l_control0
		move	#-10,8(a6)
l_control:	raster
		bsr	l_putc
		bsr	l_chgscr
		bsr	l_vector
		addi	#8,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	l_control
		clr	8(a6)
		move	#1024,4(a6)
		bsr	l_putc
		bsr	l_chgscr
		bsr	l_vector

		bsr	m_musicvec
		bsr	ex_setup
		bsr	i_starframe
		bsr	f_tracevec
		bsr	s_scroll
		bsr	kl_krata
		lea	$dff000,a0
		move.l	#$ffffffff,$44(A0)

		jsr	r_rubbervec
qq:		move	#1,f_ktore
		bsr	f_tracevec
		jsr	g_glenz
		raster
		move.l	#g_coppe1,$dff080
		type	na_napis5
		move	#220,d7
ll_wtt:		raster
		dbf	d7,ll_wtt
		raster
		move.l	#g_coppe0,$dff080
		jsr	zoom_zoom
		jsr	l_glenz2		;MD
l_control2:	btst.b	#6,$bfe001
		bne	l_control2
		rts


;-------------------------------------------------------------------
l_putc:
	lea	l_colmatrix,a1
	lea	l_copper,a2
	move	#2,d7
l_plo:	move	(a1)+,d0
	bpl	l_pl2
	neg	d0
l_pl2:	lsl	#4,d0
	move	d0,2(a2)
	lsl	#4,d0
	ori	d0,2(a2)
	addi	#4,a2
	dbf	d7,l_plo
	rts

l_chgscr:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_screen,a1
		move	#1,d2
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
		move	#[2*l_heith*64]+[l_row/2],$58(a0)
		rts

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
l_copper:
dc.l	$1820000,$1840000,$1860000
	dc.l	$1800000
dc.l	$920030,$9400d8,$8e0171,$9037d1
l_screen:
dc.l	$e00007,$e20000,$e40007,$e60000+[l_heith*[l_row]]
dc.l	$1020000,$1080000,$10a0000
;	dc.l	$2801ff00,$1800a40
;	dc.l	$2901ff00,$1800000
dc.l	$2a01ff00,$01002300
dc.l	$ffdffffe
dc.l	$2a01ff00,$01000300
;	dc.l	$2a01ff00,$1800a40
;	dc.l	$2b01ff00,$1800000
dc.l	$fffffffe

;-------------------------------------------------------------------

ww_set_logosy:	move.l	#$417be,d0
		lea	bp1,a2
		bsr.L	setcop
		move.l	#$4563e,d0
		lea	bp2,a2
		bsr.L	setcop
		raster
		move.l	#copper2,$dff080
		move	#260,l_counter
ww_set1:	raster
		subi	#1,l_counter
		bne.s	ww_set1
		lea	cop22,a2
		bsr.L	ww_colin
		move	#75,l_counter
ww_set2:	raster
		subi	#1,l_counter
		bne.s	ww_set2
		lea	cop23,a2
		bsr.L	ww_colin
		move	#15,l_counter
ww_set3:	raster
		subi	#1,l_counter
		bne.s	ww_set3
		move	#14,d7
ww_set4:	raster
		addi	#$110,cop24+2
		dbf	d7,ww_set4
		rts

ww_colin:	move	#16,d0
ww_fadcol:	move.l	a2,a1
		move	#14,d3
ww_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq.s	ww_fad2
		subi	#1,2(a1)
ww_fad2:	move	2(a1),d1
		andi	#$f0,d1
		cmpi	#$f0,d1
		beq.s	ww_fad3
		addi	#$10,2(a1)
ww_fad3:	move	2(a1),d1
		andi	#$f00,d1
		cmpi	#$f00,d1
		beq.s	ww_fad4
		addi	#$100,2(a1)
ww_fad4:	adda	#4,a1
		dbf	d3,ww_fad1
		raster
		dbf	d0,ww_fadcol
		rts

setcop:		move	#3,d7
setc1:		move	d0,6(a2)
		swap d0
		move	d0,2(a2)
		swap	d0
		addi.l	#$22,d0
		addi.l	#8,a2
		dbf	d7,setc1
		rts
;---------------------------------------------------------------
copper2:	dc.w $008e,$4a81,$0090,$0cc1
		dc.w $0092,$0040,$0094,$00c0
		dc.w $0102,$0088,$0108,$0066,$010a,$0066
		dc.l	$2a01ff00
cop24:		dc.l $1800000
cop22:	dc.w	$0182,$0FFF,$0184,$0EEE,$0186,$0DDD
	dc.w	$0188,$0CCC,$018A,$0BBB,$018C,$0AAA,$018E,$0999
	dc.w	$0190,$0777,$0192,$0666,$0194,$0555,$0196,$0444
	dc.w	$0198,$0333,$019A,$0222,$019C,$0111,$019E,$0000
bp1:		dc.w $00e0,$0000,$00e2,$0000
		dc.w $00e4,$0000,$00e6,$0000
		dc.w $00e8,$0000,$00ea,$0000
		dc.w $00ec,$0000,$00ee,$0000
		dc.l $2c07fffe
		dc.l $1004300
		dc.w $ad07,$fffe,$0100,$0300
cop23:	dc.w	$0182,$0FFF,$0184,$0EEE,$0186,$0DDD
	dc.w	$0188,$0CCC,$018A,$0BBB,$018C,$0AAA,$018E,$0999
	dc.w	$0190,$0777,$0192,$0666,$0194,$0555,$0196,$0444
	dc.w	$0198,$0333,$019A,$0222,$019C,$0111,$019E,$0000
bp2:		dc.w $00e0,$0000,$00e2,$0000
		dc.w $00e4,$0000,$00e6,$0000
		dc.w $00e8,$0000,$00ea,$0000
		dc.w $00ec,$0000,$00ee,$0000
		dc.l	$b207fffe,$01004300
		dc.l	$ffdffffe
		dc.l	$2a01ff00,$1800000,$1000300
		dc.l	$fffffffe

;-------------------------------------------------------------------
l_plane=$71000
scron:		dc.l	l_plane
scroff:		dc.l	l_plane+[2*l_heith*l_row]
scrpnt:		dc.w	0

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
l_szescian:
dc.w	175,128,0
dc.w	0,0,0
dc.w	128,128,128
dc.w	7,5
dc.l	l_szdots,l_szline
dc.w	2
l_szdots:
dc.w	-200,200,-150
dc.w	200,200,-150
dc.w	200,-200,-150
dc.w	-200,-200,-150
dc.w	-200,200,-100
dc.w	200,200,-100
dc.w	200,-200,-100
dc.w	-200,-200,-100
dc.w	0,0,15,15,0,0,0,15,0
l_szline:
dc.w	1,3,0,1,2,3,0
dc.w	1,3,4,7,6,5,4
dc.w	2,3,0,3,7,4,0
dc.w	2,3,5,6,2,1,5
dc.w	3,3,2,6,7,3,2
dc.w	3,3,5,1,0,4,5

;-------------------------------------------------------------------

;		*****************************************
;		*	ALTERNATIVE REALITY TYPER	*
;		*****************************************

set:	macro					;start pionowy
dc.l	$1800000,$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
dc.l	$e00000+[tp_plane/$10000],$e20000+[tp_plane&$ffff]
dc.l	$e40000+[[tp_plane+[80*16]]/$10000],$e60000+[[tp_plane+[80*16]]&$ffff]
dc.l	$e80000+[[tp_plane+[80*32]]/$10000],$ea0000+[[tp_plane+[80*32]]&$ffff]
dc.l	$ec0000+[[tp_plane+[80*48]]/$10000],$ee0000+[[tp_plane+[80*48]]&$ffff]
dc.l	[\1*$1000000]!$1ff00,$100c300
dc.l	[[\1+$10]*$1000000]!$1ff00,$1000300
	endm

;-----------------------------TYPER ROUTINES---------------------------
tp_typer:	move	tp_permit,d0
		beq.L	tp_notyper
		subi.b	#1,tp_time
		bne.s	tp_notyper
		move.b	#1,tp_time
		cmpi	#2,d0
		beq.L	tp_print
		cmpi	#1,d0
		beq.L	tp_clear
		cmpi	#3,d0
		beq.s	tp_bright

		move.l	tp_coladres,a1
		move	#14,d3
tp_fad1:	move	6(a1),d0
		andi	#$f,d0
		beq.s	tp_fad2
		subi	#1,6(a1)
tp_fad2:	move	6(a1),d0
		andi	#$f0,d0
		beq.s	tp_fad3
		subi	#$10,6(a1)
tp_fad3:	move	6(a1),d0
		andi	#$f00,d0
		beq.s	tp_fad4
		subi	#$100,6(a1)
tp_fad4:	adda.l	#4,a1
		dbf	d3,tp_fad1
		subi.b	#1,tp_counter
		bpl.s	tp_notyper
		move	#1,tp_permit
		move.b	#16,tp_counter
tp_notyper:	rts

;---------------------------------------------------------------------
tp_bright:	move.l	tp_coladres,a1
		move	#14,d3
tp_bri1:	move	6(a1),d0
		andi	#$f,d0
		cmpi	#$f,d0
		beq.s	tp_bri2
		addi	#1,6(a1)
tp_bri2:	move	6(a1),d0
		andi	#$f0,d0
		cmpi	#$f0,d0
		beq.s	tp_bri3
		addi	#$10,6(a1)
tp_bri3:	move	6(a1),d0
		andi	#$f00,d0
		cmpi	#$f00,d0
		beq.s	tp_bri4
		addi	#$100,6(a1)
tp_bri4:	adda.l	#4,a1
		dbf	d3,tp_bri1
		subi.b	#1,tp_counter
		bpl.s	tp_notyper
		move	#4,tp_permit
		move.b	#16,tp_counter
		bra.s	tp_notyper

;---------------------------------------------------------------------
tp_print:	lea	tp_fonts,a2
		lea	tp_plane,a3
		move.l	tp_textadres,a1
		move	tp_offset,d1
		lea	(a3,d1.w),a3
tp_sign:	clr	d0
		move.b	(a1)+,d0
		beq.L	tp_end
		bmi.L	tp_endtext
		cmpi	#'i',d0
		beq.L	tp_foundi
		cmpi	#'!',d0
		beq.L	tp_foundii
		cmpi	#'.',d0
		beq.L	tp_foundiii

		lea	tp_table,a4
		clr.l	d6
		move	#30,d7
tp_find_char:	cmp.b	(a4,d6.w),d0
		beq.s	tp_found
		addq	#1,d6
		dbf	d7,tp_find_char
tp_found:	divu	#11,d6
		move	d6,d0
		mulu	#34*16,d0
		swap	d6
		mulu	#3,d6
		addi	d6,d0
		lea	(a2,d0.w),a2

		move	#3,d6
tp_copy_plane:	move	#15,d7
tp_copy_char:	move.b	(a2),(a3)
		move.b	1(a2),1(a3)
		move.b	2(a2),2(a3)
		addi.l	#34,a2
		addi.l	#80,a3
		dbf	d7,tp_copy_char
		addi.l	#34*32,a2
		dbf	d6,tp_copy_plane

		addi	#3,tp_offset
		addi.l	#1,tp_textadres
		rts
tp_end:		move.b	(a1),tp_time
		addi.l	#2,tp_textadres
		move	#3,tp_permit
		clr	tp_offset
		rts
tp_endtext:	clr	tp_permit
		addi.l	#1,tp_textadres
		rts
;-------------------------------------------------------------------
tp_foundi:	lea	tp_fonts+[[32*34]+15],a2
		bra.s	tp_copysmall
tp_foundii:	lea	tp_fonts+[[32*34]+16],a2
		bra.s	tp_copysmall
tp_foundiii:	lea	tp_fonts+[[32*34]+17],a2
tp_copysmall:	move	#3,d6
tp_copypl:	move	#15,d7
tp_copych:	move.b	(a2),(a3)
		addi.l	#34,a2
		addi.l	#80,a3
		dbf	d7,tp_copych
		addi.l	#34*32,a2
		dbf	d6,tp_copypl
		addi	#1,tp_offset
		addi.l	#1,tp_textadres
		rts

;-------------------------------------------------------------------
tp_clear:	move	#[80*16]-1,d7
		lea	tp_plane,a2
tp_cloop:	clr.l	(a2)+
		dbf	d7,tp_cloop
		lea	tp_fonts+[4*34*48],a1
		move.l	tp_coladres,a2
		move	#15,d7
tp_colloop:	move	(a1)+,2(a2)
		addq.l	#4,a2
		dbf	d7,tp_colloop
		move	#2,tp_permit
		rts

;-------------------------------------------------------------------
tp_permit:	dc.w	0
tp_time:	dc.b	1
tp_counter:	dc.b	16
tp_textadres:	dc.l	tp_text
tp_coladres:	dc.l	tp_text
tp_offset:	dc.w	0

tp_plane:	blk.l	16*80
tp_row=80
;---------------------------------------------------------------------

tp_text:
;	'                          '+2 bajty
dc.b	"         ...sit down...    ",0,10
dc.b	"           ...relax...      ",0,10,-1
dc.b	" ordinary rubber vector... ",0,100,-1
dc.b	" maybe you prefer this one? ",0,127,-1
dc.b	" fractals ??? ",0,25,-1
dc.b	" wooow!!! escaping fractals!!! ",0,20,-1
dc.b	"          hang on !!! ",0,15
dc.b	"     now we are getting... ",0,15
dc.b	"      ...pretty close ! ",0,15,-1

dc.b	" wonderful suspect product",0,20
dc.b	"    ....just enjoy youself....",0,40
dc.b	"   this little fine typer  ",0,30
dc.b	"   was coded of course by  ",0,30
dc.b	"      ...kane of suspect... ",0,127,-1
even
tp_table:	dc.b	"abcdefghjklmnopqrstuvwxyz?-i!. "
even
;-------------------------------------------------------------------
mt_lev6use=		1		; 0=NO, 1=YES
mt_finetuneused=	0		; 0=NO, 1=YES

mt_init	LEA	mt_data,A0
	MOVE.L	A0,mt_SongDataPtr
	LEA	250(A0),A1
	MOVE.W	#511,D0
	MOVEQ	#0,D1
mtloop	MOVE.L	D1,D2
	SUBQ.W	#1,D0
mtloop2	MOVE.B	(A1)+,D1
	CMP.W	D2,D1
	BGT.S	mtloop
	DBRA	D0,mtloop2
	ADDQ	#1,D2

	MOVE.W	D2,D3
	MULU	#128,D3
	ADD.L	#766,D3
	ADD.L	mt_SongDataPtr(PC),D3
	MOVE.L	D3,mt_LWTPtr

	LEA	mt_SampleStarts(PC),A1
	MULU	#128,D2
	ADD.L	#762,D2
	ADD.L	(A0,D2.L),D2
	ADD.L	mt_SongDataPtr(PC),D2
	ADDQ.L	#4,D2
	MOVE.L	D2,A2
	MOVEQ	#30,D0
mtloop3	MOVE.L	A2,(A1)+
	MOVEQ	#0,D1
	MOVE.W	(A0),D1
	ADD.L	D1,D1
	ADD.L	D1,A2
	LEA	8(A0),A0
	DBRA	D0,mtloop3

	OR.B	#2,$BFE001
	lea	mt_speed(PC),A4
	MOVE.B	#6,(A4)
	CLR.B	mt_counter-mt_speed(A4)
	CLR.B	mt_SongPos-mt_speed(A4)
	CLR.W	mt_PatternPos-mt_speed(A4)
mt_end	LEA	$DFF096,A0
	CLR.W	$12(A0)
	CLR.W	$22(A0)
	CLR.W	$32(A0)
	CLR.W	$42(A0)
	MOVE.W	#$F,(A0)
	RTS

mt_music
	MOVEM.L	D0-D4/D7/A0-A6,-(SP)
	ADDQ.B	#1,mt_counter
	MOVE.B	mt_counter(PC),D0
	CMP.B	mt_speed(PC),D0
	BLO.S	mt_NoNewNote
	CLR.B	mt_counter
	TST.B	mt_PattDelTime2
	BEQ.S	mt_GetNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_dskip

mt_NoNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_NoNewPosYet

mt_NoNewAllChannels
	LEA	$dff090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BRA.W	mt_CheckEfx

mt_GetNewNote
	MOVE.L	mt_SongDataPtr(PC),A0
	LEA	(A0),A3
	LEA	122(A0),A2	;pattpo
	LEA	762(A0),A0	;patterndata
	CLR.W	mt_DMACONtemp

	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.S	mt_DoVoice
	BSR.S	mt_DoVoice
	BSR.s	mt_DoVoice
	BSR.s	mt_DoVoice
	BRA.W	mt_SetDMA

mt_DoVoice
	MOVEQ	#0,D0
	MOVEQ	#0,D1
	MOVE.B	mt_SongPos(PC),D0
	LEA	128(A2),A2
	MOVE.B	(A2,D0.W),D1
	MOVE.W	mt_PatternPos(PC),D2
	LSL	#7,D1
	LSR.W	#1,D2
	ADD.W	D2,D1
	LEA	44(A6),A6
	lea	$10(a5),a5

	TST.L	(A6)
	BNE.S	mt_plvskip
	BSR.W	mt_PerNop
mt_plvskip
	MOVE.W	(A0,D1.W),D1
	LSL.W	#2,D1
	MOVE.L	A0,-(sp)
	MOVE.L	mt_LWTPtr(PC),A0
	MOVE.L	(A0,D1.W),(A6)
	MOVE.L	(sp)+,A0
	MOVE.B	2(A6),D2
	AND.L	#$F0,D2
	LSR.B	#4,D2
	MOVE.B	(A6),D0
	AND.B	#$F0,D0
	OR.B	D0,D2
	BEQ.s	mt_SetRegs
	MOVEQ	#0,D3
	LEA	mt_SampleStarts(PC),A1
	SUBQ	#1,D2
	MOVE	D2,D4
	ADD	D2,D2
	ADD	D2,D2
	LSL	#3,D4
	MOVE.L	(A1,D2.L),4(A6)
	MOVE.W	(A3,D4.W),8(A6)
	MOVE.W	(A3,D4.W),40(A6)
	MOVE.W	2(A3,D4.W),18(A6)
	MOVE.L	4(A6),D2	; Get start
	MOVE.W	4(A3,D4.W),D3	; Get repeat
	BEQ.S	mt_NoLoop
	MOVE.W	D3,D0		; Get repeat
	ADD.W	D3,D3
	ADD.L	D3,D2		; Add repeat
	ADD.W	6(A3,D4.W),D0	; Add replen
	MOVE.W	D0,8(A6)

mt_NoLoop
	MOVE.L	D2,10(A6)
	MOVE.L	D2,36(A6)
	MOVE.W	6(A3,D4.W),14(A6)	; Save replen
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
mt_SetRegs
	MOVE.W	(A6),D0
	AND.W	#$0FFF,D0
	BEQ.W	mt_CheckMoreEfx	; If no note

	IF mt_finetuneused=1
	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0E50,D0
	BEQ.S	mt_DoSetFineTune
	ENDif

	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#3,D0	; TonePortamento
	BEQ.S	mt_ChkTonePorta
	CMP.B	#5,D0
	BEQ.S	mt_ChkTonePorta
	CMP.B	#9,D0	; Sample Offset
	BNE.S	mt_SetPeriod
	BSR.W	mt_CheckMoreEfx
	BRA.S	mt_SetPeriod

mt_ChkTonePorta
	BSR.W	mt_SetTonePorta
	BRA.W	mt_CheckMoreEfx

mt_DoSetFineTune
	BSR.W	mt_SetFineTune

mt_SetPeriod
	MOVEM.L	D1/A1,-(SP)
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1

	IF mt_finetuneused=0
	MOVE.W	D1,16(A6)

	ELSE
mt_SetPeriod2
	LEA	mt_PeriodTable(PC),A1
	MOVEQ	#36,D7
mt_ftuloop
	CMP.W	(A1)+,D1
	BHS.S	mt_ftufound
	DBRA	D7,mt_ftuloop
mt_ftufound
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D0
	LSL	#3,D1
	ADD	D0,D1
	MOVE.W	-2(A1,D1.W),16(A6)
	ENDif

	MOVEM.L	(SP)+,D1/A1

	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0ED0,D0 ; Notedelay
	BEQ.W	mt_CheckMoreEfx

	MOVE.W	20(A6),$DFF096
	BTST	#2,30(A6)
	BNE.S	mt_vibnoc
	CLR.B	27(A6)
mt_vibnoc
	BTST	#6,30(A6)
	BNE.S	mt_trenoc
	CLR.B	29(A6)
mt_trenoc
	MOVE.L	4(A6),(A5)	; Set start
	MOVE.W	8(A6),4(A5)	; Set length
	MOVE.W	16(A6),6(A5)	; Set period
	MOVE.W	20(A6),D0
	OR.W	D0,mt_DMACONtemp
	BRA.W	mt_CheckMoreEfx
 
mt_SetDMA
	IF mt_lev6use=1
	lea	$bfd000,a3
	move.b	#$7f,$d00(a3)
	move.w	#$2000,$dff09c
	move.w	#$a000,$dff09a
	move.l	$78.w,mt_oldirq
	move.l	#mt_irq1,$78.w
	moveq	#0,d0
	move.b	d0,$e00(a3)
	move.b	#$a8,$400(a3)
	move.b	d0,$500(a3)
	move.b	#$11,$e00(a3)
	move.b	#$81,$d00(a3)
	OR.W	#$8000,mt_DMACONtemp
	BRA.w	mt_dskip

	ELSE
	OR.W	#$8000,mt_DMACONtemp
	bsr.w	mt_WaitDMA
	ENDif

	IF mt_lev6use=1
mt_irq1:tst.b	$bfdd00
	MOVE.W	mt_dmacontemp(pc),$DFF096
	move.w	#$2000,$dff09c
	move.l	#mt_irq2,$78.w
	rte

	ELSE
	MOVE.W	mt_dmacontemp(pc),$DFF096
	bsr.w	mt_WaitDMA
	ENDif

	IF mt_lev6use=1
mt_irq2:tst.b	$bfdd00
	movem.l	a5-a6,-(a7)
	ENDif

	LEA	$DFF0A0,A5
	LEA	mt_chan1temp(PC),A6
	MOVE.L	10(A6),(A5)
	MOVE.W	14(A6),4(A5)
	MOVE.L	54(A6),$10(A5)
	MOVE.W	58(A6),$14(A5)
	MOVE.L	98(A6),$20(A5)
	MOVE.W	102(A6),$24(A5)
	MOVE.L	142(A6),$30(A5)
	MOVE.W	146(A6),$34(A5)

	IF mt_lev6use=1
	move.b	#0,$bfde00
	move.b	#$7f,$bfdd00
	move.l	mt_oldirq(pc),$78.w
	move.w	#$2000,$dff09c
	movem.l	(a7)+,a5-a6
	rte
	ENDif

mt_dskip
	lea	mt_speed(PC),A4
	ADDQ.W	#4,mt_PatternPos-mt_speed(A4)
	MOVE.B	mt_PattDelTime-mt_speed(A4),D0
	BEQ.S	mt_dskc
	MOVE.B	D0,mt_PattDelTime2-mt_speed(A4)
	CLR.B	mt_PattDelTime-mt_speed(A4)
mt_dskc	TST.B	mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.B	#1,mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.W	#4,mt_PatternPos-mt_speed(A4)
mt_dska	TST.B	mt_PBreakFlag-mt_speed(A4)
	BEQ.S	mt_nnpysk
	SF	mt_PBreakFlag-mt_speed(A4)
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	CLR.B	mt_PBreakPos-mt_speed(A4)
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
mt_nnpysk
	CMP.W	#256,mt_PatternPos-mt_speed(A4)
	BLO.S	mt_NoNewPosYet
mt_NextPosition	
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
	CLR.B	mt_PBreakPos-mt_speed(A4)
	CLR.B	mt_PosJumpFlag-mt_speed(A4)
	ADDQ.B	#1,mt_SongPos-mt_speed(A4)
	AND.B	#$7F,mt_SongPos-mt_speed(A4)
	MOVE.B	mt_SongPos(PC),D1
	MOVE.L	mt_SongDataPtr(PC),A0
	CMP.B	248(A0),D1
	BLO.S	mt_NoNewPosYet
	CLR.B	mt_SongPos-mt_speed(A4)
mt_NoNewPosYet
	lea	mt_speed(PC),A4
	TST.B	mt_PosJumpFlag-mt_speed(A4)
	BNE.S	mt_NextPosition

	lea	mt_volumes,a1			;control volume
	lea	$dff0a0,a2
	move	mt_percent,d0
	move	#3,d7
mt_putvol:
	move	(a1)+,d1
	mulu	d0,d1
	lsr	#7,d1
	move	d1,8(a2)
	lea	$10(a2),a2
	dbf	d7,mt_putvol
	MOVEM.L	(SP)+,D0-D4/D7/A0-A6
	RTS

mt_CheckEfx
	lea	$10(a5),a5
	lea	44(a6),a6
	BSR.W	mt_UpdateFunk
	MOVE.W	2(A6),D0
	AND.W	#$0FFF,D0
	BEQ.S	mt_PerNop
	MOVE.B	2(A6),D0
	MOVEQ	#$0F,D1
	AND.L	D1,D0
	BEQ.S	mt_Arpeggio
	SUBQ	#1,D0
	BEQ.W	mt_PortaUp
	SUBQ	#1,D0
	BEQ.W	mt_PortaDown
	SUBQ	#1,D0
	BEQ.W	mt_TonePortamento
	SUBQ	#1,D0
	BEQ.W	mt_Vibrato
	SUBQ	#1,D0
	BEQ.W	mt_TonePlusVolSlide
	SUBQ	#1,D0
	BEQ.W	mt_VibratoPlusVolSlide
	SUBQ	#8,D0
	BEQ.W	mt_E_Commands
SetBack	MOVE.W	16(A6),6(A5)
	ADDQ	#7,D0
	BEQ.W	mt_Tremolo
	SUBQ	#3,D0
	BEQ.W	mt_VolumeSlide
mt_Return2
	RTS

mt_PerNop
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio
	MOVEQ	#0,D0
	MOVE.B	mt_counter(PC),D0
	DIVS	#3,D0
	SWAP	D0
	TST.W	D0
	BEQ.S	mt_Arpeggio2
	SUBQ	#2,D0
	BEQ.S	mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio2
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio1
	MOVE.B	3(A6),D0
	AND.W	#15,D0
mt_Arpeggio3
	ADD.W	D0,D0
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D2
	LSL	#3,D1
	ADD	D2,D1
	ADD.L	D1,A0
	ENDif

	MOVE.W	16(A6),D1
	MOVEQ	#36,D7
mt_arploop
	CMP.W	(A0)+,D1
	BHS.S	mt_Arpeggio4
	DBRA	D7,mt_arploop
	RTS

mt_Arpeggio4
	MOVE.W	-2(A0,D0.W),6(A5)
	RTS

mt_FinePortaUp
	TST.B	mt_counter
	BNE.S	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaUp
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	SUB.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#113,D0
	BPL.S	mt_PortaUskip
	AND.W	#$F000,16(A6)
	OR.W	#113,16(A6)
mt_PortaUskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS	
 
mt_FinePortaDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaDown
	CLR.W	D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	ADD.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#856,D0
	BMI.S	mt_PortaDskip
	AND.W	#$F000,16(A6)
	OR.W	#856,16(A6)
mt_PortaDskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS

mt_SetTonePorta
	MOVE.L	A0,-(SP)
	MOVE.W	(A6),D2
	AND.W	#$0FFF,D2
	LEA	mt_PeriodTable(PC),A0

	IF	mt_finetuneused=1
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	ADD	D0,D0
	MOVE	D0,D7
	ADD	D0,D0
	ADD	D0,D0
	ADD	D0,D7
	LSL	#3,D0
	ADD	D7,D0
	ADD.L	D0,A0
	ENDif

	MOVEQ	#0,D0
mt_StpLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_StpFound
	ADDQ	#2,D0
	CMP.W	#37*2,D0
	BLO.S	mt_StpLoop
	MOVEQ	#35*2,D0
mt_StpFound
	BTST	#3,18(A6)
	BEQ.S	mt_StpGoss
	TST.W	D0
	BEQ.S	mt_StpGoss
	SUBQ	#2,D0
mt_StpGoss
	MOVE.W	(A0,D0.W),D2
	MOVE.L	(SP)+,A0
	MOVE.W	D2,24(A6)
	MOVE.W	16(A6),D0
	CLR.B	22(A6)
	CMP.W	D0,D2
	BEQ.S	mt_ClearTonePorta
	BGE.W	mt_Return2
	MOVE.B	#1,22(A6)
	RTS

mt_ClearTonePorta
	CLR.W	24(A6)
	RTS

mt_TonePortamento
	MOVE.B	3(A6),D0
	BEQ.S	mt_TonePortNoChange
	MOVE.B	D0,23(A6)
	CLR.B	3(A6)
mt_TonePortNoChange
	TST.W	24(A6)
	BEQ.W	mt_Return2
	MOVEQ	#0,D0
	MOVE.B	23(A6),D0
	TST.B	22(A6)
	BNE.S	mt_TonePortaUp
mt_TonePortaDown
	ADD.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BGT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)
	BRA.S	mt_TonePortaSetPer

mt_TonePortaUp
	SUB.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BLT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)

mt_TonePortaSetPer
	MOVE.W	16(A6),D2
	MOVE.B	31(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_GlissSkip
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	LSL	#3,D0
	MOVE	D0,D1
	LSL	#3,D0
	ADD	D1,D0
	ADD.L	D0,A0
	ENDif

	MOVEQ	#0,D0
mt_GlissLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_GlissFound
	ADDQ	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_GlissLoop
	MOVEQ	#35*2,D0
mt_GlissFound
	MOVE.W	(A0,D0.W),D2
mt_GlissSkip
	MOVE.W	D2,6(A5) ; Set period
	RTS

mt_Vibrato
	MOVE.B	3(A6),D0
	BEQ.S	mt_Vibrato2
	MOVE.B	26(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_vibskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_vibskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_vibskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_vibskip2
	MOVE.B	D2,26(A6)
mt_Vibrato2
	MOVE.B	27(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVE.B	30(A6),D2
	AND.W	#$03,D2
	BEQ.S	mt_vib_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_vib_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_vib_set
mt_vib_rampdown
	TST.B	27(A6)
	BPL.S	mt_vib_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_sine
	MOVE.B	0(A4,D0.W),D2
mt_vib_set
	MOVE.B	26(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#7,D2
	MOVE.W	16(A6),D0
	TST.B	27(A6)
	BMI.S	mt_VibratoNeg
	ADD.W	D2,D0
	BRA.S	mt_Vibrato3
mt_VibratoNeg
	SUB.W	D2,D0
mt_Vibrato3
	MOVE.W	D0,6(A5)
	MOVE.B	26(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,27(A6)
	RTS

mt_TonePlusVolSlide
	BSR.W	mt_TonePortNoChange
	BRA.W	mt_VolumeSlide

mt_VibratoPlusVolSlide
	BSR.S	mt_Vibrato2
	BRA.W	mt_VolumeSlide

mt_Tremolo
	MOVE.B	3(A6),D0
	BEQ.S	mt_Tremolo2
	MOVE.B	28(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_treskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_treskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_treskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_treskip2
	MOVE.B	D2,28(A6)
mt_Tremolo2
	MOVE.B	29(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	30(A6),D2
	LSR.B	#4,D2
	AND.B	#$03,D2
	BEQ.S	mt_tre_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_tre_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_tre_set
mt_tre_rampdown
	TST.B	27(A6)
	BPL.S	mt_tre_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_sine
	MOVE.B	0(A4,D0.W),D2
mt_tre_set
	MOVE.B	28(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#6,D2
	MOVEQ	#0,D0
	MOVE.B	19(A6),D0
	TST.B	29(A6)
	BMI.S	mt_TremoloNeg
	ADD.W	D2,D0
	BRA.S	mt_Tremolo3
mt_TremoloNeg
	SUB.W	D2,D0
mt_Tremolo3
	BPL.S	mt_TremoloSkip
	CLR.W	D0
mt_TremoloSkip
	CMP.W	#$40,D0
	BLS.S	mt_TremoloOk
	MOVE.W	#$40,D0
mt_TremoloOk
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		MOVE	D0,(A5,d1.w)
		movem.l	(sp)+,a5/d1
	MOVE.B	28(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,29(A6)
	RTS

mt_SampleOffset
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	BEQ.S	mt_sononew
	MOVE.B	D0,32(A6)
mt_sononew
	MOVE.B	32(A6),D0
	LSL.W	#7,D0
	CMP.W	8(A6),D0
	BGE.S	mt_sofskip
	SUB.W	D0,8(A6)
	ADD.W	D0,D0
	ADD.L	D0,4(A6)
	RTS
mt_sofskip
	MOVE.W	#$0001,8(A6)
	RTS

mt_VolumeSlide
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	TST.B	D0
	BEQ.S	mt_VolSlideDown
mt_VolSlideUp
	ADD.B	D0,19(A6)
	CMP.B	#$40,19(A6)
	BMI.S	mt_vsuskip
	MOVE.B	#$40,19(A6)
mt_vsuskip
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_VolSlideDown
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
mt_VolSlideDown2
	SUB.B	D0,19(A6)
	BPL.S	mt_vsdskip
	CLR.B	19(A6)
mt_vsdskip
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_PositionJump
	MOVE.B	3(A6),D0
	SUBQ	#1,D0
	MOVE.B	D0,mt_SongPos
mt_pj2	CLR.B	mt_PBreakPos
	ST 	mt_PosJumpFlag
	RTS

mt_VolumeChange
	MOVE.B	3(A6),D0
	CMP.B	#$40,D0
	BLS.S	mt_VolumeOk
	MOVEQ	#$40,D0
mt_VolumeOk
	MOVE.B	D0,19(A6)
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		MOVE.b	D0,1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_PatternBreak
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	MOVE.W	D0,D2
	LSR.B	#4,D0
	ADD	D0,D0
	MOVE	D0,D1
	ADD	D0,D0
	ADD	D0,D0
	ADD	D1,D0
	AND.B	#$0F,D2
	ADD.B	D2,D0
	CMP.B	#63,D0
	BHI.S	mt_pj2
	MOVE.B	D0,mt_PBreakPos
	ST	mt_PosJumpFlag
	RTS

mt_SetSpeed
	MOVE.B	3(A6),D0
	BEQ.W	mt_Return2
	CLR.B	mt_counter
	MOVE.B	D0,mt_speed
	RTS

mt_CheckMoreEfx
	BSR.W	mt_UpdateFunk
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	SUB.B	#9,D0
	BEQ.W	mt_SampleOffset
	SUBQ	#2,D0
	BEQ.W	mt_PositionJump
	SUBQ	#1,D0
	BEQ.L	mt_VolumeChange
	SUBQ	#1,D0
	BEQ.S	mt_PatternBreak
	SUBQ	#1,D0
	BEQ.S	mt_E_Commands
	SUBQ	#1,D0
	BEQ.S	mt_SetSpeed
	BRA.W	mt_PerNop

mt_E_Commands
	MOVE.B	3(A6),D0
	AND.W	#$F0,D0
	LSR.B	#4,D0
	BEQ.S	mt_FilterOnOff
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaUp
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaDown
	SUBQ	#1,D0
	BEQ.S	mt_SetGlissControl
	SUBQ	#1,D0
	BEQ.s	mt_SetVibratoControl

	IF mt_finetuneused=1
	SUBQ	#1,D0
	BEQ.s	mt_SetFineTune
	SUBQ	#1,D0

	ELSE
	SUBQ	#2,D0
	ENDif

	BEQ.s	mt_JumpLoop
	SUBQ	#1,D0
	BEQ.W	mt_SetTremoloControl
	SUBQ	#2,D0
	BEQ.W	mt_RetrigNote
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineUp
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineDown
	SUBQ	#1,D0
	BEQ.W	mt_NoteCut
	SUBQ	#1,D0
	BEQ.W	mt_NoteDelay
	SUBQ	#1,D0
	BEQ.W	mt_PatternDelay
	BRA.W	mt_FunkIt

mt_FilterOnOff
	MOVE.B	3(A6),D0
	AND.B	#1,D0
	ADD.B	D0,D0
	AND.B	#$FD,$BFE001
	OR.B	D0,$BFE001
	RTS	

mt_SetGlissControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,31(A6)
	OR.B	D0,31(A6)
	RTS

mt_SetVibratoControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_SetFineTune
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	MOVE.B	D0,18(A6)
	RTS

mt_JumpLoop
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_SetLoop
	TST.B	34(A6)
	BEQ.S	mt_jumpcnt
	SUBQ.B	#1,34(A6)
	BEQ.W	mt_Return2
mt_jmploop 	MOVE.B	33(A6),mt_PBreakPos
	ST	mt_PBreakFlag
	RTS

mt_jumpcnt
	MOVE.B	D0,34(A6)
	BRA.S	mt_jmploop

mt_SetLoop
	MOVE.W	mt_PatternPos(PC),D0
	LSR	#2,D0
	MOVE.B	D0,33(A6)
	RTS

mt_SetTremoloControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_RetrigNote
	MOVE.L	D1,-(SP)
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BEQ.S	mt_rtnend
	MOVEQ	#0,d1
	MOVE.B	mt_counter(PC),D1
	BNE.S	mt_rtnskp
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	BNE.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter(PC),D1
mt_rtnskp
	DIVU	D0,D1
	SWAP	D1
	TST.W	D1
	BNE.S	mt_rtnend
mt_DoRetrig
	MOVE.W	20(A6),$DFF096	; Channel DMA off
	MOVE.L	4(A6),(A5)	; Set sampledata pointer
	MOVE.W	8(A6),4(A5)	; Set length
	BSR.W	mt_WaitDMA
	MOVE.W	20(A6),D0
	BSET	#15,D0
	MOVE.W	D0,$DFF096
	BSR.W	mt_WaitDMA
	MOVE.L	10(A6),(A5)
	MOVE.L	14(A6),4(A5)
mt_rtnend
	MOVE.L	(SP)+,D1
	RTS

mt_VolumeFineUp
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$F,D0
	BRA.W	mt_VolSlideUp

mt_VolumeFineDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BRA.W	mt_VolSlideDown2

mt_NoteCut
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_counter(PC),D0
	BNE.W	mt_Return2
	CLR.B	19(A6)
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		clr	(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_NoteDelay
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_Counter(PC),D0
	BNE.W	mt_Return2
	MOVE.W	(A6),D0
	BEQ.W	mt_Return2
	MOVE.L	D1,-(SP)
	BRA.W	mt_DoRetrig

mt_PatternDelay
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	TST.B	mt_PattDelTime2
	BNE.W	mt_Return2
	ADDQ.B	#1,D0
	MOVE.B	D0,mt_PattDelTime
	RTS

mt_FunkIt
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,31(A6)
	OR.B	D0,31(A6)
	TST.B	D0
	BEQ.W	mt_Return2
mt_UpdateFunk
	MOVEM.L	D1/A0,-(SP)
	MOVEQ	#0,D0
	MOVE.B	31(A6),D0
	LSR.B	#4,D0
	BEQ.S	mt_funkend
	LEA	mt_FunkTable(PC),A0
	MOVE.B	(A0,D0.W),D0
	ADD.B	D0,35(A6)
	BTST	#7,35(A6)
	BEQ.S	mt_funkend
	CLR.B	35(A6)

	MOVE.L	10(A6),D0
	MOVEQ	#0,D1
	MOVE.W	14(A6),D1
	ADD.L	D1,D0
	ADD.L	D1,D0
	MOVE.L	36(A6),A0
	ADDQ.L	#1,A0
	CMP.L	D0,A0
	BLO.S	mt_funkok
	MOVE.L	10(A6),A0
mt_funkok
	MOVE.L	A0,36(A6)
	NEG.B	(A0)
	SUBQ.B	#1,(A0)
mt_funkend
	MOVEM.L	(SP)+,D1/A0
	RTS

mt_WaitDMA
	MOVEQ	#3,D0
mt_WaitDMA2
	MOVE.B	$DFF006,D1
mt_WaitDMA3
	CMP.B	$DFF006,D1
	BEQ.S	mt_WaitDMA3
	DBF	D0,mt_WaitDMA2
	RTS

mt_FunkTable dc.b 0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable	
	dc.b   0, 24, 49, 74, 97,120,141,161
	dc.b 180,197,212,224,235,244,250,253
	dc.b 255,253,250,244,235,224,212,197
	dc.b 180,161,141,120, 97, 74, 49, 24

mt_PeriodTable
; Tuning 0, Normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114

mt_chan1temp	blk.l	5
		dc.w	1
		blk.w	21
		dc.w	2
		blk.w	21
		dc.w	4
		blk.w	21
		dc.w	8
		blk.w	11

mt_SampleStarts	blk.l	31,0

mt_SongDataPtr	dc.l 0
mt_LWTPtr	dc.l 0
mt_oldirq	dc.l 0

mt_speed	dc.b 6
mt_counter	dc.b 0
mt_SongPos	dc.b 0
mt_PBreakPos	dc.b 0
mt_PosJumpFlag	dc.b 0
mt_PBreakFlag	dc.b 0
mt_LowMask	dc.b 0
mt_PattDelTime	dc.b 0
mt_PattDelTime2	dc.b 0,0
mt_PatternPos	dc.w 0
mt_DMACONtemp	dc.w 0
mt_volumes:	dc.w 0,0,0,0
mt_percent:	dc.w 128

;-------------------------------------------------------------------
m_negtab:
dc.w	-38,38,-38
dc.w	38,38,-38
dc.w	38,-38,-38
dc.w	-38,-38,-38

dc.w	-38,38,38
dc.w	38,38,38
dc.w	38,-38,38
dc.w	-38,-38,38
;-------------------------------------------------------------------
m_musicvec:	bsr	m_chgscr
		bsr	m_chgscr
		lea	m_szescian,a6		;tu nazwe figury
		bsr	m_turn
		bsr	m_vector
		move.l	#m_copper,$dff080
		move	#300,l_counter
m_control2:	raster
		bsr	m_putc
		bsr	m_chgscr
		bsr	m_vector
		cmpi	#270,4(a6)
		beq.s	m_con2_0
		subi	#10,4(a6)

m_con2_0:	lea	mt_volumes,a3
		move	#2,d7
m_con2_1:	move	(a3)+,d0
		lea	m_negtab,a1
		lea	m_szdots,a2
		move	d7,d6
		lsl	#1,d6
		lea	(a1,d6.w),a1
		lea	(a2,d6.w),a2
		lsr	#1,d0
		addi	#38,d0
		bsr	m_con2_set
		dbf	d7,m_con2_1

		subi	#1,l_counter
		bne.s	m_control2
		rts

;-------------------------------------------------------------------
m_con2_set:	move	#7,d6
m_con2_2:	move	d0,d1
		tst	(a1)
		bpl	m_con2_okplus
		neg	d1
m_con2_okplus:	move	d1,(a2)
		addi	#6,a1
		addi	#6,a2
		dbf	d6,m_con2_2
		rts
;-------------------------------------------------------------------
m_putc:		lea	l_colmatrix,a1
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
	lea	l_matrix,a4
	move.l	26(a6),a3
m_obr1:
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
	move	l_col,d4
	move.l	scroff,a5
	btst	#0,d4
	beq	m_bpl2
	bsr	m_draw
m_bpl2:	addi.l	#[m_heith*m_row],a5
	move	l_col,d4
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
	lea	l_sinus,a1
	lea	l_sinus+128,a3		;cosinus
	lea	l_matrix,a4
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
	lea	l_colmatrix,a4
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
	lea	l_octant,a2
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
;-------------------------------------------------------------------
m_copper:
dc.l	$1820000,$1840000,$1860000
	dc.l	$1800000
dc.l	$920058,$9400b0,$8e0171,$9037d1
m_screen:
dc.l	$e00007,$e20000,$e40007,$e60000+[m_heith*[m_row]]
dc.l	$1020000,$1080000,$10a0000
dc.l	$4601ff00,$01002300
dc.l	$ffdffffe
dc.l	$0401ff00,$01000300
dc.l	$fffffffe

;-------------------------------------------------------------------
m_heith=190
m_row=24	;16
;-------------------------------------------------------------------
m_szescian:
dc.w	96,98,1020		;min 270
dc.w	-8,-6,4
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

;-------------------------------------------------------------------
ex_setup:
	waitblt
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	#ex_plane,$54(a0)
	move	#[2*ex_heith*64]+[ex_row],$58(a0)
	move.l	#ex_plane,scron
	move.l	#ex_plane+[ex_heith*ex_row],scroff
	waitblt
	bsr	ex_makedod
	bsr	ex_explode
	raster
	move.l	#ex_copper,$dff080

ex_control:	raster
		bsr.s	ex_explode
		cmpi	#550,ex_away
		bmi.s	ex_control

ex_control1:	raster
		bsr.s	ex_explode
		raster
		bsr.s	ex_explode
		lea	ex_copper,a1
		move	#14,d7
ex_con12:	tst	2(a1)
		beq	ex_con22
		subi	#$111,2(a1)
ex_con22:	addi.l	#4,a1
		dbf	d7,ex_con12
		cmpi	#800,ex_away
		bmi.s	ex_control1
		rts

;---------------------------------------------------------------------
ex_explode:
		move.l	scron,d0
		move.l	scroff,d1
		move.l	scrclr,d2
		move.l	scrlast,d3
		move.l	d3,scrclr
		move.l	d2,scroff
		move.l	d1,scron
		move.l	d0,scrlast
		move	d1,ex_screen+6
		swap	d1
		move	d1,ex_screen+2
		swap	d1
		addi.l	#[ex_heith*ex_row],d1		;2
		move	d1,ex_screen+14
		swap	d1
		move	d1,ex_screen+10
		move	d0,ex_screen+22			;3
		swap	d0
		move	d0,ex_screen+18
		swap	d0
		addi.l	#[ex_heith*ex_row],d0		;4
		move	d0,ex_screen+30
		swap	d0
		move	d0,ex_screen+26

		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d3,$54(a0)
		move	#[ex_heith*64]+[ex_row/2],$58(a0)

;---------------------------------------------------------------------
		lea	ex_pos,a1
		lea	ex_pos+[4*ex_ilosc],a2
		move.l	scroff,a5
		lea	ex_away,a3
		movem	(a3),d3-d5
		addi	#10,(a3)+
		addi	#8,(a3)+
		addi	#6,(a3)
		move	#ex_ilosc-1,d7
ex_stars:	movem	(a1)+,d0/d1
		muls	d4,d0
		asr.l	#8,d0
		muls	d4,d1
		asr.l	#8,d1
		addi	#176,d0
		addi	#124,d1
		exg	d3,d5
		exg	d3,d4

		tst	d0
		bmi.s	ex_nostar
		tst	d1
		bmi.s	ex_nostar
		cmpi	#ex_row*8,d0
		bpl.s	ex_nostar
		cmpi	#ex_heith,d1
		bpl.s	ex_nostar

		move	d1,d2
		move	d1,d6
		lsl	#5,d1
		lsl	#3,d2		;mulu 44
		lsl	#2,d6
		addi	d2,d1
		addi	d6,d1
		move	d0,d2
		lsr	#3,d2
		andi	#7,d0
		eori	#7,d0
		addi	d2,d1
		bset.b	d0,(a5,d1.w)
ex_nostar:	dbf	d7,ex_stars
		rts

ex_away:	dc.w	10,10,10
;---------------------------------------------------------------------
ex_makedod:
		lea	ex_pos,a1
		lea	ex_sine,a2
		lea	$ff0000,a3
		lea	$ff0040,a4
		move	#[2*ex_ilosc]-1,d7
		raster
		clr	d1
		clr	d2
ex_make1:	clr	d0
		move	$dff006,d0
		add.b	$dff005,d0
		addi	(a2,d2.w),d0
		add.b	(a4)+,d0
		sub.b	(a3)+,d0
		andi	#$1ff,d0
		ror.l	#1,d0
		ext.w	d0
		rol.l	#1,d0
		move	d0,(a1)+
ex_make2:	clr	d0
		move	$dff006,d0
		add.b	$dff005,d0
		subi	(a2,d                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ,x CúÊN®ÿ  @ h p Nu9  ßðg 8yH€ üÜf  Aú– üFC070¼18Aú`® ùL&g  †Aú@® ëâ&gvAú"® ì> .ghAú® ì .gZAú6 . *€® .€®*fbyH€ üÜg2® üÜþ:f49  ¿àf  ù  ¿à3üO ßñ€Nu3üô ßñ€Nu® üþ:f`ÌCú^p0ü  QÈÿúpCú8ØQÈÿüCú
$. .a"$. *a$.þ:a$."a$.&a
$.*a`  $réš&  0 :e^ÃQÉÿèü  NuCú„N®þh/ ,WAú :p "<   ´N®ÿ¦J€g,x N®ÿjø   Nù ü Ð,x "_N®þb3üD ßñ€Nu `  Amiga Action Replay MK III AntiVirus-Boot  P#For SCA,BANDIT,NORTH-STAR,BYTE WARRIOR (DASA), VIRU ,x CúÊN®ÿ  @ h p Nu9  ßðg 8yH€ üÜf  Aú– üFC070¼18Aú`® ùL&g  †Aú@® ëâ&gvAú"® ì> .ghAú® ì .gZAú6 . *€® .€®*fbyH€ ü03aþ  Üþ:f49  ¿àf  ù  ¿«D©^þ  ñ€Nu3üô ßñ€Nu®5itÂ°  `ÌCú^p0`Šg`þ  áa`þ      Cú
`     *a$.þ:a$."a`Šg`þ  ¹€^`þ  `Šg`þ    0 :e^ÃQÉÿèü  NuCú„N®ný_`þ   :p "<   ´N®ÿ¦J€g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2       IOR (DASn;åm     ,x CúÊN®ÿ  @ h p Nu9  ßðg 8yH€ üÜf  Aú– üFC070¼18Aú`® ùL&g  †Aú@® ëâ&gvAú"® ì> .ghAú® ì .gZAú6 . *€® .€®*fbyH€ ü03aþ  Üþ:f49  ¿àf  ù  ¿«D©^þ  ñ€Nu3üô ßñ€Nu®5itÂ°  `ÌCú^p0`Šg`þ  áa`þ  ƒ   Cú
`     *a$.þ:a$."a`Šg`þ  ¹€^`þ  `Šg`þ    0 :e^ÃQÉÿèü  NuCú„N®ný_`þ   :p "<   ´N®ÿ¦J€g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2       IOR (DASn;åm     ,x CúÊN®ÿ  @ h p Nu9  ßðg 8yH€ üÜf  Aú– üFC070¼18Aú`® ùL&g  †Aú@® ëâ&gvAú"® ì> .ghAú® ì .gZAú6 . *€® .€®*fbyH€ ü03aþ  Üþ:f49  ¿àf  ù  ¿«D©^þ  ñ€Nu3üô ßñ€Nu®5itÂ°  `ÌCú^p0`Šg`þ  áa`þ  ƒ   Cú
`     *a$.þ:a$."a`Šg`þ  ¹€^`þ  `Šg`þ    0 :e^ÃQÉÿèü  NuCú„N®ný_`þ   :p "<   ´N®ÿ¦J€g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2       IOR (DASn;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      ˆ       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                            d        p              p     ÀÉÐ            ¸
‚                    ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      °      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                     d                             ÀÉÐ            ¸
‚      °             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      ð       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                     d        à              à     ÀÉÐ            ¸
‚      À             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      0       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                     d        °              °     ÀÉÐ            ¸
‚      Ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      X      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      à                                     d                           ÀÉÐ            ¸
‚      à             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      ˜       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      ð                                     d        €              €     ÀÉÐ            ¸
‚      ð             ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      À      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        €              €     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è             `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        	              	     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      @       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        °
              °
     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      h      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      0                                    d        à              à     ÀÉÐ            ¸
‚      0            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      ¨       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      @                                    d                             ÀÉÐ            ¸
‚      @            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      Ð      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      P                                    d        p              p     ÀÉÐ            ¸
‚      P            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è            `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      `                                    d        Ð              Ð     ÀÉÐ            ¸
‚      `            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      P       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          ÿÿÿÿ            n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      p                                    d        @              @     ÀÉÐ            ¸
‚      p            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      x      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      €                                    d        À              À     ÀÉÐ            ¸
‚      €            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      ¸       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                          d        P              P     ÀÉÐ            ¸
‚                  ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      à      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V                                           d        ð              ð     ÀÉÐ            ¸
‚                   ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è             `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      °                                    d                             ÀÉÐ            ¸
‚      °            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      `       `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      À                                    d        `              `     ÀÉÐ            ¸
‚      À            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½         ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                    d        0              0     ÀÉÐ            ¸
‚      Ð            ÀÈÐ    ò$V             d        ÉÐ    03aþ     3üô ôò    ÉÐ    iŠŒ     ðê-
    áa`þ  itÂ°        °|    ˆVA     ƒ       £"V            :§      ƒ   ^ÃÐú    2ú    3      pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½                 |#›J½  þ*``þ         à        çg`þ   ÌÐ                          Î*``þ                  Q$ú    d       ¨%V      Ð                                    d        0              0     ÀÉÐ            ¸
‚      Ð            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½        ÐÊÐ    è      ˆ      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                    d        0              0     ÀÉÐ            ¸
‚      Ð            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ  áa`þ  ƒ   °  `     °|    ˆVA     `Šg`þ  ¹€^`þ  `Šg`þ  :§      ƒ   ^ÃÐú    2ú    ný_`þ  pËÐ    pk     g,x N®ÿjø   Nù ü Ð,x,K›J½        ÐÊÐ    è      è      `Šg`þ          `Šg`þ  Â²b`þ                          2               n;åm    Q$ú    d       ¨%V      Ð                                    d        0              0     ÀÉÐ            ¸
‚      Ð            ÀÈÐ    03aþ          d        ÉÐ    «D©^þ     3üô ôò    5itÂ°  iŠŒ     `Šg`þ