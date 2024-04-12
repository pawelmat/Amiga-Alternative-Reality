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
		move.l	#5340,d0
		lea	p_plasmload,a0
		lea	$60000,a1
		jsr	decrunch
		raster
		move.l	#g_coppe0,$dff080
		jsr	$60000			;plazma
		raster
		move.l	#g_coppe0,$dff080
		lea	$dff000,a0
		move.l	#$ffffffff,$44(A0)

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
		addi.l	#[[ex_heith-1]*ex_row],d1		;2
		move	d1,ex_screen+14
		swap	d1
		move	d1,ex_screen+10
		move	d0,ex_screen+22				;3
		swap	d0
		move	d0,ex_screen+18
		swap	d0
		addi.l	#[[ex_heith-1]*ex_row],d0		;4
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
		subi	(a2,d1.w),d0
		add.b	(a3)+,d0
		sub.b	(a4)+,d0
		andi	#$ff,d0
		ext.w	d0
		move	d0,(a1)+
		addi	#2,d1
		andi	#$3e,d1
		addi	#-6,d2
		andi	#$3e,d2
		dbf	d7,ex_make1
		rts

;---------------------------------------------------------------------
ex_sine:
dc.w	$0001,$0001,$0003,$0005,$0007,$0009,$000B,$000C
dc.w	$000E,$000F,$0010,$0011,$0012,$0013,$0013,$0013
dc.w	$0013,$0013,$0013,$0012,$0012,$0011,$0010,$000E,$e
dc.w	$000D,$000C,$000A,$0008,$0006,$0004,$0002
dc.w	$0002,$FFFE,$FFFC,$FFFA,$FFF8,$FFF6,$FFF4,$FFF3
dc.w	$FFF2,$FFF0,$FFEF,$FFEE,$FFEE,$FFED,$FFED,$FFED,$ffed
dc.w	$FFED,$FFED,$FFED,$FFEE,$FFEF,$FFF0,$FFF1,$FFF2
dc.w	$FFF4,$FFF5,$FFF7,$FFF9,$FFFB,$FFFD,$FFFF

;---------------------------------------------------------------------
ex_ilosc=300
ex_pos=$70000
ex_plane=$71000
scrclr:		dc.l	ex_plane+[2*ex_heith*ex_row]
scrlast:	dc.l	ex_plane+[3*ex_heith*ex_row]

ex_heith=256
ex_row=44
;---------------------------------------------------------------------
ex_copper:
dc.l	$1820888,$1840888,$1860ccc
dc.l	$1880888,$18a0ccc,$18c0ccc,$18e0ccc
dc.l	$1900888,$1920ccc,$1940ccc,$1960ccc
dc.l	$1980ccc,$19a0ccc,$19c0ccc,$19e0ccc
dc.l	$1800000
dc.l	$920030,$9400d8,$8e0171,$9037d1,$1020000,$1080000,$10a0000![[-2*ex_row]&$ffff]
ex_screen:
dc.l	$e00000+[ex_plane/$10000],$e20000+[ex_plane&$ffff]
dc.l	$e40000+[ex_plane/$10000],$e60000+[ex_plane&$ffff]
dc.l	$e80000+[ex_plane/$10000],$ea0000+[ex_plane&$ffff]
dc.l	$ec0000+[ex_plane/$10000],$ee0000+[ex_plane&$ffff]
ex_scrcol:
dc.l	$2a01ff00,$1004300
dc.l	$ffdffffe
dc.l	$2a01ff00,$1000300
dc.l	$fffffffe

;---------------------------------------------------------------------
i_starframe:
	clr	ubor
	move	#255,dbor
	move	#351,rbor
	move.l	#i_plane,scron
	move.l	#i_plane+[2*i_heith*i_row],scroff
	move.l	#i_plane+[4*i_heith*i_row],scrclr

	bsr	i_makestars
	bsr	i_chgscr
	bsr	i_chgscr
	bsr	i_chgscr
	bsr	i_chgscr
	lea	i_ramka,a6		;tu nazwe figury
	bsr	i_turn
	waitblt
	raster
	move.l	#i_copper,$dff080

i_control0:	raster
		bsr.L	i_chgscr
		bsr.L	i_vector
		subi	#3,(a6)
		addi	#3,2(a6)
		cmpi	#120,2(a6)
		bmi.s	i_control0
		move	#-4,6(a6)
		move	#4,8(a6)
i_control1:	raster
		bsr.L	i_chgscr
		bsr.L	i_vector
		subi	#10,4(a6)
		bpl.s	i_control1
i_control2:	raster
		bsr.L	i_chgscr
		bsr.L	i_vector
		addi	#6,4(a6)
		cmpi	#700,4(a6)
		bmi.s	i_control2

		lea	i_upcol,a1
		move	#$444,d0
		bsr.L	i_setc
		lea	i_dbor2+4,a1
		move	#$a00,d0
		bsr.L	i_setc
i_control4:	raster
		bsr.L	i_chgscr
		bsr.L	i_vector
		addi	#6,4(a6)
		addi.b	#4,i_upbor
		subi.b	#2,i_dbor2
		bne.s	i_control4
		lea	i_dbor1+4,a1
		move	#$a00,d0
		bsr.s	i_setc
i_control5:	raster
		bsr.s	i_chgscr
		bsr.L	i_vector
		addi	#6,4(a6)
		addi.b	#4,i_upbor
		subi.b	#2,i_dbor1
		cmpi.b	#$d2,i_dbor1
		bne.s	i_control5
		rts

;-------------------------------------------------------------------
i_setc:		move	#3,d7
i_setc1:	move	d0,2(a1)
		addi.l	#4,a1
		dbf	d7,i_setc1
		rts
;-------------------------------------------------------------------
i_chgscr:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	scrclr,d2
		move.l	d0,scrclr
		move.l	d1,scron
		move.l	d2,scroff
		lea	i_screen,a1
		move	#1,d2
i_chgl:		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#[i_heith*i_row],d1
		addq	#8,a1
		dbf	d2,i_chgl
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[2*i_heith*64]+[i_row/2],$58(a0)
		rts

i_vector:	movem	6(a6),d0-d2
		add	d0,12(a6)
		add	d1,14(a6)
		add	d2,16(a6)
		lea	l_sinus,a1
		lea	l_sinus+128,a3		;cosinus
		lea	i_gwiazdy,a2
		move.l	scroff,a5
		move	#i_ilosc-1,d7
		move	#4,i_tud6
i_stars:	bsr	i_turn
		move	i_tud6,d6
		addi	d6,(a2)
		cmpi	#256,(a2)
		bmi.s	i_noskip
		subi	#512,(a2)
i_noskip:	addq.l	#4,a2
		tst	d1
		bmi.s	i_nostar
		tst	d2
		bmi.s	i_nostar
		cmpi	rbor,d1
		bpl.s	i_nostar
		cmpi	dbor,d2
		bpl.s	i_nostar
		mulu	#i_row,d2
		move	d1,d0
		lsr	#3,d0
		addi.l	d0,d2
		andi	#7,d1
		eori	#7,d1
		bset.b	d1,(a5,d2.w)
i_nostar:	addi	#2,i_tud6
		cmpi	#10,i_tud6
		bne.s	i_noskip2
		move	#4,i_tud6
i_noskip2:	dbf	d7,i_stars

i_frame:	lea	18(a6),a2		;tablica punktow
		lea	l_matrix,a4
		lea	i_heith*i_row(a5),a5
i_twodim:	bsr.L	i_turn
		movem	d1/d2,(a4)
		addq.l	#4,a2
		bsr.L	i_turn
		move	d1,d0
		move	d2,d1
		movem	(a4)+,d2/d3
		movem	d0/d1,(a4)
		bsr.s	i_drawline		;1
		addq.l	#4,a2
		bsr.L	i_turn
		move	d1,d0
		move	d2,d1
		movem	(a4),d2/d3
		movem	d0/d1,(a4)
		bsr.s	i_drawline		;2
		addq.l	#4,a2
		bsr.L	i_turn
		move	d1,d0
		move	d2,d1
		movem	(a4),d2/d3
		movem	d0/d1,(a4)
		bsr.s	i_drawline		;3
		movem	(a4),d0/d1
		movem	-4(a4),d2/d3
		bsr.s	i_drawline		;4
		rts

i_drawline:	cmpi	rbor,d0			;cut to frame
		bmi.s	i_prawo2
		cmpi	rbor,d2
		bpl.L	i_noline
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
		bra.s	i_lewo
i_prawo2:	cmpi	rbor,d2
		bmi.s	i_lewo
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
i_lewo:		tst	d0
		bpl.s	i_lewo2
		tst	d2
		bmi.l	i_noline
		move	d3,d4
		subi	d1,d4
		muls	d2,d4
		move	d2,d5
		subi	d0,d5
		divs	d5,d4
		clr	d0
		move	d3,d1
		subi	d4,d1
		bra.s	i_gora
i_lewo2:	tst	d2
		bpl.s	i_gora
		move	d1,d4
		subi	d3,d4
		muls	d0,d4
		move	d0,d5
		subi	d2,d5
		divs	d5,d4
		clr	d2
		move	d1,d3
		subi	d4,d3
i_gora:		cmpi	ubor,d1
		bpl.s	i_gora2
		cmpi	ubor,d3
		bmi.L	i_noline
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
		bra.s	i_dol
i_gora2:	cmpi	ubor,d3
		bpl.s	i_dol
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
i_dol:		cmpi	dbor,d1
		bmi.s	i_dol2
		cmpi	dbor,d3
		bpl.L	i_noline
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
		bra.L	i_draw
i_dol2:		cmpi	dbor,d3
		bmi.L	i_draw
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

i_draw:		movem.l	d0-d7/a2,-(sp)
		clr	d4
		cmpi	d1,d3
		bpl.s	i_dr0
		exg	d0,d2
		exg	d1,d3
i_dr0:		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl.s	i_dr1
		ori	#%010,d4
		neg	d0
i_dr1:		subi	d3,d1
		bpl.s	i_dr2
		ori	#%001,d4
		neg	d1
i_dr2:		cmpi	d0,d1
		bmi.s	i_dr3
		exg	d0,d1
		ori	#%100,d4
i_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	l_octant,a2
		move.b	(a2,d4.w),d7
		lsl	#1,d1
		or.l	#$0bca0001,d7
		mulu	#i_row,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move.l	#-1,$44(a0)
		move	#i_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	i_dr4
		ori	#$40,d7
i_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
		movem.l	(sp)+,d0-d7/a2
i_noline:	rts

i_turn:		move	16(a6),d0	;a6-object,a2-table,a3-sin
		clr	d1		;a1-cos
		move	(a2),d2		;zxy
		bsr.s	i_rotate
		move	14(a6),d0
		move	d2,d3
		move	2(a2),d2	;zyx
		bsr.s	i_rotate
		move	12(a6),d0
		exg	d1,d3		;xzy
		bsr.s	i_rotate

		addi	4(a6),d3		;zoom calosci
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
		rts
i_rotate:	andi	#$1fe,d0
		cmpi	#128,d0
		beq.s	i_norotate
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
i_norotate:	rts

;-------------------------------------------------------------------
i_makestars:	lea	i_gwiazdy,a1
		lea	i_data,a2
		move	#i_ilosc-1,d7
		move	#i_start,d6
i_makeloop1:	move	(a2)+,d0
		andi	#$1ff,d0
		subi	#256,d0
		move	d0,(a1)+
		move	d6,(a1)+
		addi	#8,d6
		dbf	d7,i_makeloop1
		rts

;-------------------------------------------------------------------
i_copper:
dc.l	$920030,$9400d8,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
i_screen:
dc.l	$e00007,$e20000,$e40007,$e60000+[i_heith*[i_row]]
i_upcol:
dc.l	$1800000,$1820000,$1840000,$1860000
dc.l	$2a01ff00,$01002300				;on
i_upbor:
dc.l	$2a01ff00,$1800000,$182080f,$1840a40,$1860a40
i_dbor1:
dc.l	$fe01ff00,$1800000,$182080f,$1840a40,$1860a40
dc.l	$ffdffffe
i_dbor2:
dc.l	$2a01ff00,$1800000,$1820000,$1840000,$1860000
dc.l	$2a01ff00,$01000300				;off
dc.l	$fffffffe

;-------------------------------------------------------------------
i_plane=$6f000

i_heith=256
i_row=44
i_tud6:			dc.w	0

i_ilosc=45
i_start=-180
;-------------------------------------------------------------------
i_ramka:
dc.w	405,-102,850
dc.w	-10,0,0
dc.w	110,128,128
dc.w	-256,180		;18(),		x,y only
dc.w	256,180
dc.w	256,-180
dc.w	-256,-180
i_gwiazdy:
blk.l	[i_ilosc],0

i_data:
dc.w	15167,16516,1435,13451,1421,213,14421,9764,7468,124,5265,6
dc.w	262,234,1234,16,547,8764,456,346,436,346,3467,346,37,3456,25,24
dc.w	2354,3426,236,97,769,5,7649,45,7,46,74,347,436,346,8653,546,2

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
		cmpi	#1026,4(a6)
		bmi.s	f_control10
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		bsr	f_chgscr
		raster
		move.l	#g_coppe0,$dff080
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
f_plane=$71000

f_heith=188
f_row=44

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
s_scroll:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		move.l	#$7a000,scron
		move.l	#$7a000+[[s_heigth+2]*s_row],scroff
		bsr	s_init
		move	#[3072/4]-1,d7		;copy fonts to chip
		lea	s_fontload,a1
		lea	s_font,a2
s_copfont:	move.l	(a1)+,(a2)+
		dbf	d7,s_copfont

		lea	s_altload,a1
		lea	s_alt,a2
		move	#[232*3]-1,d7		;copy pic
s_coploop:	clr	(a2)+
		move	#9,d6
s_coploop2:	move.l	(a1)+,(a2)+
		dbf	d6,s_coploop2
		clr	(a2)+
		dbf	d7,s_coploop
		bsr	s_chg
 		bsr	s_chg
		waitblt
		clr	$66(a0)
		move.l	#$1ff0000,$40(a0)
		move.l	a2,$54(a0)
		move	#[232*64]+22,$58(a0)

		waitblt
		move.l	#s_copper0,$dff080

		type	s_napisy		;sit, relax
		lea	(a2),a5
		move	#320+16,d0
		clr	d1
		move	#16,d2
		move	#231,d3
s_pokaz:	raster
		move	#4,d7
s_pok0_1:	bsr	s_drawline
		addq	#1,d1
		subq	#1,d3
		beq.s	s_pokaz1
		dbf	d7,s_pok0_1
		bra.s	s_pokaz
s_pokaz1:	raster
		move	#4,d7
s_pok1_1	bsr	s_drawline
		addq	#1,d2
		subq	#1,d0
		beq.s	s_scrolluj
		dbf	d7,s_pok1_1
		bra.s	s_pokaz1

;----------------------------------------------------------------------
s_scrolluj:	move	#25,d7
s_czekaj:	raster
		dbf	d7,s_czekaj
		waitblt
		raster
		move.l	#s_copper,$dff080
s_control:	raster
		bsr	s_chg
		bsr	s_move
		lea	s_data,a3
		tst	12(a3)
		bne.s	s_control

		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#$68000,$54(a0)
		move	#[3*232*64]+22,$58(a0)
		waitblt
		move.l	#s_alt,scroff
		move.l	#$68000,scron

		clr	d6
s_schowaj0:	move	#2,d7
s_schowaj:	raster

		waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	s_movscr,a1
		move	#2,d2
s_chgls:	move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#232*44,d1
		addq.l	#8,a1
		dbf	d2,s_chgls

		clr.l	d0
		move	d6,d0
		ror.l	#4,d0
		ori.l	#$9f00000,d0
		move.l	d0,$40(a0)
		clr.l	$64(a0)
		clr	$46(a0)
		move.l	scron,$50(a0)
		move.l	scroff,$54(a0)
		move	#[3*232*64]+22,$58(a0)
		waitblt
		dbf	d7,s_schowaj
		addi	#1,d6
		cmpi	#16,d6
		bne.L	s_schowaj0
		move.l	#$ffffffff,$44(a0)
		rts

;----------------------------------------------------------------------
s_chg:		waitblt
		move.l	scron,d0		;double screen mode
		move.l	scroff,d1
		move.l	d1,scron
		move.l	d0,scroff
		lea	s_screen,a1
		move	d1,6(a1)
		move	d1,14(a1)
		swap	d1
		move	d1,2(a1)
		move	d1,10(a1)
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[s_heigth*64]+[s_row/2],$58(a0)
		rts

s_init:		lea	s_cop2,a1		;create copperlist
		lea	s_coltab,a2
		move.l	#$5801ff00,d0
		move.l	#$1020000,(a1)+
		move.l	#$10a0000,(a1)+
		move	#$196,(a1)+
		move	(a2)+,(A1)+
		move	#s_heigth-2,d1
s_zoom_c1:	add.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1020000,(a1)+
		move.l	#$10a0000,(a1)+
		move	#$196,(a1)+
		move	(a2)+,(A1)+
		dbf	d1,s_zoom_c1
		rts

;----------------------------------------------------------------------
s_move:		lea	s_cop2,a1
		lea	s_data,a3
		lea	s_sine,a5
		move	2(a3),d1
		clr	d3
		move	6(a3),d5
		move	#s_heigth-2,d7
s_pion_loop:	clr	d0
		addi	d5,d1
		andi	#$1ff,d1
		move.b	(a5,d1.w),d0
		move	d0,d2
		lsr	#4,d0
		lsl	#1,d0
		neg	d0
		subi	d3,d0
		addi	d0,d3
;		andi	#$f,d2
		addi	#18,d0
		move	d0,6(a1)
		lea	16(a1),a1
		lsl	#4,d2
		move.b	d2,3(a1)
		dbf	d7,s_pion_loop


		lea	s_font,a1
		move.l	scroff,a2
		lea	-16(a2),a2
		movem	8(a3),d0/d1
		addi	d0,(a3)
		addi	d1,2(a3)
		move	(a3),d1
		lea	s_text,a6
		move	12(a3),d0
		lea	(a6,d0.w),a6

		subi	#4,14(a3)
		andi	#$e,14(a3)
		move	14(a3),d6		;word counter
		bne	s_no_next_char
		addi	#1,12(a3)
		cmpi	#[s_textend-s_text],12(a3)
		bne	s_no_next_char
		clr	12(a3)
s_no_next_char:
		move	#$c000,d4
		lsr	d6,d4			;rol B mask
		move	d6,d0
		lsr	#1,d0
		mulu	4(a3),d0
		addi	d0,d1			;fix angle with shift
		move	#223,d7
		move	4(a3),d5
		clr	d3			;letter counter
		clr.l	d0
		move.b	(a6)+,d0
		subi	#32,d0
		lsl	#5,d0
		lea	(a1,d0.w),a3		;get char
		clr.l	d0
		move	d6,d0
		ror.l	#4,d0
		ori.l	#$0be20000,d0		;shift
		waitblt
		move.l	d0,$40(a0)
		move	#s_row-2,$60(a0)
		move	#s_row-2,$66(a0)
		clr	$64(a0)

s_hloop:	clr	d0
		addi	d5,d1
		andi	#$1ff,d1
		move.b	(a5,d1.w),d0
		move	d0,d2			;mulu 60
		lsl	#6,d0
		lsl	#2,d2
		subi	d2,d0
		lea	(a2,d0.w),a4
		move	#[16*64]+1,d0
		move	d6,d2
		subi	d3,d2
		bpl	s_nominus
		lea	-s_row(a4),a4		;fix with shift
		addi	#64,d0
s_nominus:	move	d4,$72(a0)
		move.l	a3,$50(a0)		;copy 1 line
		move.l	a4,$48(a0)
		move.l	a4,$54(a0)
		move	d0,$58(a0)

		ror	#2,d4
		addi	#2,d6
		andi	#$e,d6
		bne	s_noword
		lea	2(a2),a2		;increase screen offset
s_noword:	addi	#2,d3
		andi	#$e,d3
		bne	s_hcont
		clr	d0
		move.b	(a6)+,d0
		subi	#32,d0
		lsl	#5,d0
		lea	(a1,d0.w),a3
s_hcont:	dbf	d7,s_hloop
		rts

;----------------------------------------------------------------------
s_drawline:
		movem.l	d0-d7/a2,-(sp)
		clr	d4
		cmpi	d1,d3
		bpl	s_dr0
		exg	d0,d2
		exg	d1,d3
s_dr0:		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl	s_dr1
		ori	#%010,d4
		neg	d0
s_dr1:		subi	d3,d1
		bpl	s_dr2
		ori	#%001,d4
		neg	d1
s_dr2:		cmpi	d0,d1
		bmi	s_dr3
		exg	d0,d1
		ori	#%100,d4
s_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	l_octant,a2
		move.b	(a2,d4.w),d7
		lsl	#1,d1
		or.l	#$0b0a0001,d7
		mulu	#44,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#44,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl	s_dr4
		ori	#$40,d7
s_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
		movem.l	(sp)+,d0-d7/a2
		rts

;----------------------------------------------------------------------
s_alt=$70000
s_font=$6f000
s_heigth=160
s_row=60
;----------------------------------------------------------------------
s_textptr:	dc.w	0

s_text:
dc.b"                                   "
dc.b" WELCOME AGAIN...   ...TO THE"
dc.b" TIMES OF... ...ALTERNATIVE REALITY... ...... TIMES OF TOTAL"
dc.b" CHAOS, DARKNESS AND DISASTER... TIMES WHEN RULEZ --- SUSPECT --- !!!"
dc.b"!!!... .. . "
s_textend:
dc.b"                                 "
even
;----------------------------------------------------------------------
s_data:
dc.w	0,20			;axids
dc.w	1,-2			;add values
dc.w	4,3			;global add
dc.w	1,0			;text ptr, B mask
;----------------------------------------------------------------------
s_copper0:
dc.l	$1800000
dc.l	$1820fff,$1840ddd,$1860bbb,$1880888,$18a0666,$18c0444,$18e0222
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
dc.l	$920030,$9400d0,$8e0171,$9037d1
dc.l	$1020000,$1080002,$10a0002
dc.l	$3001ff00,$01004300
dc.l	$e00000+[s_alt/$10000],$e20000+[s_alt&$ffff]
dc.l	$e40000+[[s_alt+10208]/$10000],$e60000+[[s_alt+10208]&$ffff]
dc.l	$e80000+[[s_alt+20416]/$10000],$ea0000+[[s_alt+20416]&$ffff]
dc.l	$ec0000+[[s_alt+30624]/$10000],$ee0000+[[s_alt+30624]&$ffff]
dc.l	$ffdffffe
dc.l	$1401ff00,$01000300
dc.l	$1020000,$1080000,$10a0000,$920038,$9400d0
s_napisy:	set	$1a
dc.l	-2

;----------------------------------------------------------------------
s_copper:
dc.l	$1800000
dc.l	$1820fff,$1840ddd,$1860bbb,$1880888,$18a0666,$18c0444,$18e0222
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000

dc.l	$920030,$9400d0,$8e0171,$9037d1
dc.l	$1020000,$1080002,$10a0012		;modulo #18
dc.l	$1040000
dc.l	$3001ff00,$01005700
dc.l	$e40007,$e60000
dc.l	$ec0007,$ee0000
s_movscr:
dc.l	$e00000+[s_alt/$10000],$e20000+[s_alt&$ffff]
dc.l	$e80000+[[s_alt+10208]/$10000],$ea0000+[[s_alt+10208]&$ffff]
dc.l	$f00000+[[s_alt+20416]/$10000],$f20000+[[s_alt+20416]&$ffff]
dc.l	$5801ff00
dc.l	$1040040
s_screen:
dc.l	$e40007,$e60000
dc.l	$ec0007,$ee0000
dc.l	$01005700
s_cop2:
ds.l	[4*160]-1,0
dd:
dc.l	$1020000,$10a0000,$1960000
dc.l	$e40007,$e60000
dc.l	$ec0007,$ee0000
dc.l	$01005700,$1040000
dc.l	$ffdffffe
dc.l	$1401ff00,$01000300
dc.l	-2

;---------------------------wykres-------------------------------------
Cosx:	macro
	add.w	#90,d5
	cmp.w	#360,d5
	blt.s	wait?0
	sub.w	#360,d5
Wait?0:	lea	kl_sintab,a5
	add.w	d5,d5
	move.w (a5,d5.w),d5
	endm

Sinx:	macro
	lea	kl_sintab,a5
	add.w	d5,d5
	move.w (a5,d5.w),d5
	endm
		
;--------------------------------------------------------------------
kl_krata:	raster
		lea	$dff000,a6
		move.l	#$78000,$54(a6)
		move.l	#$ffffffff,$44(A6)
		move.l	#$01000000,$40(A6)
		clr.l	$64(a6)
		move.w	#$4014,$58(a6)
		move.l	#kl_copper0,$dff080
		lea	kl_up,a1
kl_setkol:	raster
		addi.b	#8,(a1)
		cmpi.b	#$f8,(a1)
		bne.s	kl_setkol
		move	#$10,6(a1)
		lea	kl_dn,a1
kl_setkol2:	raster
		addi.b	#8,(a1)
		cmpi.b	#$30,(a1)
		bne.s	kl_setkol2
		waitblt
		move.l	#kl_copper,$80(a6)

kl_control:	raster				;do gory
		bsr.L	kl_doit
		subi.b	#16,kf_dn2
		bne.s	kl_control
		raster
		bsr.L	kl_doit
		move.l	#$1004300,kf_dn1+4
		move.l	#$1080000,kf_dn2+4
		raster
		bsr.L	kl_doit
kl_control1:	raster
		bsr.L	kl_doit
		subi.b	#16,kf_dn1
		cmpi.b	#$30,kf_dn1
		bne.s	kl_control1
		move	#100,l_counter

kl_control2:	raster				;stoj
		bsr.L	kl_doit
		subi	#1,l_counter
		bne.s	kl_control2

kl_control3:	raster				;obniz
		bsr.L	kl_doit
		addi.b	#16,kf_dn1
		cmpi.b	#$f0,kf_dn1
		bne.s	kl_control3
		raster
		bsr.L	kl_doit
		move.l	#$1004300,kf_dn2+4
		move.l	#$1080000,kf_dn1+4
		raster
		bsr.L	kl_doit
kl_control4:	raster
		bsr.L	kl_doit
		addi.b	#16,kf_dn2
		cmpi.b	#$30,kf_dn2
		bne.s	kl_control4

		raster
		raster
		raster
		raster
		move.l	#kl_copper0,$dff080
		lea	kl_dn,a1
kl_setkol3:	raster
		subi.b	#8,(a1)
		bne.s	kl_setkol3
		lea	kl_up,a1
		clr	6(a1)
kl_setkol4:	raster
		subi.b	#8,(a1)
		cmpi.b	#$10,(a1)
		bne.s	kl_setkol4
		clr	kl_copper0+6
		rts

kl_doit:	bsr	kl_rotate
		bsr	kl_clearplane
		bsr	kl_transform
		bsr	kl_katy
		bsr	kl_drawing
		bsr	kl_move_pic
		bsr	kl_nowa_klatka
		rts

kl_move_pic:	waitblt
		move.l	#$78000,$50(a6)
		move.l	#$7b000,$54(a6)
		move.l	#$ffffffff,$44(a6)
		move.l	#$09f00000,$40(a6)
		move.l	#$00000000,$64(a6)
		move.w	#$4014,$58(a6)
		rts

kl_Nowa_Klatka:
		subq.w	#1,kl_klatki_il
		cmpa.w	#0,kl_klatki_il
		bne.s	kl_nowa_l
		move.l	#kl_klatki,kl_last_klatka
		move.w	#41,kl_klatki_il
kl_nowa_L:	move.l	kl_last_klatka,a0
		move.l	(a0)+,kl_jakie_klatki
		move.l	a0,kl_last_klatka
		rts

****************************** katy
kl_katy:	addq.w	#4,kl_zangle
		cmpa.w	#359,kl_zangle
		bcs.s	kl_next1
		move.w	#0,kl_zangle
kl_next1:	addq.w	#4,kl_yangle
		cmp.w	#359,kl_yangle
		bcs.s	kl_next2
		move.w	#0,kl_yangle
kl_next2:	addq.w	#3,kl_xangle
		cmp.w	#359,kl_xangle
		bcs.s	kl_next3
		move.w	#0,kl_xangle
kl_next3:	rts
****************************** rysowanie
kl_drawing:	lea	kl_tab_2d,a4
		lea	kl_lines,a5
kl_drloop:	move.w	(a5)+,d4
		cmp.w	#255,d4		;table end?
		beq	kl_drawend
		add.w	d4,d4
		add.w	d4,d4
		move.w	(a4,d4.w),d0	
		move.w  2(a4,d4.w),d1	;(d0,d1) is point to draw from	
		move.w	(a5)+,d4
		add.w  	d4,d4
		add.w	d4,d4
		move.w	(a4,d4.w),d2
		move.w	2(a4,d4.w),d3	;get destination point

		add.w	#kl_offsetx,d0
		add.w	#kl_offsety,d1
		add.w	#kl_offsetx,d2
		add.w	#kl_offsety,d3

		lea	$78000,a0
		ext.l	d0
		ext.l	d1
		ext.l	d2
		ext.l	d3

		sub.w	d0,d2
		bmi.s	kl_xneg
		sub.w	d1,d3
		bmi.s	kl_yneg
		cmp.w	d3,d2
		bmi.s	kl_ygtx
		moveq.l	#kl_OCTANT1+1,d5
		bra.s	kl_lineagain
kl_ygtx:	exg	d2,d3
		moveq.l	#kl_OCTANT2+1,d5
		bra.s	kl_lineagain
kl_yneg:	neg.w	d3
		cmp.w	d3,d2
		bmi.s	kl_ynygtx	
		moveq.l	#kl_OCTANT8+1,d5
		bra.s	kl_lineagain
kl_ynygtx:	exg	d2,d3
		moveq.l	#kl_OCTANT7+1,d5
		bra.s	kl_lineagain
kl_xneg:	neg.w	d2
		sub.w	d1,d3
		bmi.s	kl_xyneg
		cmp.w	d3,d2
		bmi.s	kl_xnygtx
		moveq.l	#kl_OCTANT4+1,d5
		bra.s	kl_lineagain
kl_xnygtx:	exg	d2,d3
		moveq.l	#kl_OCTANT3+1,d5
		bra.s	kl_lineagain
kl_xyneg:	neg.w	d3
		cmp.w	d3,d2
		bmi.s	kl_xynygtx
		moveq.l	#kl_OCTANT5+1,d5
		bra.s	kl_lineagain
kl_xynygtx:	exg	d2,d3
		moveq.l	#kl_OCTANT6+1,d5
kl_lineagain:	mulu.w	#40,d1
		ror.l	#4,d0
		add.w	d0,d0
		add.l	d1,a0
		add.w	d0,a0
		swap	d0
		or.w	#$bca,d0
		lsl.w	#2,d3
		add.w	d2,d2
		move.w	d2,d1
		lsl.w	#5,d1
		add.w	#$42,d1
		waitblt
		move.w	d3,$62(a6)
		sub.w	d2,d3
		ext.l	d3
		move.l	d3,$50(a6)
		bpl.s	kl_lineover
		or.w	#$40,d5
kl_lineover:	move.w	d0,$40(a6)
		move.w	d5,$42(a6)
		move.w	#40,$60(a6)
		move.w	d4,$66(a6)
		sub.w	d2,d3
		move.w	d3,$64(a6)
		move.l	#$ffff8000,$72(a6)
		move.l	#-1,$44(a6)
		move.l	a0,$48(a6)
		move.l	a0,$54(a6)
		move.w	d1,$58(a6)
		bra	kl_drloop
kl_drawend:	rts

kl_clearplane:	waitblt
		move.l	#$78000,$54(a6)
		move.l	#$ffffffff,$44(A6)
		move.l	#$01000000,$40(A6)
		clr.l	$64(a6)
		move.w	#$4014,$58(a6)
		rts
************************************** 3d to 2d
kl_transform:	lea	kl_tab_3d,a3
		lea	kl_tab_2d,a2
kl_tloop:	movem.w	(a3)+,d0-d2	;x
		cmp.w	#255,d0
		beq.s	kl_tret
		sub.w	#-1000,d2
		muls	#-1000,d0
		muls	#-1000,d1
		divs	d2,d0
		divs	d2,d1
		move.w	d0,(a2)+
		move.w	d1,(a2)+
		bra.s	kl_tloop
kl_tret:	rts	

**********************************************
* calculate new position after 3-axis        *
*           rotation		             *
**********************************************
kl_rotate:	move.l	kl_jakie_klatki,a0
		lea	kl_tab_3d,a1
kl_rloop:	move.w	(a0)+,d0	;x
		move.w	(a0)+,d1	;y
		move.w	(a0)+,d2	;z
		cmp.w	#255,d0
		beq	kl_rend

kl_zrot:	move.w	kl_zangle,d5
		sinx
		move.l	d5,d6
		move.w	kl_zangle,d5
		cosx
		move.w	d0,d3
		move.w	d1,d4
		muls	d5,d3		;now:	d6-sin,d5-cos,d3-x,d4-y
		muls	d6,d4
		add.l	d3,d4
		asr.l	#7,d4	
		asr.l	#7,d4		;new x calculated in d4
		move.w	d0,d3
		move.w	d1,d7		;now:	d6-sin,d5-cos,d3-x,d7-y
		muls	d6,d3
		muls	d5,d7
		sub.l	d3,d7
		asr.l	#7,d7	
		asr.l	#7,d7		;y in d7
		move.w	d4,d0		;store new values
		move.w	d7,d1

kl_yrot:	move.w	kl_yangle,d5
		sinx
		move.l	d5,d6
		move.w	kl_yangle,d5
		cosx		
		move.w	d0,d3
		move.w	d2,d4
		muls	d5,d3		;now:	d6-sin,d5-cos,d3-x,d4-z
		muls	d6,d4
		sub.l	d4,d3
		asr.l	#7,d3	
		asr.l	#7,d3		;new x calculated in d3
		move.w	d0,d7
		move.w	d2,d4		;now:	d6-sin,d5-cos,d7-x,d4-z
		muls	d6,d7
		muls	d5,d4
		add.l	d7,d4
		asr.l	#7,d4	
		asr.l	#7,d4		;y in d4
		move.w	d3,d0		;store new values
		move.w	d4,d2

kl_xrot:	move.w	kl_xangle,d5
		sinx
		move.l	d5,d6
		move.w	kl_xangle,d5
		cosx
		move.w	d1,d3
		move.w	d2,d4
		muls	d5,d3		;now:	d6-sin,d5-cos,d3-y,d4-z
		muls	d6,d4
		add.l	d4,d3
		asr.l	#7,d3	
		asr.l	#7,d3		;new y calculated in d3
		move.w	d1,d7
		move.w	d2,d4		;now:	d6-sin,d5-cos,d7-y,d4-z
		muls	d6,d7
		muls	d5,d4
		sub.l	d7,d4
		asr.l	#7,d4	
		asr.l	#7,d4		;z in d4
		move.w	d3,d1		;store new values
		move.w	d4,d2

		move.w	d0,(a1)+
		move.w	d1,(a1)+
		move.w	d2,(a1)+
		bra	kl_rloop
kl_rend:	move.w	#255,(a1)+
		rts

;--------------------------------------------------------------------
kl_Klatki:
dc.l	kl_k1,kl_k1+[602],kl_k1+[2*602],kl_k1+[3*602],kl_k1+[4*602]
dc.l	kl_k1+[5*602],kl_k1+[6*602],kl_k1+[7*602],kl_k1+[8*602]
dc.l	kl_k1+[9*602],kl_k1+[10*602],kl_k1+[11*602],kl_k1+[12*602]
dc.l	kl_k1+[13*602],kl_k1+[14*602],kl_k1+[15*602],kl_k1+[16*602]
dc.l	kl_k1+[17*602],kl_k1+[18*602],kl_k1+[19*602],kl_k1+[20*602]
dc.l	kl_k1+[19*602],kl_k1+[18*602],kl_k1+[17*602],kl_k1+[16*602]
dc.l	kl_k1+[15*602],kl_k1+[14*602],kl_k1+[13*602],kl_k1+[12*602]
dc.l	kl_k1+[11*602],kl_k1+[10*602],kl_k1+[9*602],kl_k1+[8*602]
dc.l	kl_k1+[7*602],kl_k1+[6*602],kl_k1+[5*602],kl_k1+[4*602]
dc.l	kl_k1+[3*602],kl_k1+[2*602],kl_k1+[602],kl_k1

****************************** copper
kl_copper:
dc.l	$1820fff,$1840aa0,$1860fff,$1880f00,$18a0fff,$18c0aa0,$18e0fff
dc.l	$190000f,$1920fff,$1940aa0,$1960fff,$1980f00,$19a0fff,$19c0aa0,$19e0fff
dc.l	$1800010
dc.l	$920038,$9400d0,$102000f,$1080000,$10affb0
dc.l	$e00007,$e2b000,$e40007,$e6d800
dc.l	$e80007,$eab0ca,$ec0007,$eed8ca
kf_dn1:
dc.l	$f001ff00
dc.l	$1080000
dc.l	$ffdffffe
kf_dn2:
dc.l	$3001ff00
dc.l	$01004300
dc.l	$3001ff00,$01000300
dc.w	$ffff,$fffe

kl_copper0:
dc.l	$01000300,$1800010
kl_up:
dc.l	$1001ff00,$1800000
dc.l	$ffdffffe
kl_dn:
dc.l	$0001ff00,$1800000
dc.l	$fffffffe

;--------------------------------------------------------------------
kl_jakie_klatki:	dc.l	kl_k1
kl_last_klatka:		dc.l	kl_klatki
kl_klatki_il:		dc.w	41
kl_tab_3d		ds.w	500
kl_tab_2d:		ds.w	500

kl_offsetx=150
kl_offsety=130
kl_xangle		dc.w	120
kl_yangle		dc.w	180
kl_zangle		dc.w	60

kl_OCTANT8	= 24
kl_OCTANT7	= 4
kl_OCTANT6	= 12
kl_OCTANT5	= 28
kl_OCTANT4	= 20
kl_OCTANT3	= 8
kl_OCTANT2	= 0
kl_OCTANT1	= 16

;----------------------------------------------------------------------
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
	type	na_napis2
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
dc.l	$1020000,$1080000,$10a0000,$920038,$9400d0
na_napis2:	set $1a
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
		type	na_napis3
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
dc.l	$1020000,$1080000,$10a0000,$920038,$9400d0
na_napis3:	set $1a
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
		type	na_napis4
		bsr	u_fraktal1
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
u_fd1_loop:	raster
		bsr	u_colout
		tst	u_collicz
		bpl	u_fd1_loop
		bsr	u_show
		type	na_napis4
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
		move	#40,d7
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
dc.l	$1020000,$1080000,$10a0000,$920038,$9400d0
na_napis4:	set $1a
dc.l	-2

;-------------------------------------------------------------------
g_coppe0:	dc.l	$1000300,$1800000,$fffffffe
g_coppe1:	dc.l	$1000300,$1800000
		dc.l	$1020000,$1080000,$10a0000,$920038,$9400d0
na_napis5:	set	$a0
		dc.l	$fffffffe
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

;----------------------------------------------------------------------
raster2: macro
wait?0:	cmp.b	#$e0,$dff006
	bne.s	wait?0
	cmp.b	#$e1,$dff006
	bne.s	wait?0+10
	endm

;----------------------------------------------------------------------
zoom_zoom:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#zoom_plane,$54(a0)
		move	#[500*64],$58(a0)
		waitblt
		lea	zoom_load,a1
		lea	zoom_plane+[zoom_offset*5*zoom_row],a2
		move	#[5*zoom_heigth]-1,d7
zoom_coppic0:	move	#[zoom_row/4]-1,d6
zoom_coppic1:	move.l	(a1)+,(a2)+
		dbf	d6,zoom_coppic1
		lea	-4(a1),a1
		dbf	d7,zoom_coppic0

		bsr	zoom_crtab
		waitblt
		raster2
		move.l	#zoom_copper,$dff080
		bsr	zoom_setcol

		move	#600,l_counter
zoom_control:	raster2
		bsr	zoom_out
	btst.b	#6,$bfe001
	beq.s	q
		subi	#1,l_counter
		bne.s	zoom_control
		move	#17,l_counter
zoom_control1:	raster2
		bsr	zoom_out
		raster2
		bsr	zoom_out
		raster2
		bsr.L	zoom_fadcol
		bsr	zoom_out
		subi	#1,l_counter
		bne.s	zoom_control1
q:		rts

;----------------------------------------------------------------------
zoom_setcol:	move	#16,d7
zoom_colloop:	lea	zoom_copper,a1
		lea	zoom_load+28800,a2
		move	#31,d3
		raster2
		raster2
		raster2
zoom_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq	zoom_scol2
		addi	#1,2(a1)
zoom_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq	zoom_scol3
		addi	#$10,2(a1)
zoom_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq	zoom_scol4
		addi	#$100,2(a1)
zoom_scol4:	adda	#4,a1
		dbf	d3,zoom_scol1
		dbf	d7,zoom_colloop
		rts

zoom_fadcol:	lea	zoom_copper,a1
		move	#31,d3
zoom_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq	zoom_fad2
		subi	#1,2(a1)
zoom_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq	zoom_fad3
		subi	#$10,2(a1)
zoom_fad3:	move	2(a1),d1
		andi	#$f00,d1
		beq	zoom_fad4
		subi	#$100,2(a1)
zoom_fad4:	adda	#4,a1
		dbf	d3,zoom_fad1
		rts

;----------------------------------------------------------------------
zoom_out:
		bsr.L	zoom_y
		clr.l	d0
		move	zoom_xpos,d0
		addi	#1,zoom_xpos
		cmpi	#160,zoom_xpos
		bne.s	zoom_xpos_ok
		clr	zoom_xpos
zoom_xpos_ok:	lsl	#1,d0
		lea	zoom_tablex,a1
		move	(a1,d0.w),d0
		addi	#8,d0
		move	d0,d1
		lea	zoom_maska,a1
		lsr	#4,d0
		move	d0,d2
		andi	#$f,d1
		move	#$8000,d3
		asr	d1,d3
		move	d1,d6
		subq	#1,d2
		bmi.s	zoom_itis_0
zoom_makemask:	move	#-1,(a1)+
		dbf	d2,zoom_makemask
zoom_itis_0:	move	d3,(a1)
		andi	#$0fff,zoom_maska
		addi	#1,d0			;blit width
		move	d0,d1
		lsl	#1,d1
		move	#zoom_row,d2
		subi	d1,d2			;modulo
		neg	d1
		addi.l	#5*zoom_heigth*64,d0	;blit size

		waitblt
		move	d2,$60(a0)		;c
		move	d2,$64(a0)		;a
		move	d2,$66(a0)		;d
		move	d1,$62(a0)		;b
		move.l	#$ffe20000,$40(a0)
		move.l	#zoom_plane+[zoom_offset*5*[zoom_row]],$50(a0) ;a
		move.l	#zoom_maska,$4c(a0)	;b
		move.l	#zoom_plane+[zoom_offset*5*[zoom_row]]-2,$48(a0) ;c
		move.l	#zoom_plane+[zoom_offset*5*[zoom_row]]-2,$54(a0) ;d
		move	d0,$58(a0)

		raster2
		movem.l	d0-d6/a1,-(sp)
		bsr.s	zoom_y
		movem.l	(sp)+,d0-d6/a1

		waitblt
		move	#-1,(a1)
		move	#15,d3
		subi	d6,d3
		move	#$8000,d6
		asr	d3,d6
		not	d6
		move	d6,zoom_maska
		move.l	#zoom_plane+[zoom_offset*5*zoom_row]-4,d6
		addi.l	d2,d6
		move.l	#$1fe20000,$40(a0)
		move.l	d6,$50(a0)			;a
		move.l	#zoom_maska,$4c(a0)		;b
		move.l	d6,$48(a0)			;c
		move.l	d6,$54(a0)			;d
		move	d0,$58(a0)
		rts

;----------------------------------------------------------------------
zoom_y:		clr.l	d0
		move	zoom_ypos,d0
		addi	#1,zoom_ypos
		cmpi	#256,zoom_ypos
		bne.s	zoom_ypos_ok
		clr	zoom_ypos
zoom_ypos_ok:	lsl	#1,d0
		lea	zoom_tabley,a1
		move	(a1,d0.w),d0
		lea	zoom_ymask,a1
		move	zoom_ypos+2,d1
		lsl	#1,d1
		lea	(a1,d1.w),a1
		move.l	a1,a2
		clr	d1
zoom_addloop:	addi	(a1)+,d1
		cmpi	d1,d0
		bpl.s	zoom_addloop
		addq	#1,-2(a1)
		tst	zoom_ypos+4
		bne.s	zoom_not00
		subq	#1,(a2)
		move	#1,zoom_ypos+4
		bra.s	zoom_not01
zoom_not00:	clr	zoom_ypos+4
zoom_not01:	tst	(a2)
		bne.s	zoom_not0
		lea	2(a2),a2
		addq	#1,zoom_ypos+2
		lea	zoom_cop,a1
		addi	#[5*zoom_row],2(a1)
		addi	#[5*zoom_row],6(a1)
		bra.s	zoom_not0
zoom_not0:	lea	zoom_cop+8,a1
zoom_setloop:	move	(a2)+,d0
		subq	#1,d0
		beq.s	zoom_nodouble
		subq	#1,d0

zoom_double:	move	#-[zoom_row-4],6(a1)	;if>1 then double
		move	#-[zoom_row-4],10(a1)
		lea	12(a1),a1
		cmpi.l	#$ffdffffe,(a1)
		bne.s	zoom_its_ok1
		lea	4(a1),a1
zoom_its_ok1:	cmpi.l	#$2a01ff00,(a1)
		beq.s	zoom_yquit
		dbf	d0,zoom_double
zoom_nodouble:	move	#[4*zoom_row]+4,6(a1)
		move	#[4*zoom_row]+4,10(a1)
		lea	12(a1),a1
		cmpi.l	#$ffdffffe,(a1)
		bne.s	zoom_nomove
		lea	4(a1),a1
zoom_nomove:	cmpi.l	#$2a01ff00,(a1)
		bne.s	zoom_setloop
zoom_yquit:	rts

;----------------------------------------------------------------------
zoom_crtab:	lea	zoom_tab16,a1
		lea	zoom_tablex,a3
		move	#15,d7
zoom_loop1:	move	(a1)+,d0
		move	#9,d6
		lea	zoom_tab10,a2
zoom_loop2:	move	(a2)+,d1
		lsl	#4,d1
		addi	d0,d1
		move	d1,(a3)+
		dbf	d6,zoom_loop2
		dbf	d7,zoom_loop1

		lea	zoom_tab16,a1
		lea	zoom_tabley,a3
		move	#15,d7
zoom_loop3:	move	(a1)+,d0
		move	#15,d6
		lea	zoom_tab16,a2
zoom_loop4:	move	(a2)+,d1
		lsl	#4,d1
		addi	d0,d1
		move	d1,(a3)+
		dbf	d6,zoom_loop4
		dbf	d7,zoom_loop3

zoom_initcop:	move.l	#$2a01ff00,d0
		move	#254,d7
		lea	zoom_cop+8,a1
zoom_initloop:	addi.l	#$1000000,d0
		move.l	d0,d1
		andi.l	#$ff000000,d1
		bne.s	zoom_inok
		move.l	#$ffdffffe,(a1)+
zoom_inok:	move.l	d0,(a1)+
		move.l	#$1080000+[4*zoom_row]+4,(a1)+
		move.l	#$10a0000+[4*zoom_row]+4,(a1)+
		dbf	d7,zoom_initloop
		rts
		
;----------------------------------------------------------------------
zoom_plane=$70000
zoom_offset=58
zoom_heigth=144
zoom_row=44
zoom_tab16:		dc.w	7,3,11,5,9,1,13,15, 4,8,0,14,2,10,6,12
zoom_tab10:		dc.w	4,2,6,0,8, 5,7,1,9,3
zoom_tablex:		blk.w	10*16,0
zoom_tabley:		blk.w	16*16,0
zoom_ymask:		blk.w	256,1
zoom_xpos:		dc.w	0
zoom_ypos:		dc.w	0,0,1
zoom_maska:		blk.b	22,0
;----------------------------------------------------------------------
zoom_copper:
dc.l	$1800000,$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
dc.l	$1a00000,$1a20000,$1a40000,$1a60000,$1a80000,$1aa0000,$1ac0000,$1ae0000
dc.l	$1b00000,$1b20000,$1b40000,$1b60000,$1b80000,$1ba0000,$1bc0000,$1be0000
dc.l	$920038,$9400d0,$8e0171,$9037d1
;	dc.l	$3401ff00,$1800ff0
;	dc.l	$3501ff00,$1800000
dc.l	$e00000+[zoom_plane/$10000],$e20000+[zoom_plane&$ffff]
dc.l	$e40000+[[zoom_plane+zoom_row]/$10000],$e60000+[[zoom_plane+zoom_row]&$ffff]
dc.l	$e80000+[[zoom_plane+[2*zoom_row]]/$10000],$ea0000+[[zoom_plane+[2*zoom_row]]&$ffff]
dc.l	$ec0000+[[zoom_plane+[3*zoom_row]]/$10000],$ee0000+[[zoom_plane+[3*zoom_row]]&$ffff]
dc.l	$f00000+[[zoom_plane+[4*zoom_row]]/$10000],$f20000+[[zoom_plane+[4*zoom_row]]&$ffff]
dc.l	$1020000
dc.l	$2a01ff00,$1005300
zoom_cop:
dc.l	$1080000+[4*zoom_row]+4,$10a0000+[4*zoom_row]+4
blk.l	[3*256]-2,0			;255 values
zoom_dd:
dc.l	$2a01ff00,$1000300
dc.l	$fffffffe

;-------------------------------------------------------------------
;------------------------dane 2 animacji----------------------------
;-------------------------------------------------------------------
l_glenz2:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		move.l	#l_plane2,scron
		move.l	#l_plane2+[2*l_heith*l_row],scroff
		clr	ubor
		move	#255,dbor
		move	#10,lbor
		move	#351,rbor
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
		jsr	l_turn
		waitblt
		raster
		move.l	#l_copper01,$dff080

l_control00:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		jsr	l_vector
		subi	#10,(a6)
		cmpi	#150,(a6)
		bpl.s	l_control00
		move	#-4,8(a6)
		move	#6,10(a6)
		bsr	l_cocop00
l_control01:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		jsr	l_vector
		addi	#10,4(a6)
		cmpi	#1000,4(a6)
		bmi.s	l_control01
		clr	l_prioryt+2
l_control02:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		jsr	l_vector
		addi	#2,2(a6)
		subi	#8,4(a6)
		addi	#4,(a6)
		cmpi	#500,(a6)
		bmi.s	l_control02
l_control03:	raster
		bsr	l_putc0
		bsr	l_chgscr0
		jsr	l_vector
		addi	#4,(a6)
		cmpi	#550,(a6)
		bmi.s	l_control03

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
Decrunch:
	lea costam(pc),a2
	move.l 4(a0),(a2)
	add.l d0,a0
	bsr.s	lbC000EAE
	rts
lbC000EAE
	move.l a1,a2
	lea costam(pc),a5
	move.l -(a0),d5
	moveq.l	#0,d1
	move.b d5,d1
	lsr.l #8,d5
	add.l d5,a1
	move.l -(a0),d5
	lsr.l d1,d5
	moveq.b	#$20,d7
	sub.b d1,d7
lbC000EC8
	bsr.s lbC000F3A
	tst.b d1
	bne.s lbC000EEE
	moveq #0,d2
lbC000ED0
	moveq.l #2,d0
	bsr.s lbC000F3C
	add.w d1,d2
	cmp.w #3,d1
	beq.s lbC000ED0
lbC000EDC
	moveq #8,d0
	bsr.s lbC000F3C
	move.b	d1,-(a1)
	dbf d2,lbC000EDC
	cmp.l	a1,a2
	bcs.s	lbC000EEE
	rts
lbC000EEE
	moveq.l #2,d0
	bsr.s	lbC000F3C
	moveq.l	#0,d0
	move.b	0(a5,d1.w),d0
	move.l	d0,d4
	move.w	d1,d2
	addq.w	#1,d2
	cmp.w	#4,d2
	bne.s lbC000F20
	bsr.s lbC000F3A
	move.l d4,d0
	tst.b d1
	bne.s lbC000F0E
	moveq.l	#7,d0
lbC000F0E
	bsr.s	lbC000F3C
	move.w	d1,d3
lbC000F12
	moveq.l #3,d0
	bsr.s lbC000F3C
	add.w	d1,d2
	cmp.w	#7,d1
	beq.s	lbC000F12
	bra.s	lbC000F24
lbC000F20
	bsr.s	lbC000F3C
	move.w	d1,d3
lbC000F24
	move.b	0(a1,d3.w),d0
	move.b	d0,-(a1)
	dbf d2,lbC000F24
	cmp.l	a1,a2
	bcs.s	lbC000EC8
	rts
lbC000F3A
	moveq.l	#1,d0
lbC000F3C
	moveq.l	#0,d1
	subq.w	#1,d0
lbC000F40
	lsr.l	#1,d5
	roxl.l	#1,d1
	subq.b	#1,d7
	bne.s	lbC000F4E
	moveq.b	#$20,d7
	move.l	-(a0),d5
lbC000F4E
	dbf d0,lbC000F40
	rts

costam	dc.l	$090A0B0B
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
dc.w	500,110,400
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

;----------------------------------------------------------------------

all_offset=$c60000		;adres pocz.danych

l_sinus=all_offset
tp_fonts=l_sinus+640		;6560
s_coltab=tp_fonts+6560
s_sine=s_coltab+356
s_altload=s_coltab+868
s_fontload=s_altload+27840			;3072
kl_sintab=s_fontload+3072
kl_lines=kl_sintab+722
kl_k1=kl_lines+722				;calosc 14086
zoom_load=kl_sintab+14086
l_mdload=zoom_load+28864			;23400
p_plasmload=l_mdload+23400			;5340

mt_data=$3c000

>extern	"df0:.store/vec_sine(256.w).dat",l_sinus,-1
>extern	"df0:.store/font24x16x4.fnt",tp_fonts,-1
>extern	"df0:.store/mod.tremendator.pro",mt_data,-1
>extern	"df0:.store/fonts1",s_fontload,-1
>extern	"df0:.store/s_col+sine.dat",s_coltab,-1
>extern	"df0:.store/alternator.raw",s_altload,-1
>extern	"df0:.store/klatki.dat",kl_sintab,-1
>extern	"df0:.store/susMD.raw",l_mdload,-1
>extern	"df0:.store/plasm.pp",p_plasmload,-1
>extern "Glupi_ArtB_2:zooming.raw",zoom_load,-1


end:
