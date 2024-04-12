
;		*****************************************
;		*	ALTERNATIVE REALITY part I	*
;		*    ------------------------------	*
;		*					*
;		*  	  Coding on 03.10.1992		*
;		*	  by  KANE  of SUSPECT		*
;		*****************************************

exe=1
doload=1
deckula=1

waitblt:macro
wait?0:	btst.b	#14,$2(a0)
	bne.s	wait?0
	endm
raster: macro
wait?0:	cmp.b	#$fe,$dff006
	bne.s	wait?0
	cmp.b	#$ff,$dff006
	bne.s	wait?0+10
	endm


org $70000
load $70000

s:
	if exe=0
		move.l	a7,return
		movem.l	a0-a6/d0-d7,-(sp)
		move	$dff002,olddma
		ori	#$8000,olddma
		move	$dff01c,oldint
		ori	#$8000,oldint
	endif
		move	#$7fff,$dff096
		move	#$7fff,$dff09a	
		bsr	mt_init
		move.l	$6c,oldlev
		move.l	#newlev,$6c
		move	#$c020,$dff09a
		move	#$83c0,$dff096

		bsr	lo_setup

quit:
	if exe=0
		move.l	4,a6
		lea	gfxname,a1
		moveq	#0,d0
		jsr	-552(a6)
		move.l	d0,a0
		move.l	$32(a0),$dff080
	endif
		move	#150,d7
wait_loop:	raster
		dbf	d7,wait_loop
		move	#$7fff,$dff09a
;		move	#$7fff,$dff096
		bsr	mt_end
	if exe=0
		move.l	oldlev,$6c
		move.l	oldint,$dff09a
		move	olddma,$dff096
		movem.l	(sp)+,a0-a6/d0-d7
		move.l	return,a7
		rts
	endif

	if exe=1
		move	#32750,d7
		lea	$20000,a1
		lea	$5000,a2
copymus:	move.l	(a1)+,(a2)+
		dbf	d7,copymus

;		waitblt
;		clr.l	$64(a0)			;zrzuc muzyke
;		move.l	#$9f000000,$40(a0)
;		move.l	#$20000,$50(a0)
;		move.l	#$5000,$54(a0)
;		move	#0,$58(a0)		;131 kb
;		waitblt
	endif

	if exe=1
ww:		bra	ww
	endif
		rts

newlev:		movem.l	a0-a6/d0-d7,-(sp)
		bsr	cc_zrob_to
		bsr	mt_music
		move	ch_permit,d0
		beq	ch_notyper
		cmpi	#1,d0
		beq	ch_colin
		cmpi	#2,d0
		beq	ch_whiteout
		cmpi	#3,d0
		beq	ch_blueout
		bsr	ch_clear
		clr	d1
		move.l	ch_adres,a1
		move.b	(a1)+,d1
		beq	ch_stop
		bsr	ch_print
		move.l	a1,ch_adres
		move	#1,ch_permit
		move	#4,ch_time
		bra	ch_notyper
ch_colin:	addi	#$333,ch_col+2
		subi	#1,ch_time
		bpl	ch_notyper
		move	#2,ch_permit
		move	#14,ch_time
		bra	ch_notyper
ch_whiteout:	subi	#$110,ch_col+2
		subi	#1,ch_time
		bpl	ch_notyper
		move	#3,ch_permit
		move	#7,ch_time
		bra	ch_notyper
ch_blueout:	subi	#2,ch_col+2
		subi	#1,ch_time
		bpl	ch_notyper
		move	#4,ch_permit
		clr	ch_col+2
		bra	ch_notyper
ch_stop:	clr	ch_permit
ch_notyper:	movem.l	(sp)+,a0-a6/d0-d7
	if exe=0
			btst	#6,$bfe001
			bne	nopress
			move	(sp)+,storee
			move.l	(sp)+,adress
			move.l	#quit,-(sp)
			move	storee,-(sp)
	endif
nopress:move	#$20,$dff09c
	rte

;---------------------------------------------------------------------
storee:		dc.w	0
adress:		dc.l	0

return:		dc.l	0
oldint:		dc.w	0
olddma:		dc.w	0
oldlev:		dc.l	0
gfxname:	dc.b	'graphics.library',0,0
;---------------------------------------------------------------------

lo_setup:
	raster
	move.l	#cc_copper,$dff080
	clr	$dff088
		if deckula=1
	move.l	#99680,d0
	lea	ku_load,a0
	lea	ku_data,a1
	bsr	decrunch
		endif

	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	bsr	ch_stretch
	bsr	ku_initcop
	bsr	ku_anima
	waitblt
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	#lo_plane-16,$54(a0)
	move	#[3*[lo_heith+1]*64]+[lo_row/2],$58(a0)
	waitblt
	bsr	lo_initcop
	raster
	move.l	#lo_copper,$dff080

move #9,d0
lo_loop: raster
dbf d0,lo_loop

lo_control0:	raster
		addi	#$111,lo_copper+2
		addi	#$111,lo_copper+6
		addi	#$111,lo_copper+10
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		subi	#1,lo_cnt
		bpl.s	lo_control0
		move	#14,lo_cnt

lo_control:	raster
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		raster
		subi	#$11,lo_copper+2
		subi	#$110,lo_copper+6
		subi	#$10,lo_copper+10
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		subi	#1,lo_cnt
		bpl.s	lo_control
		move	#160,lo_cnt
lo_control1:	raster
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		subi	#1,lo_cnt
		bpl.s	lo_control1
		move	#14,lo_cnt
lo_control2:	raster
		addi	#$10,lo_copper+2
		addi	#$110,lo_copper+6
		subi	#1,lo_copper+6
		addi	#$10,lo_copper+10
		subi	#1,lo_copper+10
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		subi	#1,lo_cnt
		bpl.s	lo_control2
		move	#13,lo_cnt
lo_control3:	raster
		addi	#$110,lo_scolor+2
		bsr.L	lo_chgscr
		bsr.L	lo_draw
		subi	#1,lo_cnt
		bpl.s	lo_control3

		bsr	kw_setup
move #30,d0
lo_loop2: raster
dbf d0,lo_loop2
		bra	ch_setup

;---------------------------------------------------------------------
lo_chgscr:	move.l	scron,d0
		move.l	scroff,d1
		move.l	scrclr,d2
		move.l	d2,scroff
		move.l	d1,scron
		move.l	d0,scrclr
		move	d1,lo_screen+6
		move	d1,lo_screen+14
		swap	d1
		move	d1,lo_screen+2
		move	d1,lo_screen+10
		subi	#2,lo_screen+14
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[lo_heith*64]+[lo_row/2],$58(a0)
		rts
;---------------------------------------------------------------------
lo_draw:	lea	lo_cop,a1
		lea	lo_sine1,a2
		addi	#-13,lo_axidx
		andi	#$1ff,lo_axidx
		move	lo_axidx,d1
		clr	d3
		move	#lo_heith-2,d7
lo_pion_loop:	clr	d0
		addi	#4,d1
		andi	#$1ff,d1
		move.b	(a2,d1.w),d0
		move	d0,d2
		lsr	#4,d0
		lsl	#1,d0
		neg	d0
		subi	d3,d0
		addi	d0,d3
		andi	#$f,d2
		move	d0,6(a1)
		lea	12(a1),a1
		move.b	d2,3(a1)
		dbf	d7,lo_pion_loop

;---------------------------------------------------------------------
		move	lo_logo,d7
		lea	lo_logo+2,a1
		move.l	scroff,a2
		lea	lo_sine1,a3
		addi	#8,lo_axidy
		andi	#$1ff,lo_axidy
		move	lo_axidy,d6
		move	d6,d4
		addi	#60,d4
		andi	#$1ff,d4
		move	#$8000,d1
		clr	d5
		clr	d3
lo_dots:	move	(a1)+,d0
		bne.s	lo_put
lo_right:	clr	d3
		ror	#1,d1
		bpl.s	lo_next
		addq.w	#2,a2
		bra.s	lo_next
lo_put:		addi.b	(a3,d6.w),d0
		addi	#1,d6
		andi	#$1ff,d6
		addi	#-3,d4
		andi	#$1ff,d4
		exg	d4,d6
		cmpi	d0,d3
		bmi	lo_noconflict
		move	d3,d0
		addi	#2,d0
lo_noconflict:	move	d0,d3
		lsl	#5,d0
		ori	d1,(a2,d0.w)
lo_next:	dbf	d7,lo_dots


lo_fill:	clr.l	$62(a0)
		clr	$66(a0)
		move.l	#$d3c0000,$40(a0)	;a-up;b,d-down line
		move.l	scroff,d0
		move.l	d0,$50(a0)
		addi	#lo_row,d0
		move.l	d0,$4c(a0)
		move.l	d0,$54(a0)
		move	#[[lo_heith-1]*64]+[lo_row/2],$58(a0)
		rts

;---------------------------------------------------------------------
lo_initcop:	lea	lo_cop,a1
		move.l	#$6801ff00,d0
		move.l	#$1020000,(a1)+
		move.l	#$1080000,(a1)+
		move	#lo_heith-2,d1
lo_init1:	add.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1020000,(a1)+
		move.l	#$1080000,(a1)+
		dbf	d1,lo_init1
		rts

;---------------------------------------------------------------------
;			TYPER
;---------------------------------------------------------------------
ch_setup:

		lea	ch_text,a1
		move.b	(a1)+,d1
		bsr	ch_print
		bsr	ch_clear
		move.l	a1,ch_adres
		move.l	#ch_copper,$80(a0)

		move	#4,ch_permit
	if	doload=1
;		bsr	dl_seektrk0
		move	#2*25,d0
		move	#2*12,d1
		lea	$20000,a0
		bsr	dl_start
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)

	endif
		move.l	#11400,d0
		lea	zooming_pp,a0
		lea	$40000,a1
		bsr	decrunch
		lea	$dff000,a0

ch_control:	raster
		tst	ch_permit
		bne.s	ch_control
		jsr	$40000
		waitblt
		lea	$dff000,a0	;renew
		move.l	#$ffffffff,$44(a0)
		rts			;narazie wyjscie

ch_permit:	dc.w	0
ch_time:	dc.w	0
ch_adres:	dc.l	0
;-------------------------------------------------------------------
ch_print:	lea	ch_fonts,a2
		lea	ch_plane,a3
ch_sign:	clr.l	d0
		move.b	(a1)+,d0
		bmi	ch_end

		lea	ch_table,a4
		clr.l	d6
		move	#18,d7
ch_find_char:	cmp.b	(a4,d6.w),d0
		beq	ch_found
		addq	#1,d6
		dbf	d7,ch_find_char
ch_found:	divu	#5,d6
		move	d6,d0
		mulu	#40*64,d0
		swap	d6
		mulu	#8,d6
		addi	d6,d0
		lea	(a2,d0.w),a5
		lea	(a3,d1.w),a4

		move	#63,d7
ch_copy_char:	move.l	(a5)+,(a4)+
		move.l	(a5)+,(a4)+
		addi.l	#32,a5
		addi.l	#ch_row-8,a4
		dbf	d7,ch_copy_char
		addi	#8,d1
		bra	ch_sign
ch_end:		rts
;-------------------------------------------------------------------
ch_clear:	move	#[64*16]-1,d7
		lea	ch_plane,a2
ch_cloop:	clr.l	(a2)+
		dbf	d7,ch_cloop
		rts
;-------------------------------------------------------------------
ch_stretch:
		lea	ch_cop2,a1
		move.l	#$6101ff00,d0
		move.l	#$108ffc0,(a1)+
		move.l	d0,(a1)+
		move.l	#$1080000,(a1)+
		move	#62,d7
ch_strloop:	addi.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$108ffc0,(a1)+
		addi.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1080000,(a1)+
		dbf	d7,ch_strloop
		rts

;-------------------------------------------------------------------
;a0.l - destination
;d0.w - start track (side - to 160 )
;d1.w - number of sides to read (1 track = 2 sides)
;d2.l - now 0, you can choose side of disk 1-up, 2-down, 0-both

dl_start:	movem.l	d0-d7/a1-a6,-(sp)
		bsr.s	dl_readtracks
		movem.l	(sp)+,d0-d7/a1-a6
		rts

dl_readtracks:	lea	$DFF000,a6
		lea	$BFD100,a5
		lea	$BFE001,a4
		move.w	#$1002,$9A(a6)
		move.w	#$8010,$96(a6)
		clr.l	d2			;change this-head u/d
		move.l	d2,dl_side
		bsr	dl_sethead
		tst.w	dl_headpos
		bpl.s	dl_chgside
		bsr	dl_dowait1
dl_chgside:	bsr	dl_checkend
		bclr	#1,(a5)
dl_loop1:	bsr	dl_set
		bsr.s	dl_read
		subq.w	#1,dl_pos
		bgt.s	dl_loop1
		bchg	#2,(a5)
		bne.s	dl_loop2
		bsr	dl_dalay2
		bsr	dl_delay1
		addq.w	#1,dl_headpos
dl_loop2:	subq.w	#1,d1			;1 side less
		bgt.s	dl_loop1
		bset	#3,(a5)
		bset	#7,(a5)
		bclr	#3,(a5)
		rts

dl_read:	movem.l	d0-d7/a1-a6,-(sp)
		moveq	#0,d6
		lea	dl_buffer,a1
		move.l	#$55555555,d4
		moveq	#10,d5
dl_checksync:	cmp.w	#$4489,(a1)+
		bne.s	dl_checksync
		cmp.w	#$4489,(a1)
		beq.s	dl_checksync
		move.l	(a1)+,d0
		and.l	d4,d0
		add.l	d0,d0
		move.l	(a1)+,d1
		and.l	d4,d1
		or.l	d1,d0
		add.w	d0,d0
		and.w	#$1E00,d0
		lea	0(a0,d0.w),a2
		add.w	#$24,a1
		move.l	(a1)+,d0
		moveq	#9,d7
		lea	-$30(a1),a3
dl_skipsync:	move.l	(a3)+,d1
		eor.l	d1,d0
		dbra	d7,dl_skipsync

		and.l	d4,d0
		addq.w	#4,a1
		move.l	(a1)+,d2
		move.l	dl_side,d7
		lea	$200(a1),a4
		moveq	#$7F,d3
dl_decode:	move.l	(a4)+,d1
		eor.l	d1,d2
		move.l	(a1)+,d0
		eor.l	d0,d2
		and.l	d4,d0
		add.l	d0,d0
		and.l	d4,d1
		or.l	d1,d0
		eor.l	d7,d0
		move.l	d0,(a2)+
		dbra	d3,dl_decode

		dbra	d5,dl_checksync
		add.l	#$1600,a0
		movem.l	(sp)+,d0-d7/a1-a6
		rts

dl_set:		move.w	#$4000,$24(a6)
		move.l	#dl_buffer,$20(a6)
		move.w	#$4489,$7E(a6)
		move.w	#$7F00,$9E(a6)
		move.w	#$9500,$9E(a6)
		move.w	#$9900,$24(a6)
		move.w	#$9900,$24(a6)
dl_waitlast:	move.w	$1E(a6),d0
		and.w	#2,d0
		beq.s	dl_waitlast
		move.w	#2,$9C(a6)
		move.w	#$4000,$24(a6)
		rts

dl_delay1:	move.w	#$BB8,d7
dl_delayloop1:	dbra	d7,dl_delayloop1

dl_delay3:	btst	#5,(a4)
		bne.s	dl_delay3
		rts
dl_sethead:	bset	#3,(a5)
		bclr	#7,(a5)
		bclr	#3,(a5)
		bra.s	dl_delay3
dl_dowait1:	bset	#1,(a5)
dl_dowait2:	btst	#4,(a4)
		beq.s	dl_endload
		bsr.s	dl_dalay2
		bsr.s	dl_delay1
		bra.s	dl_dowait2
dl_endload:	clr.w	dl_headpos
		rts

dl_checkend:	move.w	d0,d3
		lsr.w	#1,d3
		bcc.s	dl_checkend1
		bclr	#2,(a5)
		bra.s	dl_checkend2

dl_checkend1:	bset	#2,(a5)
dl_checkend2:	tst.w	d3
		beq.s	dl_dowait1
		move.w	dl_headpos,d2
		move.w	d3,dl_headpos
		sub.w	d2,d3
		beq.s	dl_return
		bpl.s	dl_dodelay1
		bset	#1,(a5)
		neg.w	d3
		bra.s	dl_dodelay2

dl_dodelay1:	bclr	#1,(a5)
dl_dodelay2:	subq.w	#1,d3
dl_makedelay:	bsr.s	dl_dalay2
		bsr.s	dl_delay1
		dbra	d3,dl_makedelay
dl_return:	rts
dl_dalay2:	bclr	#0,(a5)
		nop
		nop
		nop
		bset	#0,(a5)
		rts

dl_pos:		dc.w	0		;292
dl_side:	dc.l	0		;294
dl_headpos:	dc.w	-1		;430

;---------------------------------------------------------------------
ku_coltab:	dc.w	$666,$aaa,$ddd,$ccc,$bbb,$aaa,$999,$888,$666,$555,$444,$333,$222,$111,0

ku_anima:
		raster
		move.l	#ku_copper0,$dff080

		move	#14,d7
ku_colin1:	move	#14,d6
		lea	ku_copper0,a1
		raster
ku_colin1_1:	addi	#$111,2(a1)
		addi.l	#4,a1
		dbf	d6,ku_colin1_1
		dbf	d7,ku_colin1

		move	#15,d7
ku_setcol:	lea	ku_copper0,a1
		lea	ku_coltab,a2
		move	#14,d6			;ilosc kolorow
ku_scol1:	move	(a2)+,d1
		cmpi	2(a1),d1
		beq.s	ku_scol2
		subi	#$111,2(a1)
ku_scol2:	addi.l	#4,a1
		dbf	d6,ku_scol1
		raster
		raster
		dbf	d7,ku_setcol

		move	#13,d7
		lea	ku_data+[56*30]+[3*3420]-1,a2
ku_promien:	raster
		clr.b	(a2)
		clr.b	1(a2)
		clr.b	30(a2)
		clr.b	31(a2)
		subi.l	#1,a2
		move.b	#-1,(a2)
		move.b	#-1,1(a2)
		move.b	#-1,30(a2)
		move.b	#-1,31(a2)
		dbf	d7,ku_promien

		move.l	#ku_data+13680,d0
		move.l	#ku_data+13680+16000,d1
		bsr.L	ku_setadr
		raster
		move.l	#ku_copper,$dff080
		move	#17,d7
ku_loop:	move	#2,d6			;animacja
ku_wait:	raster
		cmpi	#3,d7
		bpl.s	ku_colok
		bsr.s	ku_colout2
ku_colok:	dbf	d6,ku_wait
		move.l	d1,d0
		bsr.s	ku_setadr
		addi.l	#16000,d1
		dbf	d7,ku_loop
		move	#6,d7
ku_wait2:	raster
		bsr.s	ku_colout2
		dbf	d7,ku_wait2
		rts

ku_setadr:	move	d0,ku_screen+6
		swap	d0
		move	d0,ku_screen+2
		swap	d0
		addi.l	#8000,d0
		move	d0,ku_screen+14
		swap	d0
		move	d0,ku_screen+10
		rts

;---------------------------------------------------------------------
ku_colout2:	lea	ku_copper,a1
ku_colo2:	tst	2(a1)
		beq.s	ku_co22
		subi	#$111,2(a1)
ku_co22:	tst	6(a1)
		beq.s	ku_co23
		subi	#$111,6(a1)
ku_co23:	tst	10(a1)
		beq.s	ku_co24
		subi	#$111,10(a1)
ku_co24:	rts
;---------------------------------------------------------------------
ku_initcop:	move	#48,d7
		lea	ku_modulo,a1
		move.l	#$108ffd8,(a1)+
		move.l	#$10affd8,(a1)+
		move.l	#$3101ff00,(a1)+
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		move	#$31,d0
ku_init1:	addi	#4,d0
		move	d0,d5
		andi	#$ff00,d5
		beq	ku_init2
		move.l	#$ffdffffe,(a1)+
ku_init2:	andi	#$ff,d0
		move.l	d0,d5
		ror.l	#8,d5
		ori.l	#$1ff00,d5
		move.l	d5,(a1)+
		move.l	#$108ffd8,(a1)+
		move.l	#$10affd8,(a1)+
		addi.l	#$1000000,d5
		move.l	d5,(a1)+
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		addi	#1,d0
		andi	#$ff,d0
		dbf	d7,ku_init1
		rts

;---------------------------------------------------------------------
;		*****************************************
;		*	   kwadrat animation		*
;		*****************************************
kw_licz:	dc.w	7

kw_setup:
	waitblt
	lea	$dff000,a0
	move.l	#$ffffffff,$44(a0)
	waitblt
	clr	$66(a0)
	move.l	#$1000000,$40(a0)
	move.l	#kw_scrarea,$54(a0)
	move	#[3*kw_heith*64]+[kw_row],$58(a0)
	waitblt

		lea	kw_kwadrat,a6
kw_yellow:	raster
		bsr	kw_chgscr
		bsr	kw_vector
		bsr	kw_fill
		subi	#1,kw_licz
		bne.s	kw_yellow
		raster
		move.l	#kw_copper,$dff080

		lea	kw_gwiazda,a6
		move	#-10,6(a6)
kw_control:	raster
		bsr	kw_chgscr
		lea	kw_kwadrat,a6
		bsr	kw_vector
		lea	kw_gwiazda,a6
		bsr	kw_vector
		bsr	kw_fill
		subi	#10,4(a6)
		cmpi	#90,4(a6)
		bne.s	kw_control
kw_control1:	raster
		bsr	kw_chgscr
		lea	kw_kwadrat,a6
		bsr	kw_vector
		addi	#10,4(a6)
		lea	kw_gwiazda,a6
		bsr	kw_vector
		bsr	kw_fill
		addi	#10,4(a6)
		cmpi	#520,4(a6)
		bne.s	kw_control1
		lea	kw_kwadrat,a6
		move	#12,6(a6)
kw_control2:	raster
		bsr	kw_chgscr
		lea	kw_kwadrat,a6
		bsr	kw_vector
		addi	#10,4(a6)
		lea	kw_gwiazda,a6
		bsr	kw_vector
		bsr	kw_fill
		cmpi	#1040,4(a6)
		beq	kw_con21
		addi	#10,4(a6)
kw_con21:	lea	kw_kwadrat,a6
		cmpi	#1040,4(a6)
		bne.s	kw_control2
		move	#5,d7
kw_control3:	raster
		bsr	kw_chgscr
		dbf	d7,kw_control3
		rts

;----------------------------------------------------------------
kw_chgscr:	waitblt
		move	kw_scrpnt,d0
		bpl	kw_chg2
		move	#5,kw_scrpnt
		moveq	#5,d0
kw_chg2:	move	#4,d1
		lea	kw_screen+2,a1
kw_chg3:	move	d0,d2
		mulu	#[kw_row*kw_heith],d2
		add.l	#kw_scrarea,d2
		move	d2,4(a1)
		swap	d2
		move	d2,(a1)
		addq	#8,a1
		addq	#1,d0
		cmpi	#6,d0
		bne	kw_chg4
		clr	d0
kw_chg4:	dbf	d1,kw_chg3
		mulu	#[kw_row*kw_heith],d0
		add.l	#kw_scrarea,d0
		move.l	d0,kw_scroff
		subi	#1,kw_scrpnt

		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[kw_heith*64]+[kw_row/2],$58(a0)
		rts

kw_vector:	movem	6(a6),d0-d2
		add	d0,12(a6)	
		add	d1,14(a6)
		add	d2,16(a6)
		lea	kw_sinus,a1
		lea	kw_sinus+128,a4		;cosinus
		move.l	22(a6),a2		;tablica punktow
		lea	kw_matrix,a3
		move	18(a6),d6		;ilosc punktow

kw_twodim:	move	(a2)+,d1
		move	(a2)+,d2
		move	12(a6),d0
		bsr	rotate

		move	4(a6),d3		;dod.srodek z
		move	#1024,d4		;zooming calosciowy
		sub	d3,d4
		muls	d4,d1
		asr.l	#8,d1
		asr.l	#2,d1
		muls	d4,d2
		asr.l	#8,d2
		asr.l	#2,d2
		addi	(a6),d1		;dodaj srodek
		addi	2(a6),d2
		move	d1,(a3)+
		move	d2,(a3)+
		dbf	d6,kw_twodim

		move	20(a6),d7		;ilosc plaszczyzn
		lea	kw_matrix,a4
		move.l	26(a6),a3		;tab plaszczyzn
kw_obr2:	move	(a3)+,d6
kw_obr3:		move	(a3)+,d4
		move	(a3),d5
		lsl	#2,d4
		lsl	#2,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr	ldraws
		dbf	d6,kw_obr3
		lea	2(a3),a3
		dbf	d7,kw_obr2
		rts

kw_fill:	waitblt
		clr.l	$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	kw_scroff,d4
		addi.l	#[kw_heith*kw_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[kw_heith*64]+[kw_row/2],$58(a0)
		rts

ldraws:		cmpi	d1,d3
		beq	kw_noline
		bpl	dr0
		exg	d0,d2
		exg	d1,d3
dr0:		movem.l	d0-d7/a2,-(sp)
		clr	d4
		addi	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d2,d0
		bpl	dr1
		ori	#%010,d4
		neg	d0
dr1:		subi	d3,d1
		bpl	dr2
		ori	#%001,d4
		neg	d1
dr2:		cmpi	d0,d1
		bmi	dr3
		exg	d0,d1
		ori	#%100,d4
dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		swap	d7
		lea	octant,a2
		move.b	(a2,d4.w),d7
		lsl	#1,d1
		or.l	#$0b4a0003,d7
		mulu	#kw_row,d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		adda.l	kw_scroff,d5
drwait:		btst	#14,2(a0)
		bne	drwait
		move	#$8000,$74(a0)
		move	#-1,$72(a0)
		move.l	#-1,$44(a0)
		move	#kw_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl	dr4
		ori	#$40,d7
dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addi	#1,d0
		lsl	#6,d0
		addi	#2,d0
		move	d0,$58(a0)
		movem.l	(sp)+,d0-d7/a2
kw_noline:	rts


rotate:		andi.l	#$1fe,d0	;obroc punkty
		cmpi	#128,d0
		beq	rot2
		move	d1,d4
		move	d2,d5
		muls	(a4,d0.w),d4	; x*cos
		muls	(a1,d0.w),d5	; y*sin
		sub.l	d4,d5		;y'=y*sin-x*cos
		move.l	d5,d7
		andi.l	#$80000000,d7
		lsr.l	#8,d7
		or.l	d7,d5
		lsr.l	#8,d5
		muls	(a1,d0.w),d1
		muls	(a4,d0.w),d2
		add.l	d2,d1		;x'=x*sin+y*cos
		move.l	d1,d7
		andi.l	#$80000000,d7
		lsr.l	#8,d7
		or.l	d7,d1
		lsr.l	#8,d1
		move	d5,d2		;y' do d2
rot2:		rts	

;---------------------------------------------------------------------
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
;---------------------------------------------------------------------
mt_lev6use=		0		; 0=NO, 1=YES
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

;----------------------------------------------------------------------

cc_zrob_to:	tst	cc_licz
		bmi.s	cc_nie
		subi	#1,cc_licz
		bne.s	cc_nie
		tst	cc_perm
		bne.s	cc_sciemniaj
		bsr.s	setcol
		move	#2,cc_licz
		subi	#1,cc_perm+2
		bne.s	cc_nie
		move	#16,cc_perm+2
		move	#1,cc_perm
		move	#3*50,cc_licz
		bra.s	cc_nie
cc_sciemniaj:	bsr.L	fadcol
		move	#2,cc_licz
		subi	#1,cc_perm+2
		bne.s	cc_nie
		move	#-1,cc_licz
		move.l	#nic_copper,$dff080
cc_nie:		rts


setcol:	lea	cc_copper,a1
	lea	log+15488,a2
	move	#15,d3				;ilosc kolorow
scol1:	move	(a2),d1
	andi	#$f,d1
	move	2(a1),d2
	andi	#$f,d2
	cmpi	d1,d2
	beq	scol2
	addi	#1,2(a1)
scol2:	move	(a2),d1
	andi	#$f0,d1
	move	2(a1),d2
	andi	#$f0,d2
	cmpi	d1,d2
	beq	scol3
	addi	#$10,2(a1)
scol3:	move	(a2)+,d1
	andi	#$f00,d1
	move	2(a1),d2
	andi	#$f00,d2
	cmpi	d1,d2
	beq	scol4
	addi	#$100,2(a1)
scol4:	adda	#4,a1
	dbf	d3,scol1
	rts

fadcol:	lea	cc_copper,a1
	move	#15,d3
fad1:	move	2(a1),d1
	andi	#$f,d1
	beq	fad2
	subi	#1,2(a1)
fad2:	move	2(a1),d1
	andi	#$f0,d1
	beq	fad3
	subi	#$10,2(a1)
fad3:	move	2(a1),d1
	andi	#$f00,d1
	beq	fad4
	subi	#$100,2(a1)
fad4:	adda	#4,a1
	dbf	d3,fad1
	rts

cc_perm:	dc.w	0,16
cc_licz:	dc.w	30

;----------------------------------------------------------------------
cc_copper:
dc.l	$1800000,$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
dc.l	$920058,$9400a8,$8e0171,$9037d1
dc.l	$e00000+[log/$10000],$e20000+[log&$ffff]
dc.l	$e40000+[[log+[3872]]/$10000],$e60000+[[log+[3872]]&$ffff]
dc.l	$e80000+[[log+[2*3872]]/$10000],$ea0000+[[log+[2*3872]]&$ffff]
dc.l	$ec0000+[[log+[3*3872]]/$10000],$ee0000+[[log+[3*3872]]&$ffff]
dc.l	$1020000,$1080000,$10a0000
dc.l	$4f01ff00,$1004300
dc.l	$ff01ff00,$1000300
dc.l	-2
;----------------------------------------------------------------------
;---------------------------------------------------------------------
kw_copper:
dc.l	$1820770,$1840770,$1860990,$1880770,$18a0990,$18c0990,$18e0bb0
dc.l	$1900770,$1920990,$1940990,$1960bb0,$1980990,$19a0bb0,$19c0bb0,$19e0dd0
dc.l	$1a00770,$1a20990,$1a40990,$1a60bb0,$1a80990,$1aa0bb0,$1ac0bb0,$1ae0dd0
dc.l	$1b00990,$1b20bb0,$1b40bb0,$1b60dd0,$1b80bb0,$1ba0dd0,$1bc0dd0,$1be0ff0

dc.l	$1800000
dc.l	$920020,$9400d8,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
kw_screen:
dc.l	$e00007,$e20000,$e40007,$e60000,$e80007,$ea0000
dc.l	$ec0007,$ee0000,$f00007,$f20000
dc.l	$4001ff00,$01005300
dc.l	$ffdffffe
dc.l	$2001ff00,$01000300
dc.l	$1800000
dc.l	$fffffffe

;---------------------------------------------------------------------
kw_scron:		dc.l	0
kw_scroff:		dc.l	0
kw_scrpnt:		dc.w	5

kw_heith=224
kw_row=48

octant:	dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4
kw_matrix:	blk.l	20,0
;---------------------------------------------------------------------

kw_kwadrat:
dc.w	200,100,0
dc.w	0,0,0
dc.w	128,128,128
dc.w	3,0
dc.l	kw_scdots,kw_scpla
kw_scdots:				;only x,y, z=0
dc.w	-183,62
dc.w	183,62
dc.w	183,-62
dc.w	-183,-62
kw_scpla:
dc.w	3,0,1,2,3,0

kw_gwiazda:
dc.w	200,100,1020
dc.w	0,0,0
dc.w	128,128,128
dc.w	9,0
dc.l	kw_gwdots,kw_gwpla
kw_gwdots:
dc.w	0,120
 dc.w	28,40
dc.w	104,32
 dc.w	40,-16
dc.w	72,-88
 dc.w	0,-40
dc.w	-72,-88
 dc.w	-40,-16
dc.w	-104,32
 dc.w	-28,40
kw_gwpla:
dc.w	9,0,1,2,3,4,5,6,7,8,9,0

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
ch_copper:
dc.l	$920048,$9400c0,$8e0171,$9037d1
dc.l	$1020000,$1080000,$10a0000
ch_col:
dc.l	$1820000,$1800000
dc.l	$e00000+[ch_plane/$10000],$e20000+[ch_plane&$ffff]
dc.l	$6001ff00,$1009300
ch_cop2:
blk.l	[128*2]-1,0
dd:
dc.l	$e001ff00,$1000300
dc.l	$fffffffe

;---------------------------------------------------------------------
lo_plane=$7d000
scron:		dc.l	lo_plane
scroff:		dc.l	lo_plane+[lo_heith*lo_row]
scrclr:		dc.l	lo_plane+[2*lo_heith*lo_row]	;11904

lo_axidy:		dc.w	0
lo_axidx:		dc.w	180
lo_cnt:			dc.w	14

lo_heith=124
lo_row=32
;---------------------------------------------------------------------
lo_copper:
dc.l	$1820000,$1840000,$1860000
dc.l	$1800000
dc.l	$920050,$9400c8,$8e0171,$9037d1,$1020000,$1080000,$10a0000
lo_screen:
dc.l	$e00000+[lo_plane/$10000],$e20000+[lo_plane&$ffff]
dc.l	$e40000+[lo_plane/$10000],$e60000+[lo_plane&$ffff]
dc.l	$6701ff00
lo_scolor:
dc.l	$1800000
lo_scrcol:
dc.l	$6801ff00,$1002300
lo_cop:	blk.l	[3*lo_heith]-1,0
dc.l	$e301ff00,$1800000
dc.l	$e401ff00,$1000300
dc.l	$fffffffe

;---------------------------------------------------------------------
nic_copper:	dc.l	$1800000,$5001ff00,$1000300,$fffffffe,0
ku_copper:
dc.l	$1820666,$1840aaa,$1860ddd
dc.l	$1800000
dc.l	$920038,$9400d0,$8e0171,$9037d1,$1020000,$1080000,$10a0000
ku_screen:
dc.l	$e00000+[lo_plane/$10000],$e20000+[lo_plane&$ffff]
dc.l	$e40000+[lo_plane/$10000],$e60000+[lo_plane&$ffff]
dc.l	$3001ff00,$1002300
ku_modulo:
blk.l	[6*50],0
dc.l	$2a01ff00,$1000300
dc.l	$fffffffe
;---------------------------------------------------------------------
ku_copper0:
dc.l	$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
dc.l	$1800000
dc.l	$920060,$9400d0,$8e0171,$9037d1,$1020000,$1080000,$10a0000
dc.l	$e00000+[ku_data/$10000],$e20000+[ku_data&$ffff]
dc.l	$e40000+[[ku_data+3420]/$10000],$e60000+[[ku_data+3420]&$ffff]
dc.l	$e80000+[[ku_data+[2*3420]]/$10000],$ea0000+[[ku_data+[2*3420]]&$ffff]
dc.l	$ec0000+[[ku_data+[3*3420]]/$10000],$ee0000+[[ku_data+[3*3420]]&$ffff]
dc.l	$7001ff00,$1004300
dc.l	$e201ff00,$1000300
dc.l	$fffffffe
;---------------------------------------------------------------------
ch_plane=$7d000
ch_row=64

ch_text:
dc.b	18,"S",-1
dc.b	18,"YEAR",-1
dc.b	18,"2392",-1
dc.b	14,"AFTER",-1
dc.b	22,"THE",-1
dc.b	18,"LAST",-1
dc.b	22,"WAR",-1
dc.b	18,"CAME",-1
dc.b	14,"TIMES",-1
dc.b	26,"OF",-1
dc.b	06,"MADNESS",-1
dc.b	22,"AND",-1
dc.b	14,"CHAOS",-1
dc.b	14,"TIMES",-1
dc.b	26,"OF",-1,0
even
ch_table:	dc.b	"ACDEFHILMNORSTWY239"
even
;-------------------------------------------------------------------
ku_load=$1f000
ku_data=$20000
kw_scrarea=$40000
dl_buffer=$76000
;-------------------------------------------------------------------
log:
>extern "data2:logo.raw",log,-1

if exe=0
offset=$c30000				;$c10000 - exe version
else
offset=$c10000				;exe version
endif
lo_logo=offset
lo_sine1=offset+$700
ch_fonts=offset+$900			;10240
kw_sinus=offset+$900+$2800		;$280
zooming_pp=kw_sinus+$280		;$2c88
lo_end=offset+$900+$2800+$280+$2c88		;calosc:$6010
						;(24592)
;>extern	"df0:.store/64_fonts.fnt",ch_fonts,-1
;>extern	"df0:.store/sus_logo.dat",lo_logo,-1
;>extern	"df0:.store/sine(512.b).dat",lo_sine1,-1
;>extern "df0:.store/vec_sine(256.w).dat",kw_sinus,-1
;>extern	"DATA2:zoom.part.pp",zooming_pp,-1
;>extern	"DATA2:kula_anim.pp",ku_load,-1

mt_data=$5000
;>extern	"df0:mod.for.pro",mt_data,-1

end:=log+15520

