
;		*****************************************	
;		*	ALTERNATIVE REALITY LOADER	*
;		*  ----------------------------------	*
;		*	   Coded 			*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************


org	$6d000
load	$6d000

exe=1

waitblt:macro
wait?0:	btst.b	#14,$dff002
	bne	wait?0
	endm

raster: macro
wait?0:	cmp.b	#$fe,$dff006
	bne	wait?0
	cmp.b	#$ff,$dff006
	bne	wait?0+12
	endm
test:	macro
	tst	mama
	beq.s	ok?0
	bra	loadsecret
ok?0:	endm

;---------------------------------------------------------------------
	if exe=1
q:
dc.b	'DOS',0
dc.l	0
dc.l	880

	or.b	#2,$bfe001
	move.l	#reset,42(a6)			;install reset-proof
	move.l	#q,40(a1)			;dest
	move	#2,28(a1)			;read
	move.l	#66*512,36(a1)			;number
	move.l	#0,44(a1)			;offset
	jsr	-456(a6)
	jmp	st_start
reset:	lea	st_start,a0
	move	#$fffe,d0
re_2:	move.l	#0,(a0)+
	dbf	d0,re_2
	jmp	(a5)
st_start:
	lea	st_copper,a1
	move.l	a1,$dff080
st_fade:raster
	subi	#$111,2(a1)
	bne	st_fade
	endif

s:	btst.b	#2,$dff016
	bne	s_nocheat
	move	#10,light
s_nocheat:
	move.l	4,a6
	move.l	#8,d0
	move.l	#4,d1
	jsr	-198(a6)
	tst.l	d0
	bne	s_nocheat2
	move	#10,light
s_nocheat2:
	move.l #ss,$80
	trap	#0
	rts
ss:
	if exe=0
		movem.l	a0-a6/d0-d7,-(sp)
		move	$dff002,olddma
		ori	#$8000,olddma
		move	$dff01c,oldint
		ori	#$8000,oldint
	endif
		move	#$7fff,$dff09a
		move	#$7fff,$dff096
		bsr	mt_init

		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		bsr	zoom_init
		move	#[4*2048]-1,d0
		lea	dysk+2048,a1
ccllop:		clr.l	(a1)+
		dbf	d0,ccllop
		move.l	#zoom_copper,$dff080

		move.l	$6c,oldlev
		move.l	#newlev,$6c
		move	#$c020,$dff09a
		move	#$83c0,$dff096

		cmpi	#10,light
		bne	mem_ok
		bsr	nomemory
		bra	qquit
mem_ok:		bsr	setup

qquit:
	if exe=0
		move.l	4,a6
		lea	gfxname,a1
		moveq	#0,d0
		jsr	-552(a6)
		move.l	d0,a0
		move.l	$32(a0),$dff080
		move	#$7fff,$dff096
	endif
		move	#$7fff,$dff09a
	if exe=0
		move.l	oldlev,$6c
	endif
		bsr	mt_end
	if exe=0
		move	olddma,$dff096
		move	oldint,$dff09a
		movem.l	(sp)+,a0-a6/d0-d7
	else
		move	#$2100,d7
		lea	$50000,a1
		lea	$70000,a2
copy_copy:	move.l	(a1)+,(a2)+
		dbf	d7,copy_copy
		move.l	#$70000,$84
		trap	#1
	endif
		rte

newlev:movem.l d0-d7/a0-a6,-(sp)
;move	#$f,$dff180
	bsr	mt_music
	btst.b	#6,$bfe001
	bne.s	noclick
	move	#1,mama
noclick:	tst	do_it
	beq	new2
	bsr	zoom_dv
;clr	$dff180
new2:	movem.l	(sp)+,d0-d7/a0-a6
	move	#$20,$dff09c
	rte

oldlev:		dc.l	0
do_it:		dc.w	0
mama:		dc.w	0
;----------------------------------------------------------------------
setup:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		bsr	zoom_make_anim
		move	#20,d0
w_wloop:	raster
		dbf	d0,w_wloop
		move	#1,do_it
		if exe=1

		test
		move	#2*4,d0			;skad- muzyka
		move	#2*8,d1			;ile
		lea	$5000,a0		;do
		bsr	dl_start

		test
		move	#2*12,d0			;kula
		move	#2*9,d1
		lea	$1f000,a0
		bsr	dl_start

		test
		move	#2*21,d0			;kod
		move	#2*3,d1
		lea	$50000,a0
		bsr	dl_start

		test
		move	#2*24,d0			;dane-intro
		move	#2*3,d1
		lea	$c05000,a0
		bsr	dl_start

		test
		move	#2*39,d0			;glowne dane
		move	#2*16,d1
		lea	$c0e000,a0
		bsr	dl_start

		test
		move	#2*55,d0			;endpart
		move	#2*7,d1
		lea	$c6a000,a0
		bsr	dl_start

		test
		move	#2*62,d0			;main code
		move	#2*5,d1
		lea	$c3a000,a0
		bsr	dl_start
		test

		endif

		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
	if exe=0
control3:
	btst.b	#6,$bfe001
	bne.s	control3
	endif

control9:
	raster
	subi	#2,mt_percent
	cmpi	#32,mt_percent
	bpl.s	control9

		move	#15,d0
		lea	zoom_col2,a1
		move	#$ff,light
control4:	raster
		raster
		tst	6(a1)
		beq.s	cont4_1
		subi	#$111,6(a1)
cont4_1:	tst	2(a1)
		beq.s	cont4_2
		subi	#$1,2(a1)
cont4_2:	tst	10(a1)
		beq.s	cont4_3
		subi	#$100,10(a1)
cont4_3:	tst	mt_percent
		beq.s	cont4_4
		subi	#2,mt_percent
cont4_4:	dbf	d0,control4
		clr	do_it
		rts

;--------------------------------------------------------------------
loadsecret:
		move	#2*68,d0
		move	#2,d1
		lea	$30000,a0
		bsr	dl_start

control99:
	raster
	subi	#2,mt_percent
	cmpi	#32,mt_percent
	bpl.s	control99

		move	#15,d0
		lea	zoom_col2,a1
		move	#$ff,light
scontrol4:	raster
		raster
		tst	6(a1)
		beq.s	scont4_1
		subi	#$111,6(a1)
scont4_1:	tst	2(a1)
		beq.s	scont4_2
		subi	#$1,2(a1)
scont4_2:	tst	10(a1)
		beq.s	scont4_3
		subi	#$100,10(a1)
scont4_3:	tst	mt_percent
		beq.s	scont4_4
		subi	#2,mt_percent
scont4_4:	dbf	d0,scontrol4
		clr	do_it

		move	#$7fff,$dff096
		move	#$7fff,$dff09a
		move.l	oldlev,$6c
		bsr	mt_end
		move	olddma,$dff096
		move	oldint,$dff09a
		movem.l	(sp)+,a0-a6/d0-d7
		jmp	$30000

;--------------------------------------------------------------------
zoom_init:
		lea	zoom_cop3,a1
		move	#63,d1
		move.l	#$7f01ff00,d0
zoom_c1:	add.l	#$1000000,d0
		move.l	d0,(a1)+
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		dbf	d1,zoom_c1

		lea	zoom_dvdata,a1
		lea	zoom_dorder,a4
		move	#64,d0
zoom_createtab:	lsr	#4,d0			;make show_nr-tables
		move	d0,d7
		move	d0,d4
		lsl	#1,d4
		subi	#1,d7
		clr	d2
zoom_in1:	movea.l	a1,a2
		move.b	(a4)+,d1
		ext	d1
		lsl	#1,d1
		addi	d1,a2
		lea	zoom_table,a3
		move	#$f,d1
zoom_in2:	move	(a3)+,d3
		addi	d2,d3
		move	d3,(a2)
		addi	d4,a2
		dbf	d1,zoom_in2
		addi	#$10,d2
		dbf	d7,zoom_in1
		rts

;---------------------rotacja dyskietki-------------------------------

zoom_dorder:	dc.b	1,3,2,4
disk_data:	dc.w	30,1,0,1

zoom_dv:	lea	disk_data,a5
		move	2(a5),d0
		addi	d0,(a5)
		bne	zoom_no_neg
		neg	2(a5)
		move	#-1,6(a5)
zoom_no_neg:	cmpi	#31,(a5)
		bmi	zoom_no_chg
		tst	4(a5)
		beq	zoom_side_up
		move.l	#dysk+[64*8],d0
		bra	zoom_chg_side
zoom_side_up:	move.l	#dysk,d0
zoom_chg_side:	neg	2(a5)
		clr	6(a5)
		eori	#1,4(a5)
		lea	zoom_screen2,a1
		move	d0,6(a1)
		swap	d0
		move	d0,2(a1)
		swap	d0
		add.l	#128*8,d0
		move	d0,14(a1)
		swap	d0
		move	d0,10(a1)
zoom_no_chg:	lea	zoom_cop3+4,a1		;zoom vertically
		lea	zoom_dvdata,a3
		move	(a5),d0
		lea	dysk_sine,a4
		move.b	(a4,d0.w),d0
		ext	d0
		move	d0,d1
		move	d0,d5			;light analyze

		cmpi	#$ff,light
		beq	zoom_nolight
		andi	#$3f,d5
		eori	#$3f,d5
		lsr	#3,d5
		lea	zoom_col2,a4
		move	d5,d4
		lsr	#1,d4
		addi	#3,d4
		move	d4,6(a4)
		lsl	#4,d4
		ori	d4,6(a4)
		lsl	#4,d4
		ori	d4,6(a4)
		addi	#8,d5
		move	d5,2(a4)
		lsl	#8,d5
		move	d5,10(a4)

		move	d0,d5
zoom_nolight:	lsr	#2,d5			;perspective counter
		move	#63,d7
		lsr	#1,d1
		beq	zoom_dnoskip
		subi	d1,d7
		subi	#1,d1
zoom_dskip1:	move	#-8,2(a1)		;skip empty upper
		move	#-8,6(a1)
		adda.l	#12,a1
		dbf	d1,zoom_dskip1

zoom_dnoskip:	move	#62,d6
		clr	d1
		clr	d4
		tst	6(a5)
		beq	zoom_dloop
		move	d5,d1
		mulu	#2048,d1
zoom_dloop:	addi	d5,d4
		cmpi	#63,d4
		bmi	zoom_no_persp
		subi	#63,d4
		tst	6(a5)
		beq	zoom_add_p
		subi	#2048,d1
		bra	zoom_no_persp
zoom_add_p:	addi	#2048,d1
zoom_no_persp:	cmpi	(a3)+,d0		;show actual lines
		bpl	zoom_dskipline
		move	d1,2(a1)
		move	d1,6(a1)
		adda.l	#12,a1
		subi	#1,d7
		clr	d1
		bra	zoom_dloopchk
zoom_dskipline:	addi	#8,d1
zoom_dloopchk:	dbf	d6,zoom_dloop

		move	#64,d0
		mulu	#8,d0
		subi	d1,d0			;return to first line
		neg	d0
		move	d0,2(a1)
		move	d0,6(a1)
		adda.l	#12,a1
		tst	d7
		beq	zoom_dvend
		subi	#1,d7
zoom_dskip2:	move	#-8,2(a1)		;eventually skip lower
		move	#-8,6(a1)
		adda.l	#12,a1
		dbf	d7,zoom_dskip2
zoom_dvend:	rts

;----------------------------------------------------------------------
zoom_make_anim:	lea	dysk,a5
		waitblt
		clr	$42(a0)
		move	#6,$60(a0)
		move.l	#$60006,$64(a0)
		clr	d0
zoom_anim_loop:	add.l	#2048,a5
		move.l	a5,a2
		addi	#2,d0
		cmpi	#32,d0
		bne	zoom_frame
		rts

zoom_frame:	lea	dysk,a1		;zoom horizontally
		lea	zoom_dvdata,a3
		move	d0,d1
		lsr	#1,d1
		move	d1,d5
		lsr	#3,d1
		lea	(a2,d1.w),a2		;word dest offset
		andi	#$f,d5			;bit offset
		clr	d4
		move	d5,d1
		eori	#$f,d1
		bset	d1,d4			;for B mask
		move	#3,d7			;word x pic counter
		move	#0,d6			;pixel x counter
zoom_an_loop:	cmpi	(a3)+,d0
		bpl	zoom_an_skip
		move	d5,d1
		subi	d6,d1			;delta (x pixels)
		bpl	zoom_an_plus
		lea	-8(a2),a4
		bra	zoom_an_ok
zoom_an_plus:	lea	(a2),a4
zoom_an_ok:	addi	#$10,d1
		andi	#$f,d1
		ror	#4,d1
		ori	#$be2,d1
		waitblt
		move	d1,$40(a0)
		move	d4,$72(a0)
		move.l	a1,$50(a0)
		move.l	a4,$48(a0)
		move.l	a4,$54(a0)
		move	#[256*64]+1,$58(a0)
		ror	#1,d4
		addi	#1,d5
		cmpi	#$10,d5
		bne	zoom_an_skip
		lea	2(a2),a2
		move	#0,d5
zoom_an_skip:	addi	#1,d6
		cmpi	#$10,d6
		bne	zoom_an_loop
		lea	2(a1),a1
		move	#0,d6
		dbf	d7,zoom_an_loop
		bra	zoom_anim_loop

;----------------------------------------------------------------------
nomemory:	move.l	#zoom_ncopper,$dff080
		move	#14,d0
nomlo:		raster
		raster
		addi	#$100,zoom_ncol+2
		dbf	d0,nomlo
	if exe=0
nocon:		btst.b	#6,$bfe001
		bne	nocon
		rts
	else
nocon:		bra	nocon
	endif
;----------------------------------------------------------------------
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
;----------------------------------------------------------------------
;input:
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

;----------------------------------------------------------------------
mt_lev6use=		1		; 0=NO, 1=YES
mt_finetuneused=	1		; 0=NO, 1=YES

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
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
;----------------------------------------------------------------------
zoom_table:	dc.w	16,6,10,4,14,2,8,12,11,7,1,13,3,9,5,15
light:	dc.w	0

dysk_sine:
dc.b	$00,$00,$02,$02,$03,$03,$04,$05,$06,$08,$09,$0b,$0d,$0f,$11,$13
dc.b	$15,$18,$1a,$1d,$1f,$22,$25,$28,$2b,$2e,$31,$34,$37,$3a,$3d,$3f

;----------------------------------------------------------------------
st_copper:	dc.l	$1800fff,$fffffffe,0
zoom_copper:
dc.l	$920078,$940090,$1080000,$10a0000,$1020000,$8e0171,$9037d1
zoom_col2:
dc.l	$1820000,$1840000,$1860000,$1800000
zoom_screen2:
dc.l	$e00000+[dysk/$10000],$e20000+[dysk&$ffff]
dc.l	$e40000+[dysk/$10000],$e60000+[dysk&$ffff]
dc.l	$8001ff00,$1002300,$1800000
zoom_cop3:
ds.l	[3*64],0
dc.l	$c001ff00,$1000300
dc.l	$ffffffff

;----------------------------------------------------------------------
zoom_ncopper:
dc.l	$920048,$9400c0,$1080000,$10a0000,$1020000,$8e0171,$9037d1
zoom_ncol:
dc.l	$1820000,$1800000
dc.l	$e00000+[memp/$10000],$e20000+[memp&$ffff]
dc.l	$7001ff00,$1001300
dc.l	$d901ff00,$1000300
dc.l	$ffffffff

zoom_dvdata:
ds.w	70,0

mt_data:
memp=mt_data+20180
dysk=memp+3360
dl_buffer=dysk+[16*2048]		;$3400

>extern "df0:.store/memory_info.pic",memp,-1
>extern "df0:.store/dyskietka.pic",dysk,-1
>extern	"df0:.store/mod.loading.pro",mt_data,-1
end=dysk+2048

