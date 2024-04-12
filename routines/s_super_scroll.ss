
;		*****************************************
;		*	MEGASINUS SCROLL + picture	*
;		*  ----------------------------------	*
;		*	   Coded on 17.09.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org	$30000
load	$30000

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

s:	move.l	#s_ss,$80
	trap	#0
	rts

s_ss:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
	move	#$7fff,$dff09a
	move	#$7fff,$dff096
	move	#$87c0,$dff096

	bsr	s_scroll

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$32(a0),$dff080
	move	olddma,$dff096
	move.l	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rte

;----------------------------------------------------------------------
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
s_scrolluj:	waitblt
		raster
		move.l	#s_copper,$dff080
s_control:	raster
		bsr	s_chg
		bsr	s_move
		lea	s_data,a3
		tst	12(a3)
		bne.s	s_control
		clr	d6
s_schowaj0:	move	#2,d7
s_schowaj:	raster
		waitblt
		clr.l	d0
		move	d6,d0
		ror.l	#4,d0
		ori.l	#$9f00000,d0
		move.l	d0,$40(a0)
		clr.l	$64(a0)
		clr	$46(a0)
		move.l	#s_alt,$50(a0)
		move.l	#s_alt,$54(a0)
		move	#[3*232*64]+22,$58(a0)
		dbf	d7,s_schowaj
		addi	#1,d6
		cmpi	#16,d6
		bne.s	s_schowaj0
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
		lea	-18(a2),a2
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
		move	#239,d7
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
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
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

;----------------------------------------------------------------------

scron:		dc.l	$7a000
scroff:		dc.l	$7a000+[[s_heigth+2]*s_row]
l_octant:	dc.b	7*4,5*4,6*4,4*4,3*4,2*4,1*4,0*4

s_coltab=$c40000
s_sine=s_coltab+356
s_altload=s_coltab+868
s_fontload=s_altload+27840			;3072
>extern	"df0:.store/fonts1",s_fontload,-1
>extern	"df0:.store/s_col+sine.dat",s_coltab,-1
>extern	"df0:.store/alternator.raw",s_altload,-1

end:
