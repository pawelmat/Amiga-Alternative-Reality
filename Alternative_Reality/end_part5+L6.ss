
exe=0

*
* Fraktal Routines	-	by CREATOR
* Scroll Routines	-	by PILLAR & CREATOR
* Podciaganie itp itd	-	by KANE
*
;do $28000 mozesz wstawic muzyke
;od $28000 do okolo $30000 progarm
;od $30000 do konca chip'u pa,iec zarezerwowana dla klatek fraktali

S:=$1748
B:=$3840
t1:=$30000
t2:=t1+[16*$1748]
screen:=$6a000

WaitB:	macro
Wait?0:	btst.b	#$000e,$dff002
	bne.s	wait?0
	endm

if exe=0
	org	$28000
	load	$28000
else
;	org	$28000
;	load	$40000
	mus=$40000-84590
endif

ss:
		if exe=0
	move.l	(a7)+,return
	movem.l	a0-a6/d0-d7,-(sp)
		endif
	lea	$30000,a0
cls:	clr.l	(a0)+
	cmpi.l	#$7fff0,a0
	bne.s	cls

	bsr	init
	lea	$dff000,a0

	move.l	#copper,$dff080
	clr.w	$dff088

		lea	czys,a6
		movem.l	(a6),d0-d7
	bsr	anim_1
		lea	czys,a6
		movem.l	(a6),d0-d7
	bsr	anim_2
		lea	czys,a6
		movem.l	(a6),d0-d7
	bsr	anim_3
		lea	czys,a6
		movem.l	(a6),d0-d7
	bsr	anim_4
		lea	czys,a6
		movem.l	(a6),d0-d7
	bsr	anim_5

Mloop:	bra	mloop

;*------------------------------------------------------------------
czys:		ds.l	8,0
;------------------------------------------------------------
oldlev6:	dc.l	0
init:
			if exe=0
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
			endif
	move	#$7fff,$dff096
	move	#$7fff,$dff09a
	bsr	mt_init
			if exe=0
	move.l	$6c,oldlev
	move.l	$78,oldlev6
			endif
	clr.b	$bfde00		;muzyka pod level6
	move.b	#$82,$bfd400
	move.b	#$37,$bfd500	;timer na $3782(80) - ok. 1 frame
	move.b	#$81,$bfdd00
	move.l	#newlev6,$78
	move.b	#$11,$bfde00
	move.l	#newlev,$6c
	move	#$e020,$dff09a
	move	#$83c0,$dff096
	rts

			if exe=0
qquit:	bsr	mt_end
	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	#$7fff,$dff09a
	move.b	#1,$bfdd00
	move.l	oldlev6,$78
	move.l	oldlev,$6c
	move	olddma,$dff096
	move	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	move.l	return,-(a7)
	rts
			endif

newlev6:	bsr	mt_music
		tst.b	$bfdd00
		move	#$2000,$dff09c
		rte

newlev:	movem.l	d0-d7/a0-a6,-(sp)
	tst	nie_wkurwiaj
	bne.s	newl3
	btst	#6,$bfe001
	beq.s	newl3
	btst	#10,$dff016
	beq.s	newl01
	tst	czyjuz
	bne.s	newl02
	move	#1,czyjuz
newl01:	bsr	vscroll
	bra.s	newl3
newl02:	clr	czyjuz
newl3:	tst.w	anim_znk
	bne.s	newl5
	bsr	anim_fraktal
	move.w	#120,anim_znk
	bra.s	newl6
newl5:	move.w	#0,anim_znk
newl6:
	lea	$dff000,a0
	movem.l	(sp)+,d0-d7/a0-a6
		if exe=0
	btst	#6,$bfe001
	bne	nopress
	btst	#10,$dff016
	bne	nopress
	move	(sp)+,storee
	move.l	(sp)+,adress
	move.l	#qquit,-(sp)
	move	storee,-(sp)
		endif
nopress:move	#$20,$dff09c
	rte

anim_znk:	dc.w	0
Anim_Fraktal:
	cmp.w	#5,ile_animacji
	bne.s	p_a_4
	bsr	play_anim_5
	rts
p_a_4:	cmp.w	#4,ile_animacji
	bne.s	p_a_3
	bsr	play_anim_4
	rts
p_a_3:	cmp.w	#3,ile_animacji
	bne.s	p_a_2
	bsr	play_anim_3
	rts
p_a_2:	cmp.w	#2,ile_animacji
	bne.s	p_a_1
	bsr	play_anim_2
	rts
p_a_1:	cmp.w	#1,ile_animacji
	bne.s	p_quit
	bsr	play_anim_1
	rts
p_quit:	rts
**************************************** frak 1
Anim_1:	bsr	clear_fraktal_screen
ANIM_L1:bsr	makeVAR
	bsr	fractal
	bsr	move_fraktal
	bsr	clear_fraktal_screen
	sub.w	#1400,xsize
	subq.w	#1,klatki
	cmpa.w	#0,klatki
	bne.s	anim_l1

	move.w	#1,ile_animacji
	rts
**************************************** frak 2
Anim_2:	bsr	clear_fraktal_screen
	move.w	#20,klatki
	move.w	#$e782,res_1
	move.w	#$eb83,res_2
ANIM_L2:bsr	makeVAR
	bsr	fractal
	bsr	move_fraktal
	bsr	clear_fraktal_screen
	add.w	#1400,xsize
	subq.w	#1,klatki
	cmpa.w	#0,klatki
	bne.s	anim_l2
	move.w	#2,ile_animacji
	rts
**************************************** frak 3
Anim_3:	bsr	clear_fraktal_screen
	move.w	#20,klatki
	move.w	#$eb82,res_1
	move.w	#$eb83,res_2
ANIM_L3:bsr	makeVAR
	bsr	fractal
	bsr	move_fraktal
	bsr	clear_fraktal_screen
	sub.w	#1400,xsize
	subq.w	#1,klatki
	cmpa.w	#0,klatki
	bne.s	anim_l3
	move.w	#3,ile_animacji
	rts
**************************************** frak 4
Anim_4:	bsr	clear_fraktal_screen
	move.w	#20,klatki
	move.w	#$eb82,res_1
	move.w	#$e583,res_2
ANIM_L4:bsr	makeVAR
	bsr	fractal
	bsr	move_fraktal
	bsr	clear_fraktal_screen
	add.w	#1400,xsize
	subq.w	#1,klatki
	cmpa.w	#0,klatki
	bne.s	anim_l4
	move.w	#4,ile_animacji
	rts
**************************************** frak 5
Anim_5:	move.l	#$81fc00a0,div
	move.l	#$323c0077,for
	move.l	#$303c009f,fory
	move.l	#where_draw_2,last_draw
	move.l	#$47eb12c0,plane2
	move.l	#$47eb12c0,plane3
	move.w	#$eb07,y_2
	move.w	#$7507,y_1
	bsr	clear_fraktal_screen_2
	move.w	#20,klatki
	move.w	#$ed82,res_1
	move.w	#$eb83,res_2
ANIM_L5:bsr	makeVAR
	bsr	fractal
	bsr	move_fraktal_2
	bsr	clear_fraktal_screen_2
	sub.w	#1400,xsize
	subq.w	#1,klatki
	cmpa.w	#0,klatki
	bne.s	anim_l5

	move.l	#$00e00006,bit_area
	move.l	#$00e2b536,bit_area+4
	move.l	#$00e40006,bit_area+8
	move.l	#$00e6c7f6,bit_area+12
	move.l	#$00e80006,bit_area+16
	move.l	#$00eadab6,bit_area+20
	move.w	#5,ile_animacji
	rts
clear_fraktal_screen:
	waitb
	move.l	#$6c030,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$01000000,$dff040
	move.w  #$0000,$dff066
	move.w  #$2614,$dff058
	rts
clear_fraktal_screen_2:
	waitb
	move.l	#$6b540,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$01000000,$dff040
	move.w  #$0000,$dff066
	move.w  #$7014,$dff058
	rts

makeVAR	lea	xsize(pc),a0
	move	xsize-t(a0),d0
	mulu	#4,d0
	divu	#5,d0
	move	d0,ysize-t(a0)
	moveq	#0,d0
	moveq	#0,d1
	move	xsize-t(a0),d0
	move	ysize-t(a0),d1
div:	dc.l	$81fc0040
	move	d0,delta-t(a0)
	move	xpos-t(a0),d0
	move	ypos-t(a0),d1
	add	xsize-t(a0),d0
	sub	ysize-t(a0),d1
	asr	#1,d0
	asr	#1,d1
	move	d0,xmax-t(a0)
	move	d1,ymin-t(a0)
	rts
**************************** Fractal Routine
fractal	move	xMax(pc),a0
	move	yMin(pc),a1
	move	delta(pc),a2
	move	a0,a4
	move.l	plane_2(pc),a5
for:	dc.l	$323c0031
ForY	dc.l	$303c003f
ForX	move.w	a0,d2
	move.w	a1,d3
	moveq	#ItMax,d7
CheckIt	move	d2,d4
	move	d3,d5
	muls	d4,d4
	muls	d5,d5
	move.l	d5,d6
	add.l	d4,d6
	swap	d6
	cmp	#$400,d6
	bgt.s	OutOfRange
	tst	d2
	bpl.s	JumpIn
	cmp	#$60,d6
	bge.s	JumpIn
	moveq	#0,d7
	bra.s	OutOfRange
Iteration
	move	d2,d4
	move	d3,d5
	muls	d4,d4
	muls	d5,d5
	move.l	d5,d6
	add.l	d4,d6
	swap	d6
	cmp	#$400,d6
	bgt.s	OutOfRange
JumpIn	exg	d4,d2
	sub.l	d5,d2
res_1:	dc.w	$e982
	swap	d2
	add	a0,d2
	bvs.s	OutOfRange
	muls	d4,d3
res_2:	dc.w	$eb83
	swap	d3
	add	a1,d3
	dbvs	d7,Iteration
	bvc.s	out
OutOfRange
	move.l	a5,a3
	move	d0,d2
	eor	#7,d2
	move	d0,d3
	lsr	#3,d3
	add	d3,a3
Plane1:	btst	#0,d7
	beq.s	Plane2
        bset	d2,(a3)
Plane2:	dc.l	$47eb07d0
	btst	#1,d7
	beq.s	Plane3
	bset	d2,(a3)
Plane3:	dc.l	$47eb07d0
	btst	#2,d7
	beq.s	out
	bset	d2,(a3)
Out:	sub	a2,a0
	dbf	d0,ForX
	move	a4,a0
	add	a2,a1
	sub	#40,a5
	dbf	d1,ForY
	rts

Move_fraktal:
	waitB
	move.l	#$6c030,$dff050
	move.l	last_draw,a0
	move.l	(a0)+,$dff054
	move.l	a0,last_draw
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts
Move_fraktal_2:
	waitB
	move.l	#$6b540,$dff050
	move.l	last_draw,a0
	move.l	(a0)+,$dff054
	move.l	a0,last_draw
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00140014,$dff064
	move.w	#$5a0a,$dff058
	rts
*************************************************** play anim 1
Play_anim_1:
	subq.w	#1,anim_1_counter
	cmpa.w	#0,anim_1_counter
	bne.s	play_1_loop
	move.l	#anim_tab_1,last_put_a_1
	move.w	#40,anim_1_counter
	move.w	#10,anim_1_skip
play_1_loop:
	sub.w	#1,anim_1_skip
	cmp.w	#0,anim_1_skip
	bne	play_1_loop
	move.w	#10,anim_1_skip

	waitb
	move.l	last_put_a_1,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_1
	move.l	#$7a000+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts
*************************************************** play anim 1+2
Play_anim_2:
	subq.w	#1,anim_2_counter
	cmpa.w	#0,anim_2_counter
	bne.s	play_2_loop
	move.l	#anim_tab_2,last_put_a_2
	move.l	#anim_tab_1,last_put_a_1
	move.w	#40,anim_2_counter
	move.w	#10,anim_2_skip
play_2_loop:
	sub.w	#1,anim_2_skip
	cmp.w	#0,anim_2_skip
	bne	play_2_loop
	move.w	#10,anim_2_skip

	waitb
	move.l	last_put_a_2,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_2
	move.l	#$7a000+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_1,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_1
	move.l	#$7a000+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts
************************************************** play anim 1+2+3
Play_anim_3:
	subq.w	#1,anim_3_counter
	cmpa.w	#0,anim_3_counter
	bne.L	play_3_loop
	move.l	#anim_tab_3,last_put_a_3
	move.l	#anim_tab_2,last_put_a_2
	move.l	#anim_tab_1,last_put_a_1
	move.w	#40,anim_3_counter
	move.w	#10,anim_3_skip
play_3_loop:

	sub.w	#1,anim_3_skip
	cmp.w	#0,anim_3_skip
	bne	play_3_loop
	move.w	#10,anim_3_skip

	waitb
	move.l	last_put_a_3,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_3
	move.l	#$7b900+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_2,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_2
	move.l	#$7a000+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_1,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_1
	move.l	#$7a000+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts
************************************************** play anim 1+2+3+4
Play_anim_4:
	subq.w	#1,anim_4_counter
	cmpa.w	#0,anim_4_counter
	bne	play_4_loop
	move.l	#anim_tab_4,last_put_a_4
	move.l	#anim_tab_3,last_put_a_3
	move.l	#anim_tab_2,last_put_a_2
	move.l	#anim_tab_1,last_put_a_1
	move.w	#40,anim_4_counter
	move.w	#10,anim_4_skip
play_4_loop:
	sub.w	#1,anim_4_skip
	cmp.w	#0,anim_4_skip
	bne	play_4_loop
	move.w	#10,anim_4_skip

	waitb
	move.l	last_put_a_4,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_4
	move.l	#$7b900+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_3,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_3
	move.l	#$7b900+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_2,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_2
	move.l	#$7a000+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_1,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_1
	move.l	#$7a000+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts
*********************************************** play anim 1+2+3+4+5
Play_anim_5:
	subq.w	#1,anim_5_counter
	cmpa.w	#0,anim_5_counter
	bne	play_5_loop
	move.l	#anim_tab_5,last_put_a_5
	move.l	#anim_tab_4,last_put_a_4
	move.l	#anim_tab_3,last_put_a_3
	move.l	#anim_tab_2,last_put_a_2
	move.l	#anim_tab_1,last_put_a_1
	move.w	#40,anim_5_counter
	move.w	#10,anim_5_skip
play_5_loop:

	sub.w	#1,anim_5_skip
	cmp.w	#0,anim_5_skip
	bne	play_5_loop
	move.w	#10,anim_5_skip

	waitb
	move.l	last_put_a_5,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_5
	move.l	#$6b540,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00140014,$dff064
	move.w	#$700a,$dff058
	waitb
	move.l	last_put_a_4,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_4
	move.l	#$7b900+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_3,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_3
	move.l	#$7b900+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_2,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_2
	move.l	#$7a000+4,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	waitb
	move.l	last_put_a_1,a0
	move.l	(a0)+,$dff050
	move.l	a0,last_put_a_1
	move.l	#$7a000+28,$dff054
	move.l	#$ffffffff,$dff044
	move.l	#$09f00000,$dff040
	move.l	#$00200020,$dff064
	move.w	#$2704,$dff058
	rts


Anim_Tab_1:
	dc.l	t1+[8*0]+[S*0],t1+[8*1]+[S*0],t1+[8*2]+[S*0]
	dc.l	t1+[8*3]+[S*0],t1+[8*4]+[S*0],t1+[8*0]+[S*1]
	dc.l	t1+[8*1]+[S*1],t1+[8*2]+[S*1],t1+[8*3]+[S*1]
	dc.l	t1+[8*4]+[S*1],t1+[8*0]+[S*2],t1+[8*1]+[S*2]
	dc.l	t1+[8*2]+[S*2],t1+[8*3]+[S*2],t1+[8*4]+[S*2]
	dc.l	t1+[8*0]+[S*3],t1+[8*1]+[S*3],t1+[8*2]+[S*3]
	dc.l	t1+[8*3]+[S*3],t1+[8*4]+[S*3],t1+[8*4]+[S*3]
	dc.l	t1+[8*3]+[S*3],t1+[8*2]+[S*3],t1+[8*1]+[S*3]
	dc.l	t1+[8*0]+[S*3],t1+[8*4]+[S*2],t1+[8*3]+[S*2]
	dc.l	t1+[8*2]+[S*2],t1+[8*1]+[S*2],t1+[8*0]+[S*2]
	dc.l	t1+[8*4]+[S*1],t1+[8*3]+[S*1],t1+[8*2]+[S*1]
	dc.l	t1+[8*1]+[S*1],t1+[8*0]+[S*1],t1+[8*4]+[S*0]
	dc.l	t1+[8*3],t1+[8*2],t1+[8*1],t1+[8*0]
Anim_tab_2:
	dc.l	t1+[8*0]+[S*4],t1+[8*1]+[S*4],t1+[8*2]+[S*4]
	dc.l	t1+[8*3]+[S*4],t1+[8*4]+[S*4],t1+[8*0]+[S*5]
	dc.l	t1+[8*1]+[S*5],t1+[8*2]+[S*5],t1+[8*3]+[S*5]
	dc.l	t1+[8*4]+[S*5],t1+[8*0]+[S*6],t1+[8*1]+[S*6]
	dc.l	t1+[8*2]+[S*6],t1+[8*3]+[S*6],t1+[8*4]+[S*6]
	dc.l	t1+[8*0]+[S*7],t1+[8*1]+[S*7],t1+[8*2]+[S*7]
	dc.l	t1+[8*3]+[S*7],t1+[8*4]+[S*7],t1+[8*4]+[S*7]
	dc.l	t1+[8*3]+[S*7],t1+[8*2]+[S*7],t1+[8*1]+[S*7]
	dc.l	t1+[8*0]+[S*7],t1+[8*4]+[S*6],t1+[8*3]+[S*6]
	dc.l	t1+[8*2]+[S*6],t1+[8*1]+[S*6],t1+[8*0]+[S*6]
	dc.l	t1+[8*4]+[S*5],t1+[8*3]+[S*5],t1+[8*2]+[S*5]
	dc.l	t1+[8*1]+[S*5],t1+[8*0]+[S*5],t1+[8*4]+[S*4]
	dc.l	t1+24+[S*4],t1+16+[S*4],t1+8+[S*4],t1+[S*4]
Anim_Tab_3:
	dc.l	t1+[8*0]+[S*8],t1+[8*1]+[S*8],t1+[8*2]+[S*8]
	dc.l	t1+[8*3]+[S*8],t1+[8*4]+[S*8],t1+[8*0]+[S*9]
	dc.l	t1+[8*1]+[S*9],t1+[8*2]+[S*9],t1+[8*3]+[S*9]
	dc.l	t1+[8*4]+[S*9],t1+[8*0]+[S*10],t1+[8*1]+[S*10]
	dc.l	t1+[8*2]+[S*10],t1+[8*3]+[S*10],t1+[8*4]+[S*10]
	dc.l	t1+[8*0]+[S*11],t1+[8*1]+[S*11],t1+[8*2]+[S*11]
	dc.l	t1+[8*3]+[S*11],t1+[8*4]+[S*11],t1+[8*4]+[S*11]
	dc.l	t1+[8*3]+[S*11],t1+[8*2]+[S*11],t1+[8*1]+[S*11]
	dc.l	t1+[8*0]+[S*11],t1+[8*4]+[S*10],t1+[8*3]+[S*10]
	dc.l	t1+[8*2]+[S*10],t1+[8*1]+[S*10],t1+[8*0]+[S*10]
	dc.l	t1+[8*4]+[S*9],t1+[8*3]+[S*9],t1+[8*2]+[S*9]
	dc.l	t1+[8*1]+[S*9],t1+[8*0]+[S*9],t1+[8*4]+[S*8]
	dc.l	t1+24+[S*8],t1+16+[S*8],t1+8+[S*8],t1+[S*8]
Anim_Tab_4:
	dc.l	t1+[8*0]+[S*12],t1+[8*1]+[S*12],t1+[8*2]+[S*12]
	dc.l	t1+[8*3]+[S*12],t1+[8*4]+[S*12],t1+[8*0]+[S*13]
	dc.l	t1+[8*1]+[S*13],t1+[8*2]+[S*13],t1+[8*3]+[S*13]
	dc.l	t1+[8*4]+[S*13],t1+[8*0]+[S*14],t1+[8*1]+[S*14]
	dc.l	t1+[8*2]+[S*14],t1+[8*3]+[S*14],t1+[8*4]+[S*14]
	dc.l	t1+[8*0]+[S*15],t1+[8*1]+[S*15],t1+[8*2]+[S*15]
	dc.l	t1+[8*3]+[S*15],t1+[8*4]+[S*15],t1+[8*4]+[S*15]
	dc.l	t1+[8*3]+[S*15],t1+[8*2]+[S*15],t1+[8*1]+[S*15]
	dc.l	t1+[8*0]+[S*15],t1+[8*4]+[S*14],t1+[8*3]+[S*14]
	dc.l	t1+[8*2]+[S*14],t1+[8*1]+[S*14],t1+[8*0]+[S*14]
	dc.l	t1+[8*4]+[S*13],t1+[8*3]+[S*13],t1+[8*2]+[S*13]
	dc.l	t1+[8*1]+[S*13],t1+[8*0]+[S*13],t1+[8*4]+[S*12]
	dc.l	t1+24+[S*12],t1+16+[S*12],t1+8+[S*12],t1+[S*12]
anim_tab_5:
	dc.l	t2,t2,t2+20,t2+20,t2+b,t2+20+b
	dc.l	t2+[b*2],t2+20+[b*2],t2+[b*3],t2+20+[b*3]
	dc.l	t2+[b*4],t2+20+[b*4],t2+[b*5],t2+20+[b*5]
	dc.l	t2+[b*6],t2+20+[b*6],t2+[b*7],t2+20+[b*7]
	dc.l	t2+[b*8],t2+20+[b*8]
;,t2+[b*9],t2+20+[b*9]

;	dc.l	t2+20+[b*9],t2+[b*9]
	dc.l	t2+20+[b*8],t2+[b*8]
	dc.l	t2+20+[b*7],t2+[b*7],t2+20+[b*6],t2+[b*6]
	dc.l	t2+20+[b*5],t2+[b*5],t2+20+[b*4],t2+[b*4]
	dc.l	t2+20+[b*3],t2+[b*3],t2+20+[b*2],t2+[b*2]
	dc.l	t2+20+b,t2+b,t2+20,t2+20,t2,t2
Where_Draw:
	dc.l	t1+[8*0]+[S*0],t1+[8*1]+[S*0],t1+[8*2]+[$*0]
	dc.l	t1+[8*3]+[S*0],t1+[8*4]+[S*0],t1+[8*0]+[S*1]
	dc.l	t1+[8*1]+[S*1],t1+[8*2]+[S*1],t1+[8*3]+[S*1]
	dc.l	t1+[8*4]+[S*1],t1+[8*0]+[S*2],t1+[8*1]+[S*2]
	dc.l	t1+[8*2]+[S*2],t1+[8*3]+[S*2],t1+[8*4]+[S*2]
	dc.l	t1+[8*0]+[S*3],t1+[8*1]+[S*3],t1+[8*2]+[S*3]
	dc.l	t1+[8*3]+[S*3],t1+[8*4]+[S*3]
	dc.l	t1+[8*0]+[S*4],t1+[8*1]+[S*4],t1+[8*2]+[S*4]
	dc.l	t1+[8*3]+[S*4],t1+[8*4]+[S*4],t1+[8*0]+[S*5]
	dc.l	t1+[8*1]+[S*5],t1+[8*2]+[S*5],t1+[8*3]+[S*5]
	dc.l	t1+[8*4]+[S*5],t1+[8*0]+[S*6],t1+[8*1]+[S*6]
	dc.l	t1+[8*2]+[S*6],t1+[8*3]+[S*6],t1+[8*4]+[S*6]
	dc.l	t1+[8*0]+[S*7],t1+[8*1]+[S*7],t1+[8*2]+[S*7]
	dc.l	t1+[8*3]+[S*7],t1+[8*4]+[S*7]
	dc.l	t1+[8*0]+[S*8],t1+[8*1]+[S*8],t1+[8*2]+[S*8]
	dc.l	t1+[8*3]+[S*8],t1+[8*4]+[S*8],t1+[8*0]+[S*9]
	dc.l	t1+[8*1]+[S*9],t1+[8*2]+[S*9],t1+[8*3]+[S*9]
	dc.l	t1+[8*4]+[S*9],t1+[8*0]+[S*10],t1+[8*1]+[S*10]
	dc.l	t1+[8*2]+[S*10],t1+[8*3]+[S*10],t1+[8*4]+[S*10]
	dc.l	t1+[8*0]+[S*11],t1+[8*1]+[S*11],t1+[8*2]+[S*11]
	dc.l	t1+[8*3]+[S*11],t1+[8*4]+[S*11]
	dc.l	t1+[8*0]+[S*12],t1+[8*1]+[S*12],t1+[8*2]+[S*12]
	dc.l	t1+[8*3]+[S*12],t1+[8*4]+[S*12],t1+[8*0]+[S*13]
	dc.l	t1+[8*1]+[S*13],t1+[8*2]+[S*13],t1+[8*3]+[S*13]
	dc.l	t1+[8*4]+[S*13],t1+[8*0]+[S*14],t1+[8*1]+[S*14]
	dc.l	t1+[8*2]+[S*14],t1+[8*3]+[S*14],t1+[8*4]+[S*14]
	dc.l	t1+[8*0]+[S*15],t1+[8*1]+[S*15],t1+[8*2]+[S*15]
	dc.l	t1+[8*3]+[S*15],t1+[8*4]+[S*15]
Where_Draw_2:
	dc.l	t2+[20*0]+[b*0],t2+[20*1]+[b*0],t2+[20*0]+[b*1]
	dc.l	t2+[20*1]+[b*1],t2+[20*0]+[b*2],t2+[20*1]+[b*2]
	dc.l	t2+[20*0]+[b*3],t2+[20*1]+[b*3],t2+[20*0]+[b*4]
	dc.l	t2+[20*1]+[b*4],t2+[20*0]+[b*5],t2+[20*1]+[b*5]
	dc.l	t2+[20*0]+[b*6],t2+[20*1]+[b*6],t2+[20*0]+[b*7]
	dc.l	t2+[20*1]+[b*7],t2+[20*0]+[b*8],t2+[20*1]+[b*8]
	dc.l	t2+[20*0]+[b*9],t2+[20*1]+[b*9]

Last_Draw:	dc.l	where_draw
anim_1_counter:	dc.w	1
anim_1_skip:	dc.w	10
Last_put_a_1:	dc.l	anim_tab_1
anim_2_counter:	dc.w	1
anim_2_skip:	dc.w	10
Last_put_a_2:	dc.l	anim_tab_2
anim_3_counter:	dc.w	1
anim_3_skip:	dc.w	10
Last_put_a_3:	dc.l	anim_tab_3
anim_4_counter:	dc.w	1
anim_4_skip:	dc.w	10
Last_put_a_4:	dc.l	anim_tab_4
Last_put_a_5:	dc.l	anim_tab_5
anim_5_counter:	dc.w	1
anim_5_skip:	dc.w	10
ile_animacji:	dc.w	0
klatki:		dc.w	20
ItMax =15
plane_2		dc.l	screen+10240-40
xsize		dc.w	5*6553+2
ysize		dc.w	0
xpos		dc.w	0
ypos		dc.w	0
delta		dc.w	0
xmax		dc.w	0
ymin		dc.w	0
t=xsize
czyjuz:		dc.w	0
	if exe=0
storee:		dc.w	0
adress:		dc.l	0
return:		dc.l	0
oldint:		dc.w	0
olddma:		dc.w	0
oldlev:		dc.l	0
gfxname:	dc.b	'graphics.library',0,0
	endif
;---------------------------------------------------------------------
*****************************************
* Pro-Packer v2.1 Replay-Routine.	*
* Based upon the PT1.1B-Replayer	*
* by Lars 'ZAP' Hamre/Amiga Freelancers.*
* Modified by Estrup/Static Bytes.	*
*****************************************
* Additional slight volume control	*
* and removing bugs done by		*
*	     Kane / Suspect		*
*****************************************

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
	MOVEM.L	D0-D7/A0-A6,-(SP)
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
	MOVEM.L	(SP)+,D0-D7/A0-A6
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

;---------------------------------------------------------------------

Plane:		dc.l	$70000
Place:		dc.l	[$70000+[32*8*44]]+7

Vscroll:
;	btst	#10,$dff016
;	beq.L	tup
	tst.w	Vcon
	bhi.s	MoveUp

Here:	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	moveq	#0,d5
	moveq	#0,d6
	move.l	#19,d7
	move.l	textpointer,a0
	move.l	Place,a2
Pline:	lea	font,a1
	move.b	(a0)+,d0
	tst.b	d0
	bne.s	Dalej
	move.l	#1,nie_wkurwiaj
	bra.L	tup
Dalej:	sub.b	#32,d0
	lsl.w	#5,d0
	move.l	#15,d6
letter:
	move.b	(a1,d0.w),1(a2,d1.w)
	move.b	1(a1,d0.w),2(a2,d1.w)
	addi.w	#40,d1
	addq.w	#2,d0
	dbf	d6,letter
	adda.l	#2,a2
	moveq	#0,d1
	moveq	#0,d0
	dbf	d7,PLine
	move.w	#16,Vcon
	move.l	a0,textpointer
;	bra	moveup
;tup:	rts

Moveup:
;wR	cmp.b	#255,$dff006
;	bne.s	wR

	move.l	plane,a0
	adda.l	#40,a0
	move.l	plane,a1

	waitb
	lea	$dff000,a6
	move.l  #$09f00000,$40(a6)
	move.l  #$ffffffff,$44(a6)
	move.l	#$00000000,$64(a6)
	move.l  a0,$50(a6)
	move.l  a1,$54(a6)
	move.w  #%0100001000101000,$58(a6)

	subq.w	#1,Vcon
tup:	rts

Vcon:	dc.w 0
nie_wkurwiaj:	dc.w	0


TextPointer:	dc.l	text

;			 12345678901234567890
Text:
		dc.b	'                    '
		dc.b	'       FINALLY      '
		dc.b	'  YOU HAVE REACHED  '
		dc.b	'      THE  TIME     '
		dc.b	' WHEN THE FORCES OF '
		dc.b	'    ...SUSPECT...   '
		dc.b	'DESTROYED THE POWERS'
		dc.b	'      OF CHAOS...   '
		dc.b	'                    '
		dc.b	'  AFTER THIS CAME   '
		dc.b	'      THE TRUE      '
		dc.b	'  AND LONG AWAITED  '
		dc.b	'       END OF       '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'     ALTERNATIVE    '
		dc.b	'       REALITY      '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'   THE VICTORIOUS   '
		dc.b	'WARRIORS OF  SUSPECT'
		dc.b	' WON THE FOLLOWING  '
		dc.b	'      BATTLES:      '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'LOADER:             '
		dc.b	'CODE & GFX  - KANE  '
		dc.b	'MUSIC       - BFA   '
		dc.b	'                    '
		dc.b	'INTRO:              '
		dc.b	'ALL CODE    - KANE  '
		dc.b	'ANIM & LOGO - ART B '
		dc.b	'FONT        - KANE  '
		dc.b	'MUSIC ("FOR ALL     '
		dc.b	' DRUNKERS") - BFA   '
		dc.b	'                    '
		dc.b	'ALL FILL VECTORS:   '
		dc.b	'CODE        - KANE  '
		dc.b	'                    '
		dc.b	'ALL TRACEVECTORS:   '
		dc.b	'CODE        - KANE  '
		dc.b	'                    '
		dc.b	'EXPLOSIONS:         '
		dc.b	'CODE        - KANE  '
		dc.b	'                    '
		dc.b	'MEGA SINUS SCROLL:  '
		dc.b	'CODE        - KANE  '
		dc.b	'GFX ("ALTERNATOR")  '
		dc.b	'            - ART B '
		dc.b	'FONT        - PYTHON'
		dc.b	'                    '
		dc.b	'VECTOR NET:         '
		dc.b	'CODE        -CREATOR'
		dc.b	'                    '
		dc.b	'RUBBERVECTORS:      '
		dc.b	'CODE        - KANE  '
		dc.b	'                    '
		dc.b	'ESCAPING FRACTALS:  '
		dc.b	'CODE (FRACTAL)      '
		dc.b	'            -CREATOR'
		dc.b	'CODE (ESCAPING)     '
		dc.b	'            - KANE  '
		dc.b	'                    '
		dc.b	'GLENZ 96 FACES:     '
		dc.b	'CODE        - KANE  '
		dc.b	'                    '
		dc.b	'ZOOMING:            '
		dc.b	'CODE        - KANE  '
		dc.b	'GFX ("FRIAR")       '
		dc.b	'            - JEZO  '
		dc.b	'                    '
		dc.b	'SUSPECT SIGN ("MD"):'
		dc.b	'CODE        - KANE  '
		dc.b	'GFX         -CRYPTON'
		dc.b	'                    '
		dc.b	'GREAT RGB PLASM:    '
		dc.b	'CODE        -CREATOR'
		dc.b	'                    '
		dc.b	'...NO LIFE:         '
		dc.b	'CODE        - KANE  '
		dc.b	'GFX         - VADER '
		dc.b	'                    '
		dc.b	'ROZETTE:            '
		dc.b	'CODE        - PILLAR'
		dc.b	'                    '
		dc.b	'GREETINGS PART:     '
		dc.b	'CODE        - PILLAR'
		dc.b	'GFX (ABSTRACTION#80)'
		dc.b	'            - ART B '
		dc.b	'                    '
		dc.b	'TIME HAS COME:      '
		dc.b	'CODE & GFX  - PILLAR'
		dc.b	'                    '
		dc.b	'"GLUT" BYE-BYE:     '
		dc.b	'CODE, ANIMATION, FX '
		dc.b	'AND DESIGN  - KANE  '
		dc.b	'                    '
		dc.b	' MUSIC TO THE MAIN  '
		dc.b	'PART ("TREMENDATOR")'
		dc.b	'      BY DAVY       '
		dc.b	'                    '
		dc.b	'END PART (THIS):    '
		dc.b	'CODE        -CREATOR'
		dc.b	'MUSIC ("EXCELLENT   '
		dc.b	' CHOP")     - KALOSZ'
		dc.b	'            - JARKO '
		dc.b	'TEXTS       - KANE  '
		dc.b	'                    '
		dc.b	'SECRET PART:        '
		dc.b	'CODE        - PILLAR'
		dc.b	'                    '
		dc.b	'                    '
		dc.b	' THE WHOLE DEMO WAS '
		dc.b	'   FIXED TOGETHER   '
		dc.b	'      BY KANE       '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'...YES, YES!!!      '
		dc.b	'THERE IS A HIDDEN   '
		dc.b	'PART IN THIS DEMO...'
		dc.b	'                    '
		dc.b	'...CLUE ONE:        '
		dc.b	'"PRESS THE LEFT ONE '
		dc.b	'   WHEN YOU START,  '
		dc.b	'    JUST TO REACH   '
		dc.b	'   THE SECRET PART "'
		dc.b	'                    '
		dc.b	'...CLUE TWO:        '
		dc.b	'"TYPE IN THE PERSON '
		dc.b	'   THAT I DISLIKE   '
		DC.B	' CHECK UP "IMMORTAL"'
		DC.B	'  IF YOU ARE BLIND "'
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		DC.B	'NOW A FEW REMARKS:  '
		DC.B	'-THE LOADER DISK RO-'
		DC.B	' TATION IS NOT A    '
		DC.B	' SIMPLE ANIMATION!!!'
		DC.B	'-YOU CAN SKIP THE   '
		DC.B	' "...NO LIFE" PART  '
		DC.B	' BY HITTING RMB.    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'        ....        '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'   NOW THE ACTUAL   '
		dc.b	'    MEMBERLIST OF   '
		dc.b	'  ... SUSPECT ...   '
		dc.b	'     IN POLAND      '
		dc.b	'  (ALPHABETICALLY)  '
		dc.b	'                    '
		dc.b	'CODERS:             '
		dc.b	'      CREATOR       '
		dc.b	'      KANE          '
		dc.b	'      PILLAR        '
		dc.b	'      TOM           '
		dc.b	'MUSICIANS:          '
		dc.b	'      BFA           '
		dc.b	'      DAVY          '
		dc.b	'GFX-MEN:            '
		dc.b	'      ART B         '
		dc.b	'      CRYPTON       '
		dc.b	'      JEZO          '
		dc.b	'      PYTHON        '
		dc.b	'SWAPPERS:           '
		dc.b	'      JARKO         '
		dc.b	'      KALOSZ        '
		dc.b	'      TRASHHEAD     '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'...THE CURSED 13... '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'NOTE:               '
		dc.b	'ANYONE ELSE WHO CLA-'
		dc.b	'IMS TO BE THE MEMBER'
		dc.b	'OF SUSPECT IN POLAND'
		dc.b	'IS EITHER FUCKIN BR-'
		dc.b	'AVE OR FUCKIN STUPID'
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'        ....        '
		dc.b	'                    '
		dc.b	' TO CONTACT OUR HQ  '
		dc.b	' JUST WRITE OR CALL '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'       KALOSZ       '
		dc.b	'    ARTUR OPALA     '
		dc.b	' UL. BENISLAWSKIEGO '
		dc.b	'       24C/13       '
		dc.b	'    81-173 GDYNIA   '
		dc.b	'       POLAND       '
		dc.b	'TEL. (0-58) 251 446 '
		dc.b	'                    '
		dc.b	' (FOR ANY SWAPPING  '
		dc.b	'  AND ORGANISATION) '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'         ..         '
		dc.b	'                    '
		dc.b	'    OR EVENTUALLY   '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'        KANE        '
		dc.b	'    PAWEL MATUSZ    '
		dc.b	'   UL. SZCZECINSKA  '
		dc.b	'        18/43       '
		dc.b	'    84-230 RUMIA    '
		dc.b	'       POLAND       '
		dc.b	'TEL. (0-58) 713 089 '
		dc.b	'                    '
		dc.b	'(FOR CODING AND ANY '
		dc.b	' SERIOUS PROBLEMS)  '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'         ..         '
		dc.b	'                    '
		dc.b	'IF YOU WISH TO KNOW '
		dc.b	' THEM, THE REST OF  '
		dc.b	' THE ADDRESSES WILL '
		dc.b	' BE PROBABLY PLACED '
		dc.b	' IN THE SECRET PART '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'       ......       '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'     ALTERNATIVE    '
		dc.b	'       REALITY      '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'  REALEASED DURING  '
		dc.b	'    THE C-PARTY     '
		dc.b	' IN WARSAW / POLAND '
		dc.b	'                    '
		dc.b	'   ON 6-8.11.1992   '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'  (C) SUSPECT 1992  '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	'                    '
		dc.b	0,0
		even

Font:
dc.w	00000,00000,00000,00000,00000,00000,00000,00000	;" "
dc.w	00000,00000,00000,00000,00000,00000,00000,00000
dc.w	02016,02016,02016,02016,02016,02016,02016,02016	;"!"
dc.w	02016,02016,00000,02016,02016,02016,02016,00000
dc.w	32382,32382,32382,32382,28784,24672,16448,00000	;"""
dc.w	00000,00000,00000,00000,00000,00000,00000,00000
dc.w	15996,15996,15996,32766,32766,15996,15996,15996	;"#"
dc.w	15996,15996,32766,32766,15996,15996,15996,00000
dc.w	00384,08190,16382,32190,32190,32128,16376,08188	;"$"
dc.w	00446,32190,32190,32190,32764,32760,00384,00000
dc.w	14350,31774,31806,31870,14588,00504,01008,02016	;"%"
dc.w	04032,08064,16156,32318,31806,30782,28700,00000
dc.w	08184,16380,32382,32382,32510,16124,03832,06004	;"&"
dc.w	15278,32222,32494,32372,32440,16348,08174,00000
dc.w	02016,02016,02016,02016,01792,01536,01024,00000	;"'"
dc.w	00000,00000,00000,00000,00000,00000,00000,00000
dc.w	00496,02016,04032,03968,08064,07936,07936,07936	;"("
dc.w	07936,07936,08064,03968,04032,02016,00504,00000
dc.w	03968,02016,01008,00496,00504,00248,00248,00248	;")"
dc.w	00248,00248,00504,00496,01008,02016,08064,00000
dc.w	00256,00256,09096,07088,08176,04064,16376,65534	;"*"
dc.w	16376,04064,08176,07088,09096,00256,00256,00000
dc.w	00000,02016,02016,02016,02016,02016,32766,32766	;"+"
dc.w	02016,02016,02016,02016,02016,00000,00000,00000
dc.w	00000,00000,00000,00000,00000,00000,00000,00000	;","
dc.w	00000,02016,02016,02016,02016,01792,01536,01024
dc.w	00000,00000,00000,00000,00000,00000,32766,32766	;"-"
dc.w	00000,00000,00000,00000,00000,00000,00000,00000
dc.w	00000,00000,00000,00000,00000,00000,00000,00000	;"."
dc.w	00000,00000,00000,02016,02016,02016,02016,00000
dc.w	00014,00030,00062,00126,00252,00504,01008,02016	;"/"
dc.w	04032,08064,16128,32256,31744,30720,28672,00000
dc.w	08184,16380,32382,32382,32382,32382,32510,32638	;"0"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	00992,00992,02016,02016,04064,04064,02016,02016	;"1"
dc.w	02016,02016,02016,02016,32766,32766,32766,00000
dc.w	32760,32764,32382,32382,32382,00126,08188,16376	;"2"
dc.w	32256,32382,32382,32382,32382,32766,32766,00000
dc.w	32760,32764,32382,32382,32382,00126,00508,00504	;"3"
dc.w	00124,32382,32382,32382,32382,32764,32760,00000
dc.w	32382,32382,32382,32382,32382,32382,16382,08190	;"4"
dc.w	00126,00126,00126,00126,00126,00126,00126,00000
dc.w	32766,32766,32382,32382,32382,32256,32760,32764	;"5"
dc.w	00126,32382,32382,32382,32382,32764,32760,00000
dc.w	08190,16382,32382,32382,32382,32256,32760,32764	;"6"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	32760,32764,32382,32382,32382,00126,00510,00510	;"7"
dc.w	00126,00126,00126,00126,00126,00126,00126,00000
dc.w	08184,16380,32382,32382,32382,15996,08184,16380	;"8"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	08184,16380,32382,32382,32382,32382,16382,08190	;"9"
dc.w	00126,32382,32382,32382,32382,32764,32760,00000
dc.w	00000,00000,02016,02016,02016,02016,00000,00000	;":"
dc.w	02016,02016,02016,02016,00000,00000,00000,00000
dc.w	00000,00000,02016,02016,02016,02016,00000,00000	;";"
dc.w	02016,02016,02016,02016,01920,01792,01536,01024
dc.w	00252,00504,01008,02016,04032,08064,16128,32256	;"<"
dc.w	16128,08064,04032,02016,01008,00504,00252,00000
dc.w	00000,00000,16380,16380,16380,16380,00000,00000	;"="
dc.w	16380,16380,16380,16380,00000,00000,00000,00000
dc.w	16128,08064,04032,02016,01008,00504,00252,00126	;">"
dc.w	00252,00504,01008,02016,04032,08064,16128,00000
dc.w	32760,32764,32766,32382,00126,00126,01022,02044	;"?"
dc.w	02040,02016,02016,00000,02016,02016,02016,00000
dc.w	08184,16380,32382,32382,32382,32382,32382,32510	;"@"
dc.w	32510,32510,32510,32510,32256,16382,08190,00000
dc.w	08184,16380,32382,32382,32382,32382,32766,32766	;"A"
dc.w	32382,32382,32382,32382,32382,32382,32382,00000
dc.w	32760,32764,32382,32382,32382,32382,32764,32760	;"B"
dc.w	32380,32382,32382,32382,32382,32764,32760,00000
dc.w	08184,16380,32382,32382,32382,32382,32382,32256	;"C"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	32760,32764,32382,32382,32382,32382,32382,32382	;"D"
dc.w	32382,32382,32382,32382,32382,32764,32760,00000
dc.w	32766,32766,32382,32382,32382,32256,32640,32640	;"E"
dc.w	32256,32382,32382,32382,32382,32766,32766,00000
dc.w	32766,32766,32382,32382,32382,32256,32640,32640	;"F"
dc.w	32256,32256,32256,32256,32256,32256,32256,00000
dc.w	08184,16380,32382,32382,32382,32256,32510,32510	;"G"
dc.w	32382,32382,32382,32382,32382,16382,08190,00000
dc.w	32382,32382,32382,32382,32382,32382,32766,32766	;"H"
dc.w	32382,32382,32382,32382,32382,32382,32382,00000
dc.w	32766,32766,02016,02016,02016,02016,02016,02016	;"I"
dc.w	02016,02016,02016,02016,02016,32766,32766,00000
dc.w	32766,32766,32382,32382,32382,00126,00510,00510	;"J"
dc.w	00126,32382,32382,32382,32382,16380,08184,00000
dc.w	32382,32382,32382,32382,32382,32382,32764,32760	;"K"
dc.w	32380,32382,32382,32382,32382,32382,32382,00000
dc.w	32256,32256,32256,32256,32256,32256,32256,32256	;"L"
dc.w	32256,32382,32382,32382,32382,32766,32766,00000
dc.w	28686,30750,31806,32382,32766,32766,32382,32382	;"M"
dc.w	32382,32382,32382,32382,32382,32382,32382,00000
dc.w	28798,30846,31870,32382,32638,32766,32510,32382	;"N"
dc.w	32382,32382,32382,32382,32382,32382,32382,00000
dc.w	08184,16380,32382,32382,32382,32382,32382,32382	;"O"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	32760,32764,32382,32382,32382,32382,32764,32760	;"P"
dc.w	32256,32256,32256,32256,32256,32256,32256,00000
dc.w	08184,16380,32382,32382,32382,32382,32382,32382	;"Q"
dc.w	32382,32510,32510,32510,32510,16382,08190,00000
dc.w	32760,32764,32382,32382,32382,32382,32764,32760	;"R"
dc.w	32380,32382,32382,32382,32382,32382,32382,00000
dc.w	08190,16382,32382,32382,32382,32256,16376,08188	;"S"
dc.w	00126,32382,32382,32382,32382,32764,32760,00000
dc.w	32766,32766,02016,02016,02016,02016,02016,02016	;"T"
dc.w	02016,02016,02016,02016,02016,02016,02016,00000
dc.w	32382,32382,32382,32382,32382,32382,32382,32382	;"U"
dc.w	32382,32382,32382,32382,32382,16380,08184,00000
dc.w	32382,32382,32382,32382,32382,32382,32382,32382	;"V"
dc.w	32382,32382,15996,07800,03696,02016,00960,00000
dc.w	32382,32382,32382,32382,32382,32382,32382,32382	;"W"
dc.w	32382,32766,32766,32382,31806,30750,28686,00000
dc.w	32382,32382,32382,32382,32382,32382,16380,08184	;"X"
dc.w	15996,32382,32382,32382,32382,32382,32382,00000
dc.w	32382,32382,32382,32382,32382,32382,16380,08184	;"Y"
dc.w	02016,02016,02016,02016,02016,02016,02016,00000
dc.w	32766,32766,32382,32382,32382,00126,08188,16376	;"Z"
dc.w	32256,32382,32382,32382,32382,32766,32766,00000
dc.w	08184,08184,08064,08064,08064,08064,08064,08064	;"["
dc.w	08064,08064,08064,08064,08064,08184,08184,00000
dc.w	28672,30720,31744,32256,16128,08064,04032,02016	;"\"
dc.w	01008,00504,00252,00126,00062,00030,00014,00000
dc.w	08184,08184,00504,00504,00504,00504,00504,00504	;"]"
dc.w	00504,00504,00504,00504,00504,08184,08184,00000
dc.w	00384,00960,02016,04080,08184,15996,31806,63519	;"^"
dc.w	00000,00000,00000,00000,00000,00000,00000,00000
dc.w	00000,00000,00000,00000,00000,00000,00000,00000	;"_"
dc.w	00000,00000,00000,00000,00000,00000,65535,65535
dc.w	02016,02016,02016,02016,00224,00096,00032,00000	;"`"
dc.w	00000,00000,00000,00000,00000,00000,00000,00000

copper:	dc.w $0092,$0038,$0094,$00d0,$0096,$0020,$0100,$0000
	dc.w $0108,$0000,$010a,$0000,$102,0

****** animowane 2 u gory
	dc.w $4007,$fffe,$0100,$4000

	dc.w $00e0,$0007,$00e2,$a000,$00e4,$0007,$00e6,$a000+[40*50]
	dc.w $00e8,$0007,$00ea,$a000+4000,$00ec,$0007,$00ee,$0000+32
	dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000
	dc.w $4107,$fffe
	dc.w $0190,$0111,$0192,$0111,$0194,$0111,$0196,$0111
	dc.w $0198,$0111,$019a,$0111,$019c,$0111,$019e,$0111
	dc.w $4207,$fffe
	dc.w $0190,$0222,$0192,$0222,$0194,$0222,$0196,$0222
	dc.w $0198,$0222,$019a,$0222,$019c,$0222,$019e,$0222
	dc.w $4307,$fffe
	dc.w $0190,$0333,$0192,$0333,$0194,$0333,$0196,$0333
	dc.w $0198,$0333,$019a,$0333,$019c,$0333,$019e,$0333
	dc.w $4407,$fffe
	dc.w $0190,$0444,$0192,$0444,$0194,$0444,$0196,$0444
	dc.w $0198,$0444,$019a,$0444,$019c,$0444,$019e,$0444
	dc.w $4507,$fffe
	dc.w $0190,$0555,$0192,$0555,$0194,$0555,$0196,$0555
	dc.w $0198,$0555,$019a,$0555,$019c,$0555,$019e,$0555
	dc.w $4607,$fffe
	dc.w $0190,$0666,$0192,$0666,$0194,$0666,$0196,$0666
	dc.w $0198,$0666,$019a,$0666,$019c,$0666,$019e,$0666
	dc.w $4707,$fffe
	dc.w $0190,$0777,$0192,$0777,$0194,$0777,$0196,$0777
	dc.w $0198,$0777,$019a,$0777,$019c,$0777,$019e,$0777
	dc.w $4807,$fffe
	dc.w $0190,$0888,$0192,$0888,$0194,$0888,$0196,$0888
	dc.w $0198,$0888,$019a,$0888,$019c,$0888,$019e,$0888
	dc.w $0180,$0000,$0182,$0116,$0184,$0127,$0186,$0238
	dc.w $0188,$0249,$018a,$035a,$018c,$036b,$018e,$047c

	dc.w $7107,$fffe,$00e0,$0007,$00e2,$7000
	dc.w $00e4,$0007,$00e6,$7000,$00e8,$0007,$00ea,$7000
****** aktualnie rysowany fraktal
y_1:	dc.w $9007,$fffe,$0100,$4000

;	dc.w $00e0,$0006,$00e2,$b534+[120*40*0]+2
;	dc.w $00e4,$0006,$00e6,$b534+[120*40*1]+2
;	dc.w $00e8,$0006,$00ea,$b534+[120*40*2]+2

Bit_area:
	dc.w $0180,$0000,$0180,$0000
	dc.w $0180,$0000,$0180,$0000
	dc.w $0180,$0000,$0180,$0000

y_2:	dc.w $c107,$fffe,$0100,$4000,$00e0,$0007,$00e2,$7000
	dc.w $00e4,$0007,$00e6,$7000,$00e8,$0007,$00ea,$7000

****** animowane 2 na dole
	dc.w $f007,$fffe,$0100,$4000,$00e0,$0007,$00e2,$b900
	dc.w $00e4,$0007,$00e6,$b900+2000,$00e8,$0007,$00ea,$c8a0
	dc.w $ffdf,$fffe,$2107,$fffe,$00e0,$0007,$00e2,$7000
	dc.w $00e4,$0007,$00e6,$7000,$00e8,$0007,$00ea,$7000

	dc.w $2807,$fffe
	dc.w $0190,$0888,$0192,$0888,$0194,$0888,$0196,$0888
	dc.w $0198,$0888,$019a,$0888,$019c,$0888,$019e,$0888

	dc.w $2907,$fffe
	dc.w $0190,$0777,$0192,$0777,$0194,$0777,$0196,$0777
	dc.w $0198,$0777,$019a,$0777,$019c,$0777,$019e,$0777
	dc.w $2a07,$fffe
	dc.w $0190,$0666,$0192,$0666,$0194,$0666,$0196,$0666
	dc.w $0198,$0666,$019a,$0666,$019c,$0666,$019e,$0666
	dc.w $2b07,$fffe
	dc.w $0190,$0555,$0192,$0555,$0194,$0555,$0196,$0555
	dc.w $0198,$0555,$019a,$0555,$019c,$0555,$019e,$0555
	dc.w $2c07,$fffe
	dc.w $0190,$0444,$0192,$0444,$0194,$0444,$0196,$0444
	dc.w $0198,$0444,$019a,$0444,$019c,$0444,$019e,$0444
	dc.w $2d07,$fffe
	dc.w $0190,$0333,$0192,$0333,$0194,$0333,$0196,$0333
	dc.w $0198,$0333,$019a,$0333,$019c,$0333,$019e,$0333
	dc.w $2e07,$fffe
	dc.w $0190,$0222,$0192,$0222,$0194,$0222,$0196,$0222
	dc.w $0198,$0222,$019a,$0222,$019c,$0222,$019e,$0222
	dc.w $2f07,$fffe
	dc.w $0190,$0111,$0192,$0111,$0194,$0111,$0196,$0111
	dc.w $0198,$0111,$019a,$0111,$019c,$0111,$019e,$0111
	dc.w $3007,$fffe
	dc.w $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	dc.w $0198,$0000,$019a,$0000,$019c,$0000,$019e,$0000

	dc.w $ffff,$fffe


if exe=0
mt_data=$20000
>extern	"df0:.store/mod.loading.pro",mt_data
end:
else
mt_data=$5000
>extern	"data2:mod.excellent.pro",mus,-1
end=$40000+[*-$28000]
endif


