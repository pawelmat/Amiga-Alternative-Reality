;	animacja kraty - skodowal CREATOR/SCT,poprawial KANE/SCT


WaitBlt:macro
Wait?0:	btst	#$000e,$dff002
	bne.s	wait?0
	endm

raster:	macro
Wait?0:	cmp.b	#$ff,$dff006
	bne.s	wait?0
	endm

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
		
org	$20000
load	$20000

;--------------------------------------------------------------------

s:
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
		addi.b	#2,(a1)
		cmpi.b	#$fe,(a1)
		bne.s	kl_setkol
		move	#$10,6(a1)
		lea	kl_dn,a1
kl_setkol2:	raster
		addi.b	#2,(a1)
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
		subi.b	#2,(a1)
		bne.s	kl_setkol3
		lea	kl_up,a1
		clr	6(a1)
kl_setkol4:	raster
		subi.b	#2,(a1)
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
		lea	mt_volumes,a4
		lea	kl_copper,a5
		move	#7,d7
kl_doloop:	move	(a4),2(a5)
		addq	#8,a5
		dbf	d7,kl_doloop
		lea	kl_copper,a5
		move	2(a4),6(a5)
		move	4(a4),14(a5)
		move	2(a4),22(a5)
		move	6(a4),30(a5)
		move	2(a4),38(a5)
		move	4(a4),46(a5)
		move	2(a4),54(a5)
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
dc.l	$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000
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
dc.l	$0801ff00,$1800000
dc.l	$fffffffe

;--------------------------------------------------------------------
kl_sintab=$c60000
kl_lines=kl_sintab+722
kl_k1=kl_lines+722

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


mt_volumes:	dc.w	$fff,$aa,$f00,$f
l_counter:	dc.w	0
;--------------------------------------------------------------------

>extern	"df0:.store/klatki.dat",kl_sintab,-1
end:
