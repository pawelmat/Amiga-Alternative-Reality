
;			««««« 2D Rozeta »»»»»
;			 coded by Pillar/SCT
;			 Gdynia 92.X.29 (PL)


; TOWARZYSZU,OBYWATELU- KANE !!!
; jakby czasem Tobie sie nudzilo to gdzies to wsadz do dema,
; jezeli kalecznie nie wyglada .....
;
; za pozytywne rozpatrzenie sprawy w okresie 2 tygodni 
; z gory dziekuje . 
;			podpisano:  PILLAR/SCT

org	$30000
load	$30000
;*------------------------------------------------------------------

s:
	movem.l	d0-d7/a0-a6,-(a7)
	move.l	lp_plane1,a0
	move.w	#%1000000000010100,d0
	bsr	lp_clear
;	move.w	$dff002,OlDMA
;	ori.w	#$8000,OlDMA
	bsr	lp_Raster
	move.l	#lp_copper,$dff080
	clr.w	$dff088
;	move.w	#%1000001111000000,$dff096

lp_loop:bsr	lp_Star
lp_lop:	cmpi.b	#$fe,$dff006
	bne.s	lp_lop
	move.l	lp_plane1,d0
	move.w	d0,lp_pl+6
	swap	d0
	move.w	d0,lp_pl+2
	move.l	lp_plane2,d0
	move.w	d0,lp_pl+14
	swap	d0
	move.w	d0,lp_pl+10
	bsr	lp_init
	eori.l	#$00005000,lp_plane1
	eori.l	#$00005000,lp_plane2
	eori.l	#$00005000,lp_pl1
	eori.l	#$00005000,lp_pl2
	subi	#1,lp_licz2
	bne.s	lp_loop
	move	#3,lp_licz2
	addi	#$112,lp_copper+6
	addi	#$202,lp_copper+10
	addi	#$202,lp_copper+14
	subi	#1,lp_licz
	bne.L	lp_loop
	move	#120,lp_licz

lp_loop01:bsr	lp_Star
lp_lop01:cmpi.b	#$fe,$dff006
	bne.s	lp_lop01
	move.l	lp_plane1,d0
	move.w	d0,lp_pl+6
	swap	d0
	move.w	d0,lp_pl+2
	move.l	lp_plane2,d0
	move.w	d0,lp_pl+14
	swap	d0
	move.w	d0,lp_pl+10
	bsr	lp_init
	eori.l	#$00005000,lp_plane1
	eori.l	#$00005000,lp_plane2
	eori.l	#$00005000,lp_pl1
	eori.l	#$00005000,lp_pl2
	subi	#1,lp_licz
	bne.s	lp_loop01
	move	#4,lp_licz

lp_loop02:bsr	lp_Star
lp_lop02:cmpi.b	#$fe,$dff006
	bne.s	lp_lop02
	move.l	lp_plane1,d0
	move.w	d0,lp_pl+6
	swap	d0
	move.w	d0,lp_pl+2
	move.l	lp_plane2,d0
	move.w	d0,lp_pl+14
	swap	d0
	move.w	d0,lp_pl+10
	bsr	lp_init
	eori.l	#$00005000,lp_plane1
	eori.l	#$00005000,lp_plane2
	eori.l	#$00005000,lp_pl1
	eori.l	#$00005000,lp_pl2
	subi	#1,lp_licz2
	bne.s	lp_loop02
	move	#3,lp_licz2
	subi	#$112,lp_copper+6
	subi	#$202,lp_copper+10
	subi	#$202,lp_copper+14
	subi	#1,lp_licz
	bne.L	lp_loop02

;	btst	#6,$bfe001
;	bne.s	lp_loop


	move.l	$4,a6
	clr.l	d0
	lea	gfxname,a1
	jsr	-$198(a6)
	move.l	$26(a0),$dff080
	clr.w	$dff088
;	move.w	OlDMA,$dff096
	movem.l	(a7)+,d0-d7/a0-a6
	rts


gfxname:	dc.b	"graphics.library",0,0
OlDMA:		dc.w	0
;*------------------------------------------------------------------

lp_init:move.l	lp_Pl1,a0
	move.w	#%1000000000010100,d0
	bsr	lp_clear
	rts

;*------------------------------------------------------------------

lp_star:move.l	lp_num2,d7
	lsr.l	#1,d7
	subq.l	#1,d7
	lea	lp_rays,a1
	lea	lp_angles,a2
lp_st0:	movem.w	(a1)+,d0-d1
	move.w	(a2)+,lp_angle
	bsr	lp_rot
	move.w	d0,d2
	move.w	d1,d3
	neg.w	d2
	neg.w	d3
	addi.w	lp_xs,d0
	addi.w	lp_xs,d2
	addi.w	lp_ys,d1
	addi.w	lp_ys,d3
	move.l	lp_plane1,a0
	bsr	lp_line
	lea	lp_blank,a3
	movem.l	(a3)+,d0-d6
	movem.w	(a1)+,d0-d1
	move.w	(a2)+,lp_angle
	bsr	lp_rot
	move.w	d0,d2
	move.w	d1,d3
	neg.w	d2
	neg.w	d3
	addi.w	lp_xs,d0
	addi.w	lp_xs,d2
	addi.w	lp_ys,d1
	addi.w	lp_ys,d3
	move.l	lp_plane2,a0
	bsr	lp_line
	lea	lp_blank,a3
	movem.l	(a3)+,d0-d6
	dbf	d7,lp_st0

	move.l	lp_num,d7
	lea	lp_angles,a0
lp_st1:	addi.w	#3,(a0)
	cmpi.w	#360,(a0)
	ble.s	lp_st2
	subi.w	#360,(a0)
lp_st2:	adda.l	#2,a0
	dbf	d7,lp_st1
	lea	lp_blank,a0
	movem.l	(a0)+,d0-d7

	tst.w	lp_con1
	beq.s	lp_stx

lp_sty:	lea	lp_rays,a0
	move.l	lp_num2,d7
	lsr.l	#1,d7
	subq.w	#1,d7
lp_st3:	addi.w	#1,2(a0)
	adda.l	#8,a0
	dbf	d7,lp_st3

	lea	lp_rays,a0
	move.l	lp_num2,d7
	lsr.l	#1,d7
	subq.w	#1,d7
lp_st4:	subi.w	#1,6(a0)
	adda.l	#8,a0
	dbf	d7,lp_st4
	bra.s	lp_stt

lp_stx:	lea	lp_rays,a0
	move.l	lp_num2,d7
	lsr.l	#1,d7
	subq.l	#1,d7
lp_st5:	subi.w	#1,2(a0)
	adda.l	#8,a0
	dbf	d7,lp_st5

	lea	lp_rays,a0
	move.l	lp_num2,d7
	lsr.l	#1,d7
	subq.l	#1,d7
lp_st6:	addi.w	#1,6(a0)
	adda.l	#8,a0
	dbf	d7,lp_st6

lp_stt:	addq.l	#1,lp_con2
	cmpi.l	#35,lp_con2
	bne.s	lp_stx6
	tst.w	lp_con1
	beq.s	lp_stt0
	clr.w	lp_con1
	bra.s	lp_stt1
lp_stt0:move.w	#1,lp_con1
lp_stt1:clr.l	lp_con2
lp_stx6:rts

lp_rot:	movem.l	d2-d7/a5,-(a7)
	move.w	lp_angle,d5
	bsr	lp_sinx
	move.l	d5,d6
	move.w	lp_angle,d5
	bsr	lp_cosx		
	move.l	d0,d3
	move.l	d1,d4
	muls	d5,d3
	muls	d6,d4
	addi.l	d3,d4
	asr.l	#7,d4	
	asr.l	#7,d4
	move.l	d0,d3
	move.l	d1,d7
	muls	d6,d3
	muls	d5,d7
	subi.l	d3,d7
	asr.l	#7,d7	
	asr.l	#7,d7
	move.l	d4,d0
	move.l	d7,d1
	movem.l	(a7)+,d2-d7/a5
	rts

lp_cosx:addi.w	#90,d5
	cmpi.w	#360,d5
	blt.s	lp_sinx
	subi.w	#360,d5
lp_sinx:lea	lp_sin_tab,a5
	lsl.w	#1,d5
	move.w	0(a5,d5.w),d5
	rts

lp_angle:	dc.w	0
lp_xs:		dc.w	160
lp_ys:		dc.w	128
lp_con2:	dc.l	0
lp_con1:	dc.w	0
;*------------------------------------------------------------------

lp_WBlt:btst	#$e,$dff002
	bne.s	lp_WBlt
	rts

;*------------------------------------------------------------------

lp_raster:
	cmpi.b	#$fe,$dff006
	bne.s	lp_raster
	rts

;*------------------------------------------------------------------
;	****************************************************
;	*	    .. DRAW LINE ROUTINE ..		   *
;	* -- Some parameters for subroutine:		   *
;	*	d0 - X1		;	d1 - Y1		   *
;	*	d2 - X2		;	d3 - Y2		   *
;	*	a0 - Bitplane Pointer			   *
;	****************************************************

lp_Line:movem.l	d0-d7/a0-a6,-(a7)
	lea	$dff000,a3
	move.l	#40,a1
	move.l	#40,d4
	mulu	d1,d4
	moveq	#-$10,d5
	andi.w	d0,d5
	lsr.w	#3,d5
	addi.w	d5,d4
	addi.l	a0,d4
	moveq	#0,d5
	subi.w	d1,d3
	roxl.b	#1,d5
	tst.w	d3
	bge.s	lp_y2gy1
	neg.w	d3
lp_y2gy1:
	subi.w	d0,d2
	roxl.b	#1,d5
	tst.w	d2
	bge.s	lp_x2gx1
	neg.w	d2
lp_x2gx1:
	move.w	d3,d1
	subi.w	d2,d1
	bge.s	lp_dygdx
	exg	d2,d3
lp_dygdx:
	roxl.b	#1,d5
	move.b	lp_Octant_table(pc,d5),d5
	addi.w	d2,d2
lp_Wblit:
	btst	#$e,$02(a3)
	bne.s	lp_WBlit
	move.w	d2,$62(a3)	;BltBMod
	subi.w	d3,d2
	bge.s	lp_signn1
	ori.b	#$40,d5
lp_signn1:
	move.w	d2,$52(a3)	;BltAPtl
	subi.w	d3,d2
	move.w	d2,$64(a3)	;BltAMod
	move.w	#$ffff,$72(a3)
	move.w	#$ffff,$74(a3)	;BltADat
	move.w	#%1000000000000000,$44(a3)	;Mask_A
	andi.w	#$000f,d0
	ror.w	#4,d0
	ori.w	#$0bca,d0
	move.w	d0,$40(a3)	;Bltcon0
	move.w	d5,$42(a3)	;Bltcon1
	move.l	d4,$48(a3)	;BltCPth
	move.l	d4,$54(a3)	;BltDPth
	move.w	a1,$60(a3)	;BltCMod
	move.w	a1,$66(a3)	;BltDMod
	lsl.w	#6,d3
	addq.w	#2,d3
	move.w	d3,$58(a3)	;BltSize
	movem.l	(a7)+,d0-d7/a0-a6
	rts


lp_Octant_table:dc.b	0*4+1
		dc.b	4*4+1
		dc.b	2*4+1
		dc.b	5*4+1
		dc.b	1*4+1
		dc.b	6*4+1
		dc.b	3*4+1
		dc.b	7*4+1
;*------------------------------------------------------------------
; Blitter Clear !!!
; d0 - blit size
; a0 - start adress of area to clear

lp_Clear:bsr.L	lp_WBLT
	lea	$dff000,a6
	move.l 	#$01000000,$40(a6)	;Bltcon0 & Bltcon1
	move.l 	a0,$48(a6)		;Source
	move.l	a0,$54(a6)		;Destination
	clr.w	$66(a6)			;Destination Mod
	move.w 	d0,$58(a6)		;BlitSize
	rts

;*------------------------------------------------------------------

lp_plane1:	dc.l	$70000
lp_plane2:	dc.l	$72800
lp_blank:	blk.l	8,0
lp_pl1:		dc.l	$75000
lp_pl2:		dc.l	$77800
lp_licz:	dc.w	4
lp_licz2:	dc.w	3

lp_Copper:
	dc.w $0180,$0000,$0182,$0000		;448,808,808
	dc.w $0184,$0000,$0186,$0000
	dc.w $0102,$0000,$0104,$0000
	dc.w $0108,$0000,$010a,$0000
lp_pl:	dc.w $00e0,$0007,$00e2,$0000
	dc.w $00e4,$0007,$00e6,$2800
	dc.w $008e,$2981,$0090,$29c1
	dc.w $0092,$0038,$0094,$00d0
	dc.w $0100,$2300
	dc.w $0120,$0000,$0122,$0000
	dc.w $0124,$0000,$0126,$0000
	dc.w $0128,$0000,$012a,$0000
	dc.w $012c,$0000,$012e,$0000
	dc.w $0130,$0000,$0132,$0000
	dc.w $0134,$0000,$0136,$0000
	dc.w $0138,$0000,$013a,$0000
	dc.w $013c,$0000,$013e,$0000
	dc.w $ffff,$fffe

;*------------------------------------------------------------------

lp_num:		dc.l	15		; l. lini dla petli
lp_num2:	dc.l	16		; rzecz. no. lini

lp_rays:
	dc.w	0,70,	0,90,	0,110,	0,50	; wsp. koncow
	dc.w	0,70,	0,90,	0,110,	0,50
	dc.w	0,70,	0,90,	0,110,	0,50
	dc.w	0,70,	0,90,	0,110,	0,50

lp_angles:
	dc.w	5,	16,	27,	38	; katy obr.
	dc.w	49,	60,	71,	82
	dc.w	93,	104,	115,	126
	dc.w	137,	148,	159,	170

;*------------------------------------------------------------------

lp_sin_tab:
	dc.w	0,$11E,$23C,$359,$477,$594,$6B1,$7CD,$8E8,$A03,$B1D
	dc.w	$C36,$D4E,$E66,$F7C,$1090,$11A4,$12B6,$13C7,$14D6
	dc.w	$15E4,$16F0,$17FA,$1902,$1A08,$1B0C,$1C0E,$1D0E
	dc.w	$1E0C,$1F07,$2000,$20F6,$21EA,$22DB,$23CA,$24B5
	dc.w	$259E,$2684,$2767,$2847,$2923,$29FD,$2AD3,$2BA6
	dc.w	$2C75,$2D41,$2E0A,$2ECE,$2F90,$304D,$3107,$31BD
	dc.w	$326F,$331D,$33C7,$346D,$350F,$35AD,$3646,$36DC
	dc.w	$376D,$37FA,$3882,$3906,$3986,$3A01,$3A78,$3AEA
	dc.w	$3B57,$3BC0,$3C24,$3C83,$3CDE,$3D34,$3D85,$3DD2
	dc.w	$3E19,$3E5C,$3E9A,$3ED3,$3F07,$3F36,$3F61,$3F86
	dc.w	$3FA6,$3FC2,$3FD8,$3FEA,$3FF6,$3FFE,$4000,$3FFE
	dc.w	$3FF6,$3FEA,$3FD8,$3FC2,$3FA6,$3F86,$3F61,$3F36
	dc.w	$3F07,$3ED3,$3E9A
	dc.w	$3E5C,$3E19,$3DD2,$3D85,$3D34,$3CDE,$3C83,$3C24
	dc.w	$3BC0,$3B57,$3AEA,$3A77,$3A01,$3986,$3906,$3882
	dc.w	$37FA,$376D,$36DC,$3646,$35AD,$350F,$346D,$33C7
	dc.w	$331D,$326F,$31BD,$3107,$304D,$2F90,$2ECE,$2E0A
	dc.w	$2D41,$2C75,$2BA6,$2AD3,$29FD,$2923,$2847,$2767
	dc.w	$2684,$259E,$24B5,$23CA,$22DB,$21EA,$20F6,$2000
	dc.w	$1F07,$1E0C,$1D0E,$1C0E,$1B0C,$1A08,$1902,$17FA
	dc.w	$16F0,$15E4,$14D6,$13C7,$12B6,$11A4,$1090,$F7C,$E66
	dc.w	$D4E,$C36,$B1D,$A03,$8E8,$7CD,$6B1,$594,$477,$359
	dc.w	$23C,$11E,0,$FEE2,$FDC4,$FCA7,$FB89,$FA6C,$F94F
	dc.w	$F833,$F718,$F5FD,$F4E3,$F3CA,$F2B2,$F19A,$F084
	dc.w	$EF70,$EE5C,$ED4A,$EC39,$EB2A,$EA1C,$E910,$E806
	dc.w	$E6FE,$E5F8,$E4F4,$E3F2,$E2F2,$E1F4,$E0F9,$E000
	dc.w	$DF0A,$DE16,$DD25,$DC36,$DB4B,$DA62,$D97C,$D899
	dc.w	$D7B9,$D6DD,$D603,$D52D,$D45A,$D38B,$D2BF,$D1F6
	dc.w	$D132,$D070,$CFB3,$CEF9,$CE43,$CD91,$CCE3,$CC39
	dc.w	$CB93,$CAF1,$CA53,$C9BA,$C924,$C893,$C806,$C77E
	dc.w	$C6FA,$C67A,$C5FF,$C588,$C516,$C4A9,$C440,$C3DC
	dc.w	$C37D,$C322,$C2CC,$C27B,$C22E,$C1E7,$C1A4,$C166
	dc.w	$C12D,$C0F9,$C0CA,$C09F,$C07A,$C05A,$C03E,$C028
	dc.w	$C016,$C00A,$C002,$C000,$C002,$C00A,$C016,$C028
	dc.w	$C03E,$C05A,$C07A,$C09F,$C0CA,$C0F9,$C12D,$C166
	dc.w	$C1A4,$C1E7,$C22E,$C27B,$C2CC,$C322,$C37D,$C3DC
	dc.w	$C440,$C4A9,$C516,$C589,$C5FF,$C67A,$C6FA,$C77E
	dc.w	$C806,$C893,$C924,$C9BA,$CA53,$CAF1,$CB93,$CC39
	dc.w	$CCE3,$CD91,$CE43,$CEF9,$CFB3,$D070,$D132,$D1F6
	dc.w	$D2BF,$D38B,$D45A,$D52D,$D603,$D6DD,$D7B9,$D899
	dc.w	$D97C,$DA62,$DB4B,$DC36,$DD25,$DE16,$DF0A,$E000
	dc.w	$E0F9,$E1F4,$E2F2,$E3F2,$E4F4,$E5F8,$E6FE,$E806
	dc.w	$E910,$EA1C,$EB2A,$EC39,$ED4A,$EE5C,$EF70,$F084
	dc.w	$F19A,$F2B2,$F3CA,$F4E3,$F5FD,$F718,$F833,$F94F
	dc.w	$FA6C,$FB89,$FCA7,$FDC4,$FEE2,0,0

;*------------------------------------------------------------------
