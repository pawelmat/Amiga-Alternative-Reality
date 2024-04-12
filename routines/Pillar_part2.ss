
*		HU HU HU !!!!
*			To sa czesci PILLARA/SCT
*				zlozone w jeden kawal kodu!!!
*					do ALTERNATIVE REALITY !!!
*	YO YO!!!! LIFE IS NIRVANA!!!! (coding toooooo!!!)
*		data:	dc.l*8	30.10.1992
*						(Kane/SCT)

; TOWARZYSZU,OBYWATELU- KANE !!!
; jakby czasem Tobie sie nudzilo to gdzies to wsadz do dema,
; jezeli kalecznie nie wyglada .....
;
; za pozytywne rozpatrzenie sprawy w okresie 2 tygodni 
; z gory dziekuje . 
;			podpisano:  PILLAR/SCT

;Partia rulez, wniosek przyjety... podp. Kane/partia rewolucyjna SCT

org	$60000
load	$60000
;*------------------------------------------------------------------

s:
	movem.l	d0-d7/a0-a6,-(a7)
;	move	#$4000,$dff09a
	bsr	rozeta
	bsr	no_glebiej
	bsr	credits
	bsr	no_glebiej
	bsr	timehascome
	movem.l	(a7)+,d0-d7/a0-a6
;	move	#$c000,$dff09a
	rts

no_glebiej:	move	#15,d7
no_jeszcze:	cmpi.b	#$fe,$dff006
		bne.s	*-10
		cmpi.b	#$ff,$dff006
		bne.s	*-10
		dbf	d7,no_jeszcze
		rts

rozeta:
r_lop22:cmpi.b	#$ff,$dff006
	bne.s	r_lop22
	lea	$dff000,a6
blit06:	btst.b	#$e,$dff002
	bne.s	Blit06
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

	rts

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

lp_WBlt:btst.b	#$e,$dff002
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
	btst.b	#$e,$02(a3)
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
credits:
	lea	$70000,a0
cls:	clr.l	(a0)+
	cmpa.l	#$7fff0,a0
	bne.s	cls
	bsr	setpic
Hendi:	cmpi.b	#$ff,$dff006
	bne.s	hendi
	move.l	#copper,$dff080
	clr.w	$dff088

		lea	scrup,a1
		lea	scrdn,a2
ramkuj:		cmpi.b	#$fe,$dff006
		bne.s	ramkuj
		cmpi.b	#$ff,$dff006
		bne.s	ramkuj+10
		addi.b	#8,9(a1)
		addi.b	#8,13(a2)
		cmpi.b	#$ff,13(a2)
		bne.s	ramkuj

	move	#%0110100100000000,scron+2
	move	#15,d0
setcol:	lea	Kolors,a1
	lea	klatka+15360,a2
	move	#30,d3
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
	move	#$1c00,d3
waitt:	muls	d1,d1
	dbf	d3,waitt
	dbf	d0,setcol

	move.l	#25,con0
mloop1:	cmpi.b	#$ff,$dff006
	bne.s	mloop1
	bsr	vscroll
	subq.l	#1,con0
	bne.s	mloop1

	move.l	#44,con0
mloopx:	cmpi.b	#$ff,$dff006
	bne.s	mloopx
	move.l	#$ff,d0
dox:	nop
	dbf	d0,dox
	subq.l	#1,con0
	bne.s	mloopx

Mloop2:	cmpi.b	#$ff,$dff006
	bne.s	mloop2
	bsr.L	Vscroll
	tst	hendvar
	beq.s	Mloop2

Hend:	move	#7,d7
fadcol:	lea	Kolors,a1
	move	#30,d3
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
	move	#$1c00,d6
wait5:;	muls	d1,d1
	dbf	d6,wait5
	dbf	d7,fadcol
	move	#$300,scron+2

		lea	scrup,a1
		lea	scrdn,a2
ramkuj2:	cmpi.b	#$fe,$dff006
		bne.s	ramkuj2
		cmpi.b	#$ff,$dff006
		bne.s	ramkuj2+10
		addi.b	#8,1(a1)
		addi.b	#8,1(a2)
		cmpi.b	#$ff,1(a2)
		bne.s	ramkuj2

	rts

con0:		dc.l	0
;*------------------------------------------------------------------
SetPic:	lea	$dff000,a6
blit0:	btst.b	#$e,$02(a6)
	bne.s	Blit0
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka1,$50(a6)
	move.l  Place1,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit1:	btst.b	#$e,$02(a6)
	bne.s	Blit1
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka2,$50(a6)
	move.l  Place2,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit2:	btst.b	#$e,$02(a6)
	bne.s	Blit2
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka3,$50(a6)
	move.l  Place3,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit3:	btst.b	#$e,$02(a6)
	bne.s	Blit3
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka4,$50(a6)
	move.l  place4,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit4:	btst.b	#$e,$02(a6)
	bne.s	Blit4
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka5,$50(a6)
	move.l  place5,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit5:	btst.b	#$e,$02(a6)
	bne.s	Blit5
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka6,$50(a6)
	move.l  place6,$54(a6)
	move.w  #%0010000000001010,$58(a6)
blit6:	btst.b	#$e,$02(a6)
	bne.s	Blit6
	rts

Klatka1:	dc.l	klatka
Klatka2:	dc.l	klatka+2560
Klatka3:	dc.l	klatka+[2*2560]
Klatka4:	dc.l	klatka+[3*2560]
Klatka5:	dc.l	klatka+[4*2560]
Klatka6:	dc.l	klatka+[5*2560]
Place1:		dc.l	$70000+[63*40]
Place2:		dc.l	$72800+[63*40]
Place3:		dc.l	$75000+[63*40]
Place4:		dc.l	$77800+[63*40]
Place5:		dc.l	$7a000+[63*40]
Place6:		dc.l	$7c800+[63*40]
;*------------------------------------------------------------------
; scrolling up ....

Vscroll:btst	#10,$dff016
	beq	tup
	subi	#1,zmiana
	bne.s	rusz
	move	#2,zmiana
here:	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d6
	moveq	#19,d7
	move.l	textpointer,a0
	move.l	#$70000+[[216*40]+20],a2
Pline:	lea	font,a1
	move.b	(a0)+,d0
	tst.b	d0
	beq	Hend2
Dalej:	subi.b	#32,d0
	lsl.w	#3,d0
	moveq	#7,d6
letter:	move.b	0(a1,d0.w),0(a2,d1.w)
	addi.w	#40,d1
	addq.w	#1,d0
	dbf	d6,letter
	adda.l	#1,a2
	moveq	#0,d1
	moveq	#0,d0
	dbf	d7,PLine
	move.l	a0,textpointer

rusz:	lea	$dff000,a6
blit:	btst.b	#$e,$02(a6)
	bne.s	Blit
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00140014,$64(a6)
	move.l  #$70000+[56*40]+[40*4]+20,$50(a6)
	move.l  #$70000+[56*40]+20,$54(a6)
	move.w  #$2c0a,$58(a6)			;180
tup:	rts
hend2:	move	#-1,hendvar
	rts

hendvar:	dc.w	0
zmiana:		dc.w	2
;*------------------------------------------------------------------
; data section.

TextPointer:	dc.l	text


Copper:	dc.w $0182,$0000
scron:
	dc.w $0100,$0300
	dc.w $0120,$0000,$0122,$0000
	dc.w $0124,$0000,$0126,$0000
	dc.w $0128,$0000,$012a,$0000
	dc.w $012c,$0000,$012e,$0000
	dc.w $0130,$0000,$0132,$0000
	dc.w $0134,$0000,$0136,$0000
	dc.w $0138,$0000,$013a,$0000
	dc.w $013c,$0000,$013e,$0000
	dc.w $008e,$2c81,$0090,$29c1
	dc.w $0092,$0038,$0094,$00d0
	dc.w $0108,$0000,$010a,$0000
	dc.w $0102,$0000,$0104,$0000
	dc.w $00e0,$0007,$00e2,$0000
	dc.w $00e4,$0007,$00e6,$2800
	dc.w $00e8,$0007,$00ea,$5000
	dc.w $00ec,$0007,$00ee,$7800
	dc.w $00f0,$0007,$00f2,$a000
	dc.w $00f4,$0007,$00f6,$c800
scrup:
	dc.w $6407,$fffe,$0180,$0006
	dc.w $6407,$fffe,$0180,$0000
Kolors:	dc.w $0180,$0000,$0182,$0000
	dc.w $0184,$0000,$0186,$0000
	dc.w $0188,$0000,$018a,$0000
	dc.w $018c,$0000,$018e,$0000
	dc.w $0190,$0000,$0192,$0000
	dc.w $0194,$0000,$0196,$0000
	dc.w $0198,$0000,$019a,$0000
	dc.w $019c,$0000,$019e,$0000
	dc.w $01a0,$0000,$01a2,$0000
	dc.w $01a4,$0000,$01a6,$0000
	dc.w $01a8,$0000,$01aa,$0000
	dc.w $01ac,$0000,$01ae,$0000
	dc.w $01b0,$0000,$01b2,$0000
	dc.w $01b4,$0000,$01b6,$0000
	dc.w $01b8,$0000,$01ba,$0000
	dc.w $01bc,$0000,$01be,$0000

scrdn:
	dc.w $fa07,$fffe,$0180,$0006,$182,0
	dc.w $fa07,$fffe,$0180,$0000
	dc.w $ffff,$fffe

Font:
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;" "
dc.b	$30,$30,$30,$30,$20,$00,$20,$00	;"!"
dc.b	$CC,$44,$88,$00,$00,$00,$00,$00	;"""
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"#"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"$"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"%"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"&"
dc.b	$30,$10,$20,$00,$00,$00,$00,$00	;"'"
dc.b	$18,$30,$30,$30,$30,$30,$18,$00	;"("
dc.b	$30,$18,$18,$18,$18,$18,$30,$00	;")"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"*"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"+"
dc.b	$00,$00,$00,$00,$00,$30,$10,$20	;","
dc.b	$00,$00,$00,$FC,$00,$00,$00,$00	;"-"
dc.b	$00,$00,$00,$00,$00,$30,$30,$00	;"."
dc.b	$04,$0C,$18,$30,$60,$C0,$80,$00	;"/"
dc.b	$58,$CC,$CC,$CC,$CC,$CC,$58,$00	;"0"
dc.b	$70,$30,$30,$30,$30,$30,$78,$00	;"1"
dc.b	$58,$CC,$0C,$18,$60,$0C,$FC,$00	;"2"
dc.b	$68,$CC,$0C,$28,$0C,$CC,$68,$00	;"3"
dc.b	$0C,$2C,$4C,$8C,$FE,$0C,$1E,$00	;"4"
dc.b	$FC,$30,$80,$D8,$0C,$CC,$D8,$00	;"5"
dc.b	$58,$CC,$C0,$D8,$CC,$CC,$58,$00	;"6"
dc.b	$FC,$C0,$08,$18,$30,$30,$30,$00	;"7"
dc.b	$58,$CC,$CC,$58,$CC,$CC,$58,$00	;"8"
dc.b	$58,$CC,$CC,$5C,$0C,$CC,$58,$00	;"9"
dc.b	$00,$00,$30,$00,$00,$30,$00,$00	;":"
dc.b	$00,$00,$30,$00,$00,$30,$10,$20	;";"
dc.b	$0C,$18,$30,$60,$30,$18,$0C,$00	;"<"
dc.b	$00,$00,$FC,$00,$FC,$00,$00,$00	;"="
dc.b	$60,$30,$18,$0C,$18,$30,$60,$00	;">"
dc.b	$6C,$C6,$06,$0C,$18,$00,$18,$00	;"?"
dc.b	$00,$00,$00,$00,$00,$00,$00,$00	;"@"
dc.b	$0C,$0C,$2C,$26,$5E,$46,$EE,$00	;"A"
dc.b	$EC,$66,$66,$6C,$66,$66,$EC,$00	;"B"
dc.b	$2E,$62,$E0,$E0,$E0,$62,$2E,$00	;"C"
dc.b	$E8,$6C,$66,$66,$66,$6C,$E8,$00	;"D"
dc.b	$EE,$66,$60,$6C,$60,$66,$EE,$00	;"E"
dc.b	$EE,$66,$60,$6C,$60,$60,$F0,$00	;"F"
dc.b	$2E,$66,$E0,$EE,$E6,$66,$2C,$00	;"G"
dc.b	$F6,$66,$66,$6E,$66,$66,$F6,$00	;"H"
dc.b	$78,$30,$30,$30,$30,$30,$78,$00	;"I"
dc.b	$3C,$18,$18,$18,$D8,$D8,$70,$00	;"J"
dc.b	$F6,$64,$68,$6C,$6E,$66,$F6,$00	;"K"
dc.b	$F0,$60,$60,$60,$62,$66,$FE,$00	;"L"
dc.b	$C6,$EE,$7E,$B6,$86,$86,$CE,$00	;"M"
dc.b	$66,$72,$3A,$5E,$4E,$46,$E6,$00	;"N"
dc.b	$58,$CC,$CC,$CC,$CC,$CC,$58,$00	;"O"
dc.b	$EC,$66,$66,$6C,$60,$60,$F0,$00	;"P"
dc.b	$58,$CC,$CC,$CC,$CC,$58,$1C,$00	;"Q"
dc.b	$EC,$66,$66,$6C,$68,$64,$F6,$00	;"R"
dc.b	$5C,$CC,$C0,$78,$0C,$CC,$D8,$00	;"S"
dc.b	$FC,$B4,$30,$30,$30,$30,$30,$00	;"T"
dc.b	$F6,$62,$62,$62,$62,$62,$2C,$00	;"U"
dc.b	$F6,$62,$62,$62,$22,$3C,$18,$00	;"V"
dc.b	$E6,$C2,$C2,$D2,$DA,$EC,$C6,$00	;"W"
dc.b	$CE,$CC,$68,$30,$58,$4C,$CE,$00	;"X"
dc.b	$E6,$62,$34,$18,$18,$18,$3C,$00	;"Y"
dc.b	$EC,$8C,$18,$30,$60,$CC,$DC,$00	;"Z"
dc.b	$38,$30,$30,$30,$30,$30,$38,$00	;"["
dc.b	$80,$C0,$60,$30,$18,$0C,$04,$00	;"\"
dc.b	$38,$18,$18,$18,$18,$18,$38,$00	;"]"
dc.b	$10,$38,$6C,$00,$00,$00,$00,$00	;"^"
dc.b	$00,$00,$00,$00,$00,$00,$00,$FF	;"_"
dc.b	$30,$20,$10,$00,$00,$00,$00,$00	;"`"


;		 12345678901234567890
Text:	dc.b	"                    "
	dc.b	"SUSPECT SENDS GREETS"
	dc.b	"                    "
	dc.b	"  TO THE FOLLOWING: "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"       *****        "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	DC.B	"   ACTION DIRECT    "
	dc.b	"                    "
	dc.b	"        ACME        "
	dc.b	"                    "
	DC.B	"      ALCHEMY       "
	dc.b	"                    "
	dc.b	"    ARCHITECTURE    "
	dc.b	"                    "
	dc.b	"      ARCHOUS       "
	dc.b	"                    "
	dc.b	"     BETA TEAM      "
	dc.b	"                    "
	dc.b	"      BRAINIAX      "
	dc.b	"                    "
	dc.b	"      CASCADE       "
	dc.b	"                    "
;	dc.b	"  23 CELSIUS CREW   "
;	dc.b	"                    "
	dc.b	"      DEFORM        "
	dc.b	"                    "
	dc.b	"      DIOXIDE       "
	dc.b	"                    "
	dc.b	"     DRUNKARDS      "
	dc.b	"                    "
	dc.b	"      ENERGY        "
	dc.b	"                    "
	dc.b	"     FAIRLIGHT      "
	dc.b	"                    "
;	dc.b	"      FERROX        "
;	dc.b	"                    "
	dc.b	"    FLUFFY BEARS    "
	dc.b	"                    "
	dc.b	" FUTURE REVOLUTION  "
	dc.b	"                    "
	dc.b	"       GHOST        "
	dc.b	"                    "
	dc.b	"       GRACE        "
	dc.b	"                    "
;	dc.b	"     GRAFFITI       "
;	dc.b	"                    "
	dc.b	"       JOKER        "
	dc.b	"                    "
	dc.b	"     KATHARSIS      "
	dc.b	"                    "
	dc.b	"       LUZERS       "
	dc.b	"                    "
;	dc.b	"   LUNORIC EARLS    "
;	dc.b	"                    "
	dc.b	"     OLD BULLS      "
	dc.b	"                    "
	dc.b	"   PEPSI DRINKERS   "
	dc.b	"                    "
	dc.b	"   PIC SAINT LOUP   "
	dc.b	"                    "
	dc.b	"       PROXIS       "
	dc.b	"                    "
	dc.b	"       QUARTZ       "
	dc.b	"                    "
	dc.b	"  REAL DESTRUCTION  "
	dc.b	"                    "
	dc.b	"       SANITY       "
	dc.b	"                    "
	dc.b	"       STAGE        "
	dc.b	"                    "
;	dc.b	"     STAR TREK      "
;	dc.b	"                    "
	dc.b	"       TETLOX       "
	dc.b	"                    "
;	dc.b	"      WARRIORS      "
;	dc.b	"                    "
	dc.b	"     WILD LIFE      "
	dc.b	"                    "
	dc.b	"      W.F.M.H.      "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"       *****        "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	"                    "
	dc.b	0,0

;*------------------------------------------------------------------
przerwa1=50			;prerwa miedzy napisami
przerwa2=10			;ile trwa napis

;-------------------------------------------------------------------
;poczatek ,a moze i ... koniec ???

; UWAGA ! KANE ! hu,hu,hu.... tutaj .... no nie spij .....

; dobra - skoro ty to skladasz to musisz to dopasowac do 
; muzyczki . mozesz wyregulowac trwanie wszystkich elementow
; (opisanych mniejwiecej) przez zmiane wrzucanej wartosci do
; mk_con0 przed kazdym elementem (mk_con0 to liczba ramek
; w czasie ktorych to bedzie wykonywane ).
;					podpisano Pillar/SCT'92
; OK !!! No problem...
;					podpisano xxx/XXX (Kane?)

timehascome:
mk_Init:move.w	#40,mk_Width
	move.w	#$ffff,mk_Texture
	move.l	mk_Plane1,a0
	moveq	#0,d0
	move.w	#%1010000000101000,d0
	bsr.L	mk_Clear

	lea	Zegar,a0
	move.l	mk_plane1,a1
	adda.l	#[64*40]+12,a1
	moveq	#0,d0
	move.w	#22,d1
	move.w	#%0010000010001001,d2
	bsr	mk_copy

	lea	Zegar+2340,a0
	move.l	mk_plane2,a1
	adda.l	#[64*40]+12,a1
	moveq	#0,d0
	move.w	#22,d1
	move.w	#%0010000010001001,d2
	bsr	mk_copy

	bsr.L	mk_Raster
	move.l	#mk_copper,$dff080
	clr.w	$dff088

		move	#15,d0
mk_setcol:	lea	mk_coltab,a2
		lea	mk_copper+4,a1
		move	#14,d3				;ilosc kolorow
mk_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq	mk_scol2
		addi	#1,2(a1)
mk_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq	mk_scol3
		addi	#$10,2(a1)
mk_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq	mk_scol4
		addi	#$100,2(a1)
mk_scol4:	adda	#4,a1
		dbf	d3,mk_scol1
		bsr	mk_raster
		bsr	mk_raster
		dbf	d0,mk_setcol


	move.l	#przerwa1,mk_con0		; o tutaj przykladowo
mk_dal0:moveq	#0,d0			; element bedzie trwal
	moveq	#0,d1			; 30 ramek.
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	subi.w	#6,mk_wsk1
	tst.w	mk_wsk1
	bhi.s	mk_dal1

	move.w	#360,mk_wsk1
mk_dal1:subq.w	#2,mk_wsk2
	tst.w	mk_wsk2
	bhi.s	mk_dal2
	move.w	#360,mk_wsk2
mk_dal2:cmpi.b	#$fe,$dff006
	bne.s	mk_dal2
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal0		;tutaj jest petla i koniec elementu

;	wchodzi napis 'time' , wskazowki sie kreca

	lea	time,a0
	move.l	mk_plane4,a1
	adda.l	#[57*40]+2,a1
	moveq	#0,d0
	moveq	#4,d1
	move.w	#%0010001110010010,d2
	bsr	mk_copy

	move.l	#przerwa2,mk_con0
mk_dal3:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	subi.w	#6,mk_wsk1
	tst.w	mk_wsk1
	bhi.s	mk_dal4
	move.w	#360,mk_wsk1
mk_dal4:subq.w	#2,mk_wsk2
	tst.w	mk_wsk2
	bhi.s	mk_dal5
	move.w	#360,mk_wsk2
mk_dal5:cmpi.b	#$fe,$dff006
	bne.L	mk_dal5
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal3

; znika 'time' i inaczej sie obracaja wskazowki.

	clr.w	mk_wsk1
	clr.w	mk_wsk2

	move.l	mk_plane4,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear

	move.l	#przerwa1,mk_con0
mk_dal6:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	addi.w	#6,mk_wsk1
	cmpi.w	#360,mk_wsk1
	ble.s	mk_dal7
	move.w	#1,mk_wsk1
mk_dal7:addq.w	#2,mk_wsk2
	cmpi.w	#360,mk_wsk2
	ble.s	mk_dal8
	move.w	#1,mk_wsk2
mk_dal8:cmpi.b	#$fe,$dff006
	bne.L	mk_dal8
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal6

;	wchodzi napis 'has' i kreca sie wskazowki

	lea	has,a0
	move.l	mk_plane4,a1
	adda.l	#[75*40]+9,a1
	moveq	#0,d0
	moveq	#18,d1
	move.w	#%0001101001001011,d2
	bsr	mk_copy


	move.l	#przerwa2,mk_con0
mk_dal9:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	addi.w	#6,mk_wsk1
	cmpi.w	#360,mk_wsk1
	ble.s	mk_dal10
	move.w	#1,mk_wsk1
mk_dal10:addq.w	#2,mk_wsk2
	cmpi.w	#360,mk_wsk2
	ble.s	mk_dal11
	move.w	#1,mk_wsk2
mk_dal11:cmpi.b	#$fe,$dff006
	bne.L	mk_dal11
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal9

;  znika napis 'has' kreca sie wskazowki w rozne strony

	move.l	mk_plane4,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear

	clr.w	mk_wsk1
	move.w	#360,mk_wsk2

	move.l	#przerwa1,mk_con0
mk_dal12:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	addq.w	#6,mk_wsk1
	cmpi.w	#360,mk_wsk1
	ble.s	mk_dal13
	move.w	#1,mk_wsk1
mk_dal13:subq.w	#2,mk_wsk2
	tst.w	mk_wsk2
	bhi.s	mk_dal14
	move.w	#360,mk_wsk2
mk_dal14:cmpi.b	#$fe,$dff006
	bne.L	mk_dal14
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal12

;  wszedl napis 'come' i wskazowki kreca sie jak poprzednio

	lea	come,a0
	move.l	mk_plane4,a1
	adda.l	#[70*40]+3,a1
	moveq	#0,d0
	moveq	#6,d1
	move.w	#%0001110100010001,d2
	bsr	mk_copy

	move.l	#przerwa2,mk_con0
mk_dal15:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	addq.w	#6,mk_wsk1
	cmpi.w	#360,mk_wsk1
	ble.s	mk_dal16
	move.w	#1,mk_wsk1
mk_dal16:subq.w	#2,mk_wsk2
	tst.w	mk_wsk2
	bhi.s	mk_dal17
	move.w	#360,mk_wsk2
mk_dal17:cmpi.b	#$fe,$dff006
	bne.L	mk_dal17
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal15

; wygas napis 'come' popracuj troche i fajrant, wskazowki bez zmian 

	move.l	mk_plane4,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear

	move.l	#przerwa1,mk_con0
mk_dal18:moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p1,d0
	move.w	mk_p1+2,d1
	move.w	mk_wsk1,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk1,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	move.w	mk_p2,d0
	move.w	mk_p2+2,d1
	move.w	mk_wsk2,d5
	bsr	mk_sinx
	move.l	d5,d6
	move.w	mk_wsk2,d5
	bsr	mk_cosx		
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
	addi.w	#166,d0
	addi.w	#128,d1
	move.w	#166,d2
	move.w	#128,d3
	move.l	mk_plane3,a0
	bsr	mk_line

	lea	mk_blank,a0
	movem.l	(a0)+,d0-d7

	addq.w	#6,mk_wsk1
	cmpi.w	#360,mk_wsk1
	ble.s	mk_dal19
	move.w	#1,mk_wsk1
mk_dal19:subq.w	#2,mk_wsk2
	tst.w	mk_wsk2
	bhi.s	mk_dal20
	move.w	#360,mk_wsk2
mk_dal20:cmpi.b	#$fe,$dff006
	bne.L	mk_dal20
	move.l	mk_plane3,a0
	move.w	#%0100000000010100,d0
	bsr	mk_clear
	subq.l	#1,mk_con0
	tst.l	mk_con0
	bne.L	mk_dal18

	moveq	#7,d0
mk_fadcol:lea	mk_copper,a1
	moveq	#15,d3
mk_fad1:move.w	2(a1),d1
	andi.w	#$f,d1
	beq.s	mk_fad2
	subq.w	#1,2(a1)
mk_fad2:move.w	2(a1),d1
	andi.w	#$f0,d1
	beq.s	mk_fad3
	subi.w	#$10,2(a1)
mk_fad3:move.w	2(a1),d1
	andi.w	#$f00,d1
	beq.s	mk_fad4
	subi.w	#$100,2(a1)
mk_fad4:adda.l	#4,a1
	dbf	d3,mk_fad1
	move.w	#$1c00,d3
mk_wait5:muls	d1,d1
	dbf	d3,mk_wait5
	dbf	d0,mk_fadcol
	rts


mk_cosx:addi.w	#90,d5
	cmpi.w	#360,d5
	blt.s	mk_sinx
	subi.w	#360,d5
mk_sinx:lea	mk_sin_tab,a5
	lsl.w	#1,d5
	move.w	0(a5,d5.w),d5
	rts


mk_con0:	dc.l	0
;-------------------------------------------------------------------

mk_raster:
	cmpi.b	#$fe,$dff006
	bne.s	mk_raster
	cmpi.b	#$ff,$dff006
	bne.s	mk_raster+10
	rts

;-------------------------------------------------------------------
; WAIT UNTIL BLITTER FINISH .

mk_WBlt:btst.b	#$e,$dff002
	bne.s	mk_WBlt
	rts

;-------------------------------------------------------------------

; Bliter copy routine !!!
; d0 - source modulo
; d1 - destination modulo
; d2 - blit size
; a0 - source address
; a1 - destination address

mk_Copy:bsr	mk_wblt
	lea	$dff000,a6
	move.w  #$09f0,$40(a6)	;Bltcon0
	move.w  #$0000,$42(a6)		;Bltcon1
	move.w  #$ffff,$44(a6)	;Mask read all
	move.w  #$ffff,$46(a6)	;Mask write all
	move.w	d0,$64(a6)	;Source		mod
	move.w  d1,$66(a6)	;Destination	mod
	move.l  a0,$50(a6)	;Source		A
	move.l  a1,$54(a6)	;Destination	D
	move.w  d2,$58(a6)
	rts

;-------------------------------------------------------------------
;	****************************************************
;	*	    .. DRAW LINE ROUTINE ..		   *
;	* -- Some parameters for subroutine:		   *
;	*	d0 - X1		;	d1 - Y1		   *
;	*	d2 - X2		;	d3 - Y2		   *
;	*	a0 - Bitplane Pointer			   *
;	****************************************************

mk_Line:movem.l	d0-d7/a0-a6,-(a7)
	lea	$dff000,a3
	suba.l	a1,a1
	move.w	mk_Width,a1
	move.l	a1,d4
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
	bge.s	mk_y2gy1
	neg.w	d3

mk_y2gy1:
	subi.w	d0,d2
	roxl.b	#1,d5
	tst.w	d2
	bge.s	mk_x2gx1
	neg.w	d2

mk_x2gx1:
	move.w	d3,d1
	subi.w	d2,d1
	bge.s	mk_dygdx
	exg	d2,d3

mk_dygdx:
	roxl.b	#1,d5
	move.b	mk_Octant_table(pc,d5),d5
	addi.w	d2,d2

mk_Wblit:
	btst.b	#$e,$02(a3)
	bne.s	mk_WBlit

	move.w	d2,$62(a3)	;BltBMod
	subi.w	d3,d2
	bge.s	mk_signn1

	ori.b	#$40,d5
mk_signn1:
	move.w	d2,$52(a3)	;BltAPtl

	subi.w	d3,d2
	move.w	d2,$64(a3)	;BltAMod

	move.w	mk_Texture,$72(a3)
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

; EXTREMLY NEEDED FOR THIS SUBROUTINE :

mk_Octant_table:dc.b	0*4+1
		dc.b	4*4+1
		dc.b	2*4+1
		dc.b	5*4+1
		dc.b	1*4+1
		dc.b	6*4+1
		dc.b	3*4+1
		dc.b	7*4+1

mk_Texture:	dc.w	0
mk_Width:	dc.w	0
;-------------------------------------------------------------------

; Blitter Clear !!!
; d0 - blit size
; a0 - start adress of area to clear

mk_Clear:
	bsr.L	mk_WBLT
	lea	$dff000,a6
	move.l 	#$01000000,$40(a6)	;Bltcon0 & Bltcon1
	move.l 	a0,$48(a6)		;Source
	move.l	a0,$54(a6)		;Destination
	clr.w	$66(a6)			;Destination Mod
	move.w 	d0,$58(a6)		;BlitSize
	rts

;-------------------------------------------------------------------
; SOME DATAS .

mk_Blank:		blk.l	8,0
mk_Plane1:		dc.l	$70000
mk_plane2:		dc.l	$72800
mk_plane3:		dc.l	$75000
mk_plane4:		dc.l	$77800

mk_wsk1:	dc.w	360
mk_wsk2:	dc.w	360
mk_coltab:	dc.w	     $440,$880,$777,$00f,$00e,$00d,$00c
		dc.w	$0f0,$0c0,$0e0,$0d0,$0f0,$0f0,$0f0,$0f0

mk_Copper:
	dc.w $0180,$0000,$0182,$0000
	dc.w $0184,$0000,$0186,$0000
	dc.w $0188,$0000,$018a,$0000
	dc.w $018c,$0000,$018e,$0000

	dc.w $0190,$0000,$0192,$0000
	dc.w $0194,$0000,$0196,$0000
	dc.w $0198,$0000,$019a,$0000
	dc.w $019c,$0000,$019e,$0000

	dc.w $0102,$0000,$0104,$00aa
	dc.w $0108,$0000,$010a,$0000
	dc.w $00e0,$0007,$00e2,$0000
	dc.w $00e4,$0007,$00e6,$2800
	dc.w $00e8,$0007,$00ea,$5000
	dc.w $00ec,$0007,$00ee,$7800
	dc.w $008e,$2981,$0090,$29c1
	dc.w $0092,$0038,$0094,$00d0
	dc.w $0100,$4300
	dc.w $0120,$0000,$0122,$0000
	dc.w $0124,$0000,$0126,$0000
	dc.w $0128,$0000,$012a,$0000
	dc.w $012c,$0000,$012e,$0000
	dc.w $0130,$0000,$0132,$0000
	dc.w $0134,$0000,$0136,$0000
	dc.w $0138,$0000,$013a,$0000
	dc.w $013c,$0000,$013e,$0000
	dc.w $ffff,$fffe

;---------------------------------------------------

mk_sin_tab:
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

;---------------------------------------------------

mk_p1:	dc.w	0,60
mk_p2:	dc.w	0,30

;---------------------------------------------------
loadit:

zegar:		equ	loadit
time:		equ	loadit+4680
has:		equ	loadit+4680+5112
come:		equ	loadit+4680+5112+2310	;3944
Klatka:		equ	come+3944		;15424

>extern	"df0:.store/zegar.pic",zegar,-1
>extern	"df0:.store/time.pic",time,-1
>extern	"df0:.store/has.pic",has,-1
>extern	"df0:.store/come.pic",come,-1
>extern	"df0:.store/abstrakcja.raw",klatka,-1

end=klatka+15424

