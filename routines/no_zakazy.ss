
;		*****************************************
;		*	  NO... NO... NO... !!!		*
;		*    ------------------------------	*
;		*  	  Coding on 30.10.1992		*
;		*	  by  KANE  of SUSPECT		*
;		*****************************************

waitblt:macro
	btst.b	#14,$2(a0)
	bne.s	*-8
	endm
raster: macro
	cmp.b	#$fe,$dff006
	bne.s	*-10
	cmp.b	#$ff,$dff006
	bne.s	*-10
	endm


org $30000
load $30000

s:	movem.l	a0-a6/d0-d7,-(sp)
	move	#$4000,$dff09a
	move	#$20,$dff096

	bsr	no_bigwords

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$8020,$dff096
	move	#$c000,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;---------------------------------------------------------------------
gfxname:	dc.b	'graphics.library',0,0
;---------------------------------------------------------------------
no_bigwords:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#no_plane,$54(a0)
		move	#[2*[no_heith+5]*64]+[no_row/2],$58(a0)
		bsr	no_makecop
		waitblt
		raster
		move.l	#no_copper0,$dff080

no_control0:	raster
		move	#275,d7
		lea	no_copper0+4,a1
no_con0_0:	addi.b	#16,9(a1)
		lea	16(a1),a1
		cmpi	#$ffdf,(a1)
		bne.s	no_con0_1
		lea	4(a1),a1
no_con0_1:	dbf	d7,no_con0_0
		cmpi.b	#$cf,-32+9(a1)
		bne.s	no_control0
		raster
		move.l	#no_copper,$dff080
		btst.b	#2,$dff016
		beq.L	no_not

		move	#20,d7
no_wait1:	raster
		dbf	d7,no_wait1
		lea	no_sex,a1		;sex
		lea	no_plane+48,a2
		move	#109,d7
no_cop1:	move	#24,d6
no_cop1_1:	move	(a1)+,(a2)+
		dbf	d6,no_cop1_1
		lea	102-50(a2),a2
		dbf	d7,no_cop1
		lea	no_screen,a1
		move	#50,d7
no_control1:	raster
		addi	#2,6(a1)
		addi	#2,14(a1)
		dbf	d7,no_control1
		move.b	#$c0,no_ustaw
		move.b	#$f7,no_ustaw+8

		move	#$49,no_copper+2
		move	#$12,no_copper+6
		move	#$5e,no_copper+10
		move	#15,d7
no_wait2:	raster
		dbf	d7,no_wait2
		lea	no_beer,a1		;beer
		lea	no_plane+44,a2
		move	#109,d7
no_cop2:	move	#28,d6
no_cop2_1:	move	(a1)+,(a2)+
		dbf	d6,no_cop2_1
		lea	102-58(a2),a2
		dbf	d7,no_cop2
		lea	no_screen,a1
		move	#50,d7
no_control2:	raster
		subi	#2,6(a1)
		subi	#2,14(a1)
		dbf	d7,no_control2
		move.b	#$80,no_ustaw
		move.b	#$b7,no_ustaw+8

		move	#$b0,no_copper+2
		move	#$50,no_copper+6
		move	#$f0,no_copper+10
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#no_plane,$54(a0)
		move	#[2*[no_heith+5]*64]+[no_row/2],$58(a0)
		move	#15,d7
no_wait3:	raster
		dbf	d7,no_wait3
		waitblt
		lea	no_life,a1		;life
		lea	no_plane+46,a2
		move	#109,d7
no_cop3:	move	#25,d6
no_cop3_1:	move	(a1)+,(a2)+
		dbf	d6,no_cop3_1
		lea	102-52(a2),a2
		dbf	d7,no_cop3
		lea	no_screen,a1
		move	#50,d7
no_control3:	raster
		addi	#2,6(a1)
		addi	#2,14(a1)
		dbf	d7,no_control3

no_not:		move	#10,d7
no_wait9:	raster
		dbf	d7,no_wait9
		raster
		move.l	#no_copper0,$dff080
no_control9:	raster
		move	#275,d7
		lea	no_copper0+4,a1
no_con9_0:	subi.b	#16,9(a1)
		lea	16(a1),a1
		cmpi	#$ffdf,(a1)
		bne.s	no_con9_1
		lea	4(a1),a1
no_con9_1:	dbf	d7,no_con9_0
		cmpi.b	#$f,-32+9(a1)
		bne.s	no_control9
		rts

;---------------------------------------------------------------------
no_makecop:	lea	no_copper0,a1
		move.l	#$1000300,(a1)+
		move.l	#$2007fffe,d0
		move.l	#$200ffffe,d2
		move	#275,d7
no_initloop:	addi.l	#$1000000,d0
		addi.l	#$1000000,d2
		move.l	d0,d1
		andi.l	#$ff000000,d1
		bne.s	no_inok
		move.l	#$ffdffffe,(a1)+
no_inok:	move.l	d0,(a1)+
		move.l	#$1800100,(a1)+
		move.l	d2,(a1)+
		move.l	#$1800000,(a1)+
		dbf	d7,no_initloop
		move.l	#-2,(a1)
		rts
;---------------------------------------------------------------------
no_plane=$71000			;nie zmieniac!!!
no_heith=55
no_row=44+58		;102
;---------------------------------------------------------------------
no_copper0=$6a000

no_copper:
dc.l	$1820a0a,$1840303,$1860f0f
dc.l	$1800100
dc.l	$920030,$9400d8,$8e0171,$9037d1,$1020000
dc.l	$108003a,$10a003a,$1020000
no_screen:
dc.l	$e00000+[no_plane/$10000],$e20000+[no_plane&$ffff]
dc.l	$e40000+[[no_plane+[no_heith*no_row]]/$10000],$e60000+[[no_plane+[no_heith*no_row]]&$ffff]
no_ustaw:
dc.l	$5001ff00,$1002300
dc.l	$8701ff00,$1000300
dc.l	$fffffffe
;---------------------------------------------------------------------
no_sex=$50000
no_beer=no_sex+5500
no_life=no_sex+5500+6380		;5720

>extern	"df0:.store/nosex.raw",no_sex,-1
>extern	"df0:.store/nobeer.raw",no_beer,-1
>extern	"df0:.store/nolife.raw",no_life,-1

end:

