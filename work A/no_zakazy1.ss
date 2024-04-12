
;		*****************************************
;		*	  NO... NO... NO... !!!		*
;		*    ------------------------------	*
;		*  	  Coding on 28.10.1992		*
;		*	  by  KANE  of SUSPECT		*
;		*****************************************


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


org $30000
load $30000

s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
;	move	#$7fff,$dff09a
;	move	#$7fff,$dff096
	move	#$83c0,$dff096

	bsr	no_bigwords

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	move	#$7fff,$dff096
	move	#$7fff,$dff09a
	move	olddma,$dff096
	move	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;---------------------------------------------------------------------
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
;---------------------------------------------------------------------

no_bigwords:
		lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#no_plane,$54(a0)
		move	#[2*no_heith*64]+[no_row/2],$58(a0)
		bsr	no_makecop
		waitblt
		raster
		move.l	#no_copper0,$dff080

no_control:	raster
		btst.b	#$6,$bfe001
		bne.s	no_control
		rts

;---------------------------------------------------------------------
no_makecop:	lea	no_copper0,a1
		move.l	#$1000300,(a1)+
		move.l	#$2007fffe,d0
		move.l	#$20a7fffe,d2
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
no_plane=$70000
no_heith=55
no_row=44+58		;102
;---------------------------------------------------------------------
no_copper0=$60000

no_copper:
dc.l	$1820975,$1840531,$1860eca
dc.l	$1800100
dc.l	$920030,$9400d8,$8e0171,$9037d1,$1020000
dc.l	$108003a,$10a003a
no_screen:
dc.l	$e00000+[no_plane/$10000],$e20000+[no_plane&$ffff]
dc.l	$e40000+[no_plane/$10000],$e60000+[no_plane&$ffff]
dc.l	$7001ff00,$1002300
dc.l	$ffdffffe
dc.l	$a701ff00,$1000300
dc.l	$fffffffe
;---------------------------------------------------------------------
no_sex=$50000
no_beer=no_sex+2750
no_life=no_sex+2750+3190		;2860

>extern	"df0:.store/nosex.raw",no_sex,-1
>extern	"df0:.store/nobeer.raw",no_beer,-1
>extern	"df0:.store/nolife.raw",no_life,-1

end:

