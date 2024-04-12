;		*****************************************
;		*	    GRAPHICS ZOOM OUT		*
;		*  ----------------------------------	*
;		*	   Coded on xx.09.1992		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************

org	$30000
load	$30000

waitblt:macro
wait?0:	btst.b	#14,$dff002
	bne.s	wait?0
	endm
raster2: macro
wait?0:	cmp.b	#$e8,$dff006
	bne.s	wait?0
	cmp.b	#$e9,$dff006
	bne.s	wait?0+10
	endm

s:	movem.l	a0-a6/d0-d7,-(sp)
	move	$dff002,olddma
	ori	#$8000,olddma
	move	$dff01c,oldint
	ori	#$8000,oldint
	move	#$7fff,$dff09a
	move	#$7fff,$dff096
;move	#$20,$dff096
	move	#$87c0,$dff096

	bsr	zoom_zoom

	move.l	4,a6
	lea	gfxname,a1
	moveq	#0,d0
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$32(a0),$dff080
	move	olddma,$dff096
	move	oldint,$dff09a
	movem.l	(sp)+,a0-a6/d0-d7
	rts

;----------------------------------------------------------------------
zoom_zoom:	lea	$dff000,a0
		move.l	#$ffffffff,$44(a0)
		waitblt
		clr	$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#zoom_plane,$54(a0)
		move	#0,$58(a0)
		move.l	#zoom_plane,scroff
		move.l	#zoom_plane+[3*258*44],scron
		waitblt
		lea	zoom_load,a1
		lea	zoom_plane+[zoom_offset*3*zoom_row],a2
		move	#[3*zoom_heigth]-1,d7
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
		move	#10,l_counter
zoom_control1:	raster2
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
		lea	zoom_load+19200,a2
		move	#7,d3
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
		move	#7,d3
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
zoom_out:	waitblt
		move.l	scron,d0
		move.l	scroff,d1
		move.l	d0,scroff
		move.l	d1,scron
		move.l	d1,d3
		lea	zoom_screen,a1
		move	#2,d2
zoom_chgl:	move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#[zoom_row],d1
		addq.l	#8,a1
		dbf	d2,zoom_chgl
		clr.l	$64(a0)
		move.l	#$9f00000,$40(a0)
		addi.l	#[zoom_offset*3*zoom_row],d3
		move.l	d3,$50(a0)
		addi.l	#[zoom_offset*3*zoom_row],d0
		move.l	d0,$54(a0)
		move	#[3*zoom_heigth*64]+[zoom_row/2],$58(a0)
		move.l	d0,d7

;----------------------------------------------------------------------
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
		move.l	#zoom_row,d2
		subi	d1,d2			;modulo
		neg	d1
		addi.l	#3*zoom_heigth*64,d0	;blit size

		waitblt
		move	d2,$60(a0)		;c
		move	d2,$64(a0)		;a
		move	d2,$66(a0)		;d
		move	d1,$62(a0)		;b
		move.l	#$ffe20000,$40(a0)
		move.l	d7,$50(a0) ;a
		move.l	#zoom_maska,$4c(a0)	;b
		subi.l	#2,d7
		move.l	d7,$48(a0) ;c
		move.l	d7,$54(a0) ;d
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
;		move.l	#zoom_plane+[zoom_offset*3*zoom_row]-4,d6
		subi.l	#2,d7
		addi.l	d2,d7
		move.l	#$1fe20000,$40(a0)
		move.l	d7,$50(a0)			;a
		move.l	#zoom_maska,$4c(a0)		;b
		move.l	d7,$48(a0)			;c
		move.l	d7,$54(a0)			;d
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
		addi	#[3*zoom_row],2(a1)
		addi	#[3*zoom_row],6(a1)
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
zoom_nodouble:	move	#[2*zoom_row]+4,6(a1)
		move	#[2*zoom_row]+4,10(a1)
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
		move.l	#$1080000+[2*zoom_row]+4,(a1)+
		move.l	#$10a0000+[2*zoom_row]+4,(a1)+
		dbf	d7,zoom_initloop
		rts
		
;----------------------------------------------------------------------
zoom_plane=$60000		;musi byc!!!
zoom_offset=48
zoom_heigth=160
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
dc.l	$920038,$9400d0,$8e0171,$9037d1
zoom_screen:
dc.l	$e00000+[zoom_plane/$10000],$e20000+[zoom_plane&$ffff]
dc.l	$e40000+[[zoom_plane+zoom_row]/$10000],$e60000+[[zoom_plane+zoom_row]&$ffff]
dc.l	$e80000+[[zoom_plane+[2*zoom_row]]/$10000],$ea0000+[[zoom_plane+[2*zoom_row]]&$ffff]
dc.l	$1020000
dc.l	$2a01ff00,$1003300
zoom_cop:
dc.l	$1080000+[2*zoom_row]+4,$10a0000+[2*zoom_row]+4
blk.l	[3*256]-2,0			;255 values
zoom_dd:
dc.l	$2a01ff00,$1000300
dc.l	$fffffffe

;----------------------------------------------------------------------
oldint:		dc.w	0
olddma:		dc.w	0
gfxname:	dc.b	'graphics.library',0,0
;----------------------------------------------------------------------
zoom_load=$50000
l_counter:	dc.w	0
scron:		dc.l	0
scroff:		dc.l	0

>extern "Data2:friar.raw",zoom_load,-1
end:
