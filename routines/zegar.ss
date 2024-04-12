
;	takie szamanie z zegarem /code by pillar of suspect/
;		part of sct demo - alt. reality

org	$30000
load	$30000

; externy na samym dole - milej zabawy

;-------------------------------------------------------------------
doit=0

przerwa1=50			;prerwa miedzy napisami
przerwa2=10			;ile trwa napis

s:
mk_Start:
	movem.l	d0-d7/a0-a6,-(a7)
if doit=0
	move.w	$dff002,d0
	ori.w	#$8000,d0
	move.w	d0,mk_OlDMA
	move.w	#%1000001111000000,$dff096
endif
	bsr	mk_Init

if doit=0
	move.l	4,a6
	lea	gfxname,a1
	jsr	-552(a6)
	move.l	d0,a0
	move.l	$26(a0),$dff080
	clr.w	$dff088
	move.w	mk_OlDMA,$dff096
endif
	movem.l	(a7)+,d0-d7/a0-a6
	rts

mk_OlDMA:		dc.w	0
Gfxname:	dc.b	"graphics.library",0,0
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

mk_WBlt:btst	#$e,$dff002
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
	btst	#$e,$02(a3)
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

>extern	"df0:.store/zegar.pic",zegar,-1
>extern	"df0:.store/time.pic",time,-1
>extern	"df0:.store/has.pic",has,-1
>extern	"df0:.store/come.pic",come,-1

end=loadit+4680+5112+2310+3950
