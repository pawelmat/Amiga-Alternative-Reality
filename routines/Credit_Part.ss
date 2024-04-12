
;		      Credits Part


; nota do Kane'a:
; cos sie chyba pieprzy na srodku ale juy to fakam /mam tego dosyc/!
; podpisano - Pillar.


org	$30000
load	$30000

Klatka:	equ	$60000	;adres ladowania klatki 


S:	movem.l	d0-d7/a0-a6,-(a7)
	lea	$70000,a0
cls:	clr.l	(a0)+
	cmpa.l	#$7e000,a0
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

	movem.l	(a7)+,d0-d7/a0-a6
	rts

con0:		dc.l	0
;*------------------------------------------------------------------
SetPic:	lea	$dff000,a6
blit0:	btst	#$e,$02(a6)
	bne.s	Blit0
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka1,$50(a6)
	move.l  Place1,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit1:	btst	#$e,$02(a6)
	bne.s	Blit1
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka2,$50(a6)
	move.l  Place2,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit2:	btst	#$e,$02(a6)
	bne.s	Blit2
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  Klatka3,$50(a6)
	move.l  Place3,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit3:	btst	#$e,$02(a6)
	bne.s	Blit3
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka4,$50(a6)
	move.l  place4,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit4:	btst	#$e,$02(a6)
	bne.s	Blit4
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka5,$50(a6)
	move.l  place5,$54(a6)
	move.w  #%0010000000001010,$58(a6)

blit5:	btst	#$e,$02(a6)
	bne.s	Blit5
	move.w  #$09f0,$40(a6)
	clr.w   $42(a6)
	move.l	#$ffffffff,$44(a6)
	move.l	#$00000014,$64(a6)
	move.l  klatka6,$50(a6)
	move.l  place6,$54(a6)
	move.w  #%0010000000001010,$58(a6)
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
blit:	btst	#$e,$02(a6)
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

>extern	"df0:.store/abstrakcja.raw",klatka,-1
