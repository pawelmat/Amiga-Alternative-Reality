*	Wart1	- Przewaga  R - czerwonego   (0-20000)
*	Wart2	- Przewaga  G - zielonego    (0-20000)
*	Wart3	- Przewaga  B - niebieskiego (0-20000)
*	Wart4	- Szybkosc  R (d-70 lub 441-511)
*	Wart5	- Szybkosc  G (d-70 lub 441-511)
*	Wart6	- Szybkosc  B (d-70 lub 441-511)
*	Wart7	- Nasycenie R (0-90)
*	Wart8	- Nasycenie G (0-90)
*	Wart9	- Nasycenie B (0-90)
*	Wart10	- Szybkosc rozmycia  x (0-70 lub 441-511)
*	Wart11	- Nasycenie rozmycia y (0-43)

WaitBlit:	macro
		btst	#14,$2(a6)
		bne.s	*-8
		endm

		org	$60000
		load	$60000

s:		lea	$dff000,a6
		move.w	#$4000,$9a(a6)
		move.w	#$20,$96(a6)
		move.w	#%1000010000000000,$96(a6)
		bsr	inits
;	move.l	#cop1,$dff080

		move.w	#4000*2,wart_1+2
		move.w	#4030*2,wart_2+2
		move.w	#4060*2,wart_3+2
		move.w	#2*2,wart_4+2
		move.w	#1*2,wart_5+2
		move.w	#1*2,wart_6+2
		move.w	#2*2,wart_7+2
		move.w	#3*2,wart_8+2
		move.w	#2*2,wart_9+2
		move.w	#2*2,wart_10+2
		move.w	#[2*2]-2,wart_11+2
		bsr.L	loop


		move.w	#3040*2,wart_1+2
		move.w	#3000*2,wart_2+2
		move.w	#2060*2,wart_3+2
		move.w	#1*2,wart_4+2
		move.w	#1*2,wart_5+2
		move.w	#3*2,wart_6+2
		move.w	#3*2,wart_7+2
		move.w	#4*2,wart_8+2
		move.w	#5*2,wart_9+2
		move.w	#4*2,wart_10+2
		move.w	#[3*2]-2,wart_11+2
		bsr.s	loop

		move.w	#4000*2,wart_1+2
		move.w	#3000*2,wart_2+2
		move.w	#5000*2,wart_3+2
		move.w	#1*2,wart_4+2
		move.w	#3*2,wart_5+2
		move.w	#2*2,wart_6+2
		move.w	#6*2,wart_7+2
		move.w	#2*2,wart_8+2
		move.w	#4*2,wart_9+2
		move.w	#3*2,wart_10+2
		move.w	#[2*2]-2,wart_11+2
		bsr.s	loop
		rts

br=256/8
ho=140
ystart=$64
cols=50
coplen=cols*4+8

loop:		move.l	$4(a6),d0
		and.l	#$000fff00,d0
		cmp.l	#$00013000,d0
		bne.s	loop
		bsr.w	irq
		move	pokaz,d0
		lsl	#1,d0
		lea	plane1,a1
		lea	plane2,a2
		move	#-1,(a1,d0.w)
		move	#$ff00,(a2,d0.w)
		move	#$ff00,2(a1,d0.w)
		addi	#1,pokaz
		cmpi	#20,pokaz
		bne.s	loop
		move	#350,licz

loop02:		move.l	$4(a6),d0
		and.l	#$000fff00,d0
		cmp.l	#$00013000,d0
		bne.s	loop02
		bsr.w	irq
		subi	#1,licz
		bne.s	loop02

loop03:		move.l	$4(a6),d0
		and.l	#$000fff00,d0
		cmp.l	#$00013000,d0
		bne.s	loop03
		bsr.w	irq
		move	pokaz,d0
		lsl	#1,d0
		lea	plane1,a1
		lea	plane2,a2
		move	#$ff00,(a1,d0.w)
		clr	2(a1,d0.w)
		clr	(a2,d0.w)
		subi	#1,pokaz
		bpl.s	loop03

;		btst	#6,$bfe001
;		bne.s	loop02
		rts

pokaz:		dc.w	0
licz:		dc.w	200

plane1:		blk.w	21,0
plane2:		blk.w	21,0

		blk.w	500,0

;------------------------------------------------------------------
TABa:		dc.w	$1,$1,$1,$1,$1
		dc.w	$2,$2,$2,$2
		dc.w	$3,$3,$3,$3
		dc.w	$4,$4,$4
		dc.w	$5,$5,$5
		dc.w	$6,$6,$6
		dc.w	$7,$7,$7
		dc.w	$8,$8,$8
		dc.w	$9,$9,$9
		dc.w	$a,$a,$a
		dc.w	$b,$b,$b
		dc.w	$c,$c,$c
		dc.w	$d,$d
		dc.w	$e,$e
		dc.w	$f,$f,$f
		dc.w	$e,$e
		dc.w	$d,$d
		dc.w	$c,$c,$c
		dc.w	$b,$b,$b
		dc.w	$a,$a,$a
		dc.w	$9,$9,$9
		dc.w	$8,$8,$8
		dc.w	$7,$7,$7
		dc.w	$6,$6,$6
		dc.w	$5,$5,$5
		dc.w	$4,$4,$4
		dc.w	$3,$3,$3,$3
		dc.w	$2,$2,$2,$2
		dc.w	$1,$1,$1,$1,$1

TABb:		blk.w	TABb-TABa
		blk.w	500,0

COPOFFSET:	dc.l	cop2-cop1

IRQ:		lea	cop1,a0
		add.l	copoffset(pc),a0
		move.l	a0,$84(a6)
		eor.l	#cop2-cop1,copoffset
		lea	data+10,a3
		add.l	copoffset(pc),a3
		lea	sin,a4
		moveq	#cols-1,d7
		lea	mod4+2(pc),a0
wart_6:		add.w	#0,(a0)

		cmp.w	#1024,(a0)
		blo.s	ok1
		sub.w	#1024,(a0)

ok1:
wart_5:		add.w	#0,mod5-mod4(a0)
		cmp.w	#1024,mod5-mod4(a0)
		blo.s	ok2
		sub.w	#1024,mod5-mod4(a0)
ok2:
wart_4:		add.w	#0,mod6-mod4(a0)
		cmp.w	#1024,mod6-mod4(a0)
		blo.s	ok3
		sub.w	#1024,mod6-mod4(a0)
ok3:
mod4:		move.w	#0,d0
mod5:		move.w	#0,d1
mod6:		move.w	#0,d2
wart_3:		move.w	#0,d4
wart_2:		move.w	#0,d5
wart_1:		move.w	#0,d6
		lea	-2&[TABb-TABa/2+TABb-ho](pc),a5

		waitblit
		clr.w	$64(a6)
		clr.l	$60(a6)
		move.w	#coplen-2,$66(a6)

s_R=[%111&4*%10101010/%100]
s_G=[%111&2*%11001100/%10]
s_B=[%111&1*%11110000/%1]

		move.w	#$8f00!s_R!s_G!s_B,$40(a6)
		move.w	#%0100000000000000,$42(a6)
		move.l	#-1,$44(a6)

loop5:		move.w	(a4,d0.w),d3
		muls	d4,d3
		swap	d3
		lea	(a5,d3.w),a0
		move.w	(a4,d1.w),d3
		muls	d5,d3
		swap	d3
		lea	[TABb-TABa/3](a5,d3.w),a1
		move.w	(a4,d2.w),d3
		muls	d6,d3
		swap	d3
		lea	-[TABb-TABa/3](a5,d3.w),a2
		waitblit
		move.l	a0,$50(a6)
		move.l	a1,$4c(a6)
		move.l	a2,$48(a6)
		move.l	a3,$54(a6)
		move.w	#64*ho+1,$58(a6)
wart_9:		add.w	#0,d0
wart_8:		add.w	#0,d1
wart_7:		add.w	#0,d2
		addq.l	#4,a3
		dbra	d7,loop5

		lea	data+1,a3
		add.l	copoffset(pc),a3
		lea	lrsin,a0

mod20:		add.w	#0,a0
wart_10:	add.w	#0,mod20+2
		cmp.w	#sinlen,mod20+2
		blo.s	ok4
		sub.w	#sinlen,mod20+2
ok4:		move.w	#ho-1,d7
lx1:		move.b	(a0)+,(a3)
		move.b	(a0)+,6(a3)
wart_11:	lea	0(a0),a0
		lea	coplen(a3),a3
		dbf	d7,lx1
		rts

inits:		move.l	#$01020000,d1
		move.l	#$01840186,d2
		lea	data,a0
		move	#ystart,d0
		move	#ho-1,d4
loop1:		move.b	d0,(a0)+
		move.b	#$31,(a0)+
		move.w	#$fffe,(a0)+
		move.l	d1,(a0)+
		moveq	#cols-1,d3
loop2:		move.w	d2,(a0)+
		swap	d2
		move.w	d5,(a0)+
		dbra	d3,loop2

		addq.w	#1,d0
		dbra	d4,loop1

		lea	TABa(pc),a0
		move	#TABb-TABa/2-1,d7
loop7:		move.w	(a0),d0
		lsl.w	#8,d0
		move.w	d0,(a0)+
		move.w	d0,TABb-TABa-2(a0)
		move.w	d0,TABb-TABa*2-2(a0)
		dbra	d7,loop7

		lea	sin,a0
		move.w	#sinlen/2-1,d7
loop6:		move.w	(a0)+,d0
		move.w	d0,sinlen*1-2(a0)
		move.w	d0,sinlen*2-2(a0)
		move.w	d0,sinlen*3-2(a0)
		move.w	d0,sinlen*4-2(a0)
		move.w	d0,sinlen*5-2(a0)
		move.w	d0,sinlen*6-2(a0)
		move.w	d0,sinlen*7-2(a0)
		move.w	d0,sinlen*8-2(a0)
		move.w	d0,sinlen*9-2(a0)
		dbra	d7,loop6

		move.w	#sinlen/2-1,d7
		lea	sin,a0
		lea	lrsin,a1
mlrt:		move.w	(a0)+,d0
		muls	#$900,d0
		swap	d0
		add.w	#72,d0
		move.w	d0,d1
		and.w	#$f,d1
		lsr.w	#2,d0
		add.w	d0,d0
		addq.w	#1,d0
		lsl.w	#8,d0
		add.w	d1,d0
		move.w	d0,(a1)+
		move.w	d0,1*sinlen-2(a1)
		move.w	d0,2*sinlen-2(a1)
		move.w	d0,3*sinlen-2(a1)
		move.w	d0,4*sinlen-2(a1)
		move.w	d0,5*sinlen-2(a1)
		move.w	d0,6*sinlen-2(a1)
		move.w	d0,7*sinlen-2(a1)
		move.w	d0,8*sinlen-2(a1)
		move.w	d0,9*sinlen-2(a1)
		move.w	d0,10*sinlen-2(a1)
		move.w	d0,11*sinlen-2(a1)
		move.w	d0,12*sinlen-2(a1)
		move.w	d0,13*sinlen-2(a1)
		move.w	d0,14*sinlen-2(a1)
		dbra	d7,mlrt

		lea	cop1,a0
		lea	cop2,a1
		move	#cop2-cop1/4-1,d0
ic2loop:	move.l	(a0)+,(a1)+
		dbra	d0,ic2loop
		rts

cop1:		dc.w	$8e,ystart*$100+$a9,$90,ystart+ho*$100+$a1
		dc.w	$92,$48,$94,$c0
		dc.w	$108,-br,$10a,-br,$100,$2200
		dc.w	$180,$0
		dc.w	$e0,plane2/$10000,$e2,plane2&$ffff
		dc.w	$e4,plane1/$10000,$e6,plane1&$ffff
data:		blk.b	coplen*ho,0
		dc.l	$01800000
		dc.l	-2

cop2:		blk.b	cop2-cop1

; od $800 do -$800
sin:		dc.w	$0000,$0019,$0032,$004b,$0064
		dc.w	$007e,$0097,$00b0,$00c9,$00e2
		dc.w	$00fb,$0114,$012d,$0145,$015e
		dc.w	$0177,$0190,$01a8,$01c1,$01d9
		dc.w	$01f2,$020a,$0222,$023a,$0253
		dc.w	$026b,$0282,$029a,$02b2,$02ca
		dc.w	$02e1,$02f8,$0310,$0327,$033e
		dc.w	$0355,$036c,$0382,$0399,$03af
		dc.w	$03c5,$03dc,$03f1,$0407,$041d
		dc.w	$0432,$0448,$045d,$0472,$0487
		dc.w	$049b,$04b0,$04c4,$04d8,$04ec
		dc.w	$0500,$0513,$0527,$053a,$054d
		dc.w	$055f,$0572,$0584,$0596,$05a8
		dc.w	$05ba,$05cb,$05dc,$05ed,$05fe
		dc.w	$060f,$061f,$062f,$063f,$064f
		dc.w	$065e,$066d,$067c,$068a,$0699
		dc.w	$06a7,$06b5,$06c2,$06d0,$06dd
		dc.w	$06e9,$06f6,$0702,$070e,$071a
		dc.w	$0725,$0730,$073b,$0746,$0750
		dc.w	$075a,$0764,$076e,$0777,$0780
		dc.w	$0788,$0791,$0799,$07a0,$07a8
		dc.w	$07af,$07b6,$07bc,$07c3,$07c9
		dc.w	$07ce,$07d4,$07d9,$07dd,$07e2
		dc.w	$07e6,$07ea,$07ed,$07f1,$07f4
		dc.w	$07f6,$07f8,$07fa,$07fc,$07fe
		dc.w	$07ff,$07ff,$0800,$0800,$0800
		dc.w	$07ff,$07ff,$07fe,$07fc,$07fa
		dc.w	$07f8,$07f6,$07f4,$07f1,$07ed
		dc.w	$07ea,$07e6,$07e2,$07dd,$07d9
		dc.w	$07d4,$07ce,$07c9,$07c3,$07bc
		dc.w	$07b6,$07af,$07a8,$07a0,$0799
		dc.w	$0791,$0788,$0780,$0777,$076e
		dc.w	$0764,$075a,$0750,$0746,$073b
		dc.w	$0730,$0725,$071a,$070e,$0702
		dc.w	$06f6,$06e9,$06dd,$06d0,$06c2
		dc.w	$06b5,$06a7,$0699,$068a,$067c
		dc.w	$066d,$065e,$064f,$063f,$062f
		dc.w	$061f,$060f,$05fe,$05ed,$05dc
		dc.w	$05cb,$05ba,$05a8,$0596,$0584
		dc.w	$0572,$055f,$054d,$053a,$0527
		dc.w	$0513,$0500,$04ec,$04d8,$04c4
		dc.w	$04b0,$049b,$0487,$0472,$045d
		dc.w	$0448,$0432,$041d,$0407,$03f1
		dc.w	$03dc,$03c5,$03af,$0399,$0382
		dc.w	$036c,$0355,$033e,$0327,$0310
		dc.w	$02f8,$02e1,$02ca,$02b2,$029a
		dc.w	$0282,$026b,$0253,$023a,$0222
		dc.w	$020a,$01f2,$01d9,$01c1,$01a8
		dc.w	$0190,$0177,$015e,$0145,$012d
		dc.w	$0114,$00fb,$00e2,$00c9,$00b0
		dc.w	$0097,$007e,$0064,$004b,$0032
		dc.w	$0019,$0000,$ffe7,$ffce,$ffb5
		dc.w	$ff9c,$ff82,$ff69,$ff50,$ff37
		dc.w	$ff1e,$ff05,$feec,$fed3,$febb
		dc.w	$fea2,$fe89,$fe70,$fe58,$fe3f
		dc.w	$fe27,$fe0e,$fdf6,$fdde,$fdc6
		dc.w	$fdad,$fd95,$fd7e,$fd66,$fd4e
		dc.w	$fd36,$fd1f,$fd08,$fcf0,$fcd9
		dc.w	$fcc2,$fcab,$fc94,$fc7e,$fc67
		dc.w	$fc51,$fc3b,$fc24,$fc0f,$fbf9
		dc.w	$fbe3,$fbce,$fbb8,$fba3,$fb8e
		dc.w	$fb79,$fb65,$fb50,$fb3c,$fb28
		dc.w	$fb14,$fb00,$faed,$fad9,$fac6
		dc.w	$fab3,$faa1,$fa8e,$fa7c,$fa6a
		dc.w	$fa58,$fa46,$fa35,$fa24,$fa13
		dc.w	$fa02,$f9f1,$f9e1,$f9d1,$f9c1
		dc.w	$f9b1,$f9a2,$f993,$f984,$f976
		dc.w	$f967,$f959,$f94b,$f93e,$f930
		dc.w	$f923,$f917,$f90a,$f8fe,$f8f2
		dc.w	$f8e6,$f8db,$f8d0,$f8c5,$f8ba
		dc.w	$f8b0,$f8a6,$f89c,$f892,$f889
		dc.w	$f880,$f878,$f86f,$f867,$f860
		dc.w	$f858,$f851,$f84a,$f844,$f83d
		dc.w	$f837,$f832,$f82c,$f827,$f823
		dc.w	$f81e,$f81a,$f816,$f813,$f80f
		dc.w	$f80c,$f80a,$f808,$f806,$f804
		dc.w	$f802,$f801,$f801,$f800,$f800
		dc.w	$f800,$f801,$f801,$f802,$f804
		dc.w	$f806,$f808,$f80a,$f80c,$f80f
		dc.w	$f813,$f816,$f81a,$f81e,$f823
		dc.w	$f827,$f82c,$f832,$f837,$f83d
		dc.w	$f844,$f84a,$f851,$f858,$f860
		dc.w	$f867,$f86f,$f878,$f880,$f889
		dc.w	$f892,$f89c,$f8a6,$f8b0,$f8ba
		dc.w	$f8c5,$f8d0,$f8db,$f8e6,$f8f2
		dc.w	$f8fe,$f90a,$f917,$f923,$f930
		dc.w	$f93e,$f94b,$f959,$f967,$f976
		dc.w	$f984,$f993,$f9a2,$f9b1,$f9c1
		dc.w	$f9d1,$f9e1,$f9f1,$fa02,$fa13
		dc.w	$fa24,$fa35,$fa46,$fa58,$fa6a
		dc.w	$fa7c,$fa8e,$faa1,$fab3,$fac6
		dc.w	$fad9,$faed,$fb00,$fb14,$fb28
		dc.w	$fb3c,$fb50,$fb65,$fb79,$fb8e
		dc.w	$fba3,$fbb8,$fbce,$fbe3,$fbf9
		dc.w	$fc0f,$fc24,$fc3b,$fc51,$fc67
		dc.w	$fc7e,$fc94,$fcab,$fcc2,$fcd9
		dc.w	$fcf0,$fd08,$fd1f,$fd36,$fd4e
		dc.w	$fd66,$fd7e,$fd95,$fdad,$fdc6
		dc.w	$fdde,$fdf6,$fe0e,$fe27,$fe3f
		dc.w	$fe58,$fe70,$fe89,$fea2,$febb
		dc.w	$fed3,$feec,$ff05,$ff1e,$ff37
		dc.w	$ff50,$ff69,$ff82,$ff9c,$ffb5
		dc.w	$ffce,$ffe7

sinlen=1024
lrsin=sin+[10*sinlen]
