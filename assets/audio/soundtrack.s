;this file for FamiTone2 library generated by text2data tool

soundtrack_music_data:
	.byte 1
	.word @instruments
	.word @samples-3
	.word @song0ch0,@song0ch1,@song0ch2,@song0ch3,@song0ch4,307,256 ; Pachelbel's Canon in D

@instruments:
	.byte $30 ;instrument $00
	.word @env1,@env0,@env0
	.byte $00
	.byte $f0 ;instrument $01
	.word @env1,@env0,@env0
	.byte $00

@samples:
@env0:
	.byte $c0,$00,$00
@env1:
	.byte $c8,$c6,$c4,$c3,$c2,$00,$04


; Pachelbel's Canon in D
@song0ch0:
	.byte $fb,$08
@song0ch0loop:
@ref0:
	.byte $00,$f9,$83
@ref1:
	.byte $82,$56,$8d,$52,$8d,$4e,$8d,$4c,$8d,$48,$8d,$44,$8d,$48,$8d,$4c
	.byte $8d
@ref2:
	.byte $4e,$8d,$4c,$8d,$48,$8d,$44,$8d,$40,$8d,$3e,$8d,$40,$8d,$44,$8d
@ref3:
	.byte $36,$85,$3e,$85,$44,$85,$40,$85,$3e,$85,$36,$85,$3e,$85,$3a,$85
	.byte $36,$85,$2c,$85,$36,$85,$44,$85,$40,$85,$48,$85,$44,$85,$40,$85
@ref4:
	.byte $3e,$85,$36,$85,$3a,$85,$4c,$85,$4e,$85,$56,$85,$5c,$85,$44,$85
	.byte $48,$85,$40,$85,$44,$85,$3e,$85,$36,$85,$4e,$85,$4e,$89,$4c,$81
@ref5:
	.byte $4f,$4d,$4f,$37,$35,$45,$3b,$3f,$37,$4f,$4d,$49,$4d,$57,$5d,$65
	.byte $59,$57,$53,$59,$57,$53,$4f,$4d,$49,$45,$41,$3f,$3b,$41,$3f,$3a
	.byte $81
@ref6:
	.byte $37,$3b,$3f,$41,$45,$3b,$45,$41,$3f,$49,$45,$41,$45,$41,$3f,$3b
	.byte $37,$2d,$49,$4d,$4f,$4d,$49,$45,$41,$3f,$3b,$49,$45,$49,$45,$40
	.byte $81
@ref7:
	.byte $3e,$85,$56,$85,$52,$8d,$00,$85,$4e,$85,$56,$8d,$60,$8d,$5c,$8d
	.byte $60,$8d,$64,$8d
@ref8:
	.byte $66,$85,$4e,$85,$4c,$8d,$00,$85,$48,$85,$4e,$8d,$4e,$95,$4e,$85
	.byte $4e,$85,$58,$85,$52,$85,$5c,$85
@ref9:
	.byte $5d,$56,$58,$5d,$56,$58,$5c,$44,$48,$4c,$4e,$52,$56,$58,$57,$4e
	.byte $52,$57,$3e,$40,$44,$48,$44,$40,$44,$3e,$40,$44,$41,$48,$44,$41
	.byte $3e,$3a,$3e,$3a,$36,$3a,$3e,$40,$44,$48,$41,$48,$44,$49,$4c,$4e
	.byte $44,$48,$4c,$4e,$52,$56,$58,$5c
@ref10:
	.byte $57,$4e,$52,$57,$52,$4e,$52,$4c,$4e,$52,$56,$52,$4e,$4c,$4e,$83
	.byte $48,$4c,$4e,$36,$3a,$3e,$40,$3e,$3a,$3e,$4e,$4c,$4e,$49,$4e,$4c
	.byte $49,$44,$40,$44,$40,$3e,$40,$44,$48,$4c,$4e,$49,$4e,$4c,$4f,$4c
	.byte $48,$4c,$4e,$52,$4e,$4c,$4e,$48,$4c
@ref11:
	.byte $4f,$01,$4d,$01,$49,$01,$4f,$01,$37,$01,$37,$01,$37,$01,$3b,$00
	.byte $85,$45,$01,$45,$01,$3f,$01,$45,$01,$41,$01,$3f,$01,$41,$01,$52
	.byte $81
@ref12:
	.byte $57,$3f,$41,$3f,$3b,$53,$57,$53,$4f,$3f,$37,$49,$45,$2d,$29,$2d
	.byte $27,$49,$4d,$49,$45,$2d,$29,$2d,$31,$49,$45,$49,$4d,$35,$31,$34
	.byte $81
@ref13:
	.byte $3f,$45,$45,$45,$45,$45,$45,$45,$3f,$3f,$3f,$3f,$3f,$3f,$45,$45
	.byte $41,$41,$41,$4f,$4f,$4f,$4f,$4f,$4f,$4f,$49,$49,$45,$45,$53,$4c
	.byte $81
@ref14:
	.byte $45,$57,$57,$57,$53,$53,$53,$53,$4f,$4f,$4f,$4f,$5d,$5d,$5d,$5d
	.byte $61,$61,$61,$61,$5d,$5d,$5d,$5d,$61,$61,$61,$61,$65,$4d,$4d,$4c
	.byte $81
@ref15:
	.byte $4f,$36,$3a,$3f,$37,$35,$4c,$4e,$53,$4d,$49,$30,$34,$37,$31,$35
	.byte $44,$40,$3f,$3b,$37,$40,$3e,$3b,$41,$3f,$36,$3a,$3f,$45,$41,$48
	.byte $44,$41,$3f,$3b,$44,$40,$3f,$3a,$81
@ref16:
	.byte $3f,$4e,$4c,$4f,$3f,$45,$44,$48,$4d,$45,$3f,$4e,$52,$57,$4f,$57
	.byte $56,$52,$4f,$4d,$49,$48,$44,$49,$4d,$4f,$56,$52,$4f,$57,$59,$4e
	.byte $4c,$49,$49,$45,$3b,$45,$44,$81
@ref17:
	.byte $44,$95,$44,$85,$36,$95,$44,$85,$40,$8d,$44,$8d,$40,$85,$36,$85
	.byte $36,$89,$34,$81
@ref18:
	.byte $36,$85,$4e,$85,$4c,$8d,$48,$8d,$44,$8d,$36,$89,$3b,$3e,$8d,$48
	.byte $8d,$3a,$89,$3a,$81
@ref19:
	.byte $3e,$89,$57,$57,$59,$57,$53,$4e,$89,$4f,$4f,$53,$4f,$4d,$48,$8d
	.byte $4e,$8d,$4f,$4b,$49,$4b,$44,$89,$44,$81
@ref20:
	.byte $44,$89,$5d,$5d,$61,$5d,$59,$56,$89,$57,$57,$59,$57,$53,$4f,$4b
	.byte $49,$4b,$44,$89,$45,$40,$85,$4e,$85,$4c,$89,$4c,$81
@ref21:
	.byte $4e,$85,$4e,$8d,$4c,$8d,$44,$8d,$44,$8d,$40,$8d,$3e,$91,$3b,$3a
	.byte $8d
@ref22:
	.byte $3e,$85,$56,$8d,$52,$85,$4e,$85,$66,$8d,$62,$85,$60,$8d,$66,$85
	.byte $5c,$85,$60,$8d,$5c,$8d
@ref23:
	.byte $5c,$8d,$44,$89,$41,$3e,$8d,$56,$89,$53,$4e,$95,$4e,$85,$4e,$8d
	.byte $4c,$8d
@ref24:
	.byte $4e,$85,$36,$85,$34,$85,$4c,$85,$48,$85,$30,$85,$2c,$85,$44,$85
	.byte $40,$85,$58,$85,$56,$85,$3e,$85,$3a,$85,$48,$85,$3a,$85,$52,$85
@ref25:
	.byte $56,$85,$3e,$85,$3a,$85,$52,$85,$4e,$85,$36,$85,$34,$85,$4c,$85
	.byte $48,$85,$60,$85,$5c,$85,$44,$85,$40,$89,$53,$44,$85,$44,$85
@ref26:
	.byte $44,$8d,$00,$8f
	.byte $fd
	.word @song0ch0loop

; Pachelbel's Canon in D
@song0ch1:
@song0ch1loop:
@ref27:
	.byte $00,$f9,$83
@ref28:
	.byte $00,$f9,$83
	.byte $ff,$10
	.word @ref1
	.byte $ff,$10
	.word @ref2
	.byte $ff,$20
	.word @ref3
	.byte $ff,$20
	.word @ref4
	.byte $ff,$21
	.word @ref5
	.byte $ff,$21
	.word @ref6
	.byte $ff,$14
	.word @ref7
	.byte $ff,$18
	.word @ref8
	.byte $ff,$38
	.word @ref9
	.byte $ff,$39
	.word @ref10
	.byte $ff,$21
	.word @ref11
	.byte $ff,$21
	.word @ref12
	.byte $ff,$21
	.word @ref13
	.byte $ff,$21
	.word @ref14
	.byte $ff,$29
	.word @ref15
	.byte $ff,$28
	.word @ref16
	.byte $ff,$14
	.word @ref17
	.byte $ff,$15
	.word @ref18
	.byte $ff,$1a
	.word @ref19
	.byte $ff,$1d
	.word @ref20
	.byte $ff,$11
	.word @ref21
	.byte $ff,$16
	.word @ref22
	.byte $ff,$12
	.word @ref23
	.byte $ff,$20
	.word @ref24
@ref53:
	.byte $56,$8d,$00,$8f
	.byte $fd
	.word @song0ch1loop

; Pachelbel's Canon in D
@song0ch2:
@song0ch2loop:
@ref54:
	.byte $80,$4e,$8d,$44,$8d,$48,$8d,$3e,$8d,$40,$8d,$36,$8d,$40,$8d,$44
	.byte $8d
@ref55:
	.byte $4e,$8d,$44,$8d,$48,$8d,$3e,$8d,$40,$8d,$36,$8d,$40,$8d,$44,$8d
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
	.byte $ff,$10
	.word @ref55
@ref80:
	.byte $82,$4e,$8d,$00,$8f
	.byte $fd
	.word @song0ch2loop

; Pachelbel's Canon in D
@song0ch3:
@song0ch3loop:
@ref81:
	.byte $f9,$85
@ref82:
	.byte $f9,$85
@ref83:
	.byte $f9,$85
@ref84:
	.byte $f9,$85
@ref85:
	.byte $f9,$85
@ref86:
	.byte $f9,$85
@ref87:
	.byte $f9,$85
@ref88:
	.byte $f9,$85
@ref89:
	.byte $f9,$85
@ref90:
	.byte $f9,$85
@ref91:
	.byte $f9,$85
@ref92:
	.byte $f9,$85
@ref93:
	.byte $f9,$85
@ref94:
	.byte $f9,$85
@ref95:
	.byte $f9,$85
@ref96:
	.byte $f9,$85
@ref97:
	.byte $f9,$85
@ref98:
	.byte $f9,$85
@ref99:
	.byte $f9,$85
@ref100:
	.byte $f9,$85
@ref101:
	.byte $f9,$85
@ref102:
	.byte $f9,$85
@ref103:
	.byte $f9,$85
@ref104:
	.byte $f9,$85
@ref105:
	.byte $f9,$85
@ref106:
	.byte $f9,$85
@ref107:
	.byte $a1
	.byte $fd
	.word @song0ch3loop

; Pachelbel's Canon in D
@song0ch4:
@song0ch4loop:
@ref108:
	.byte $f9,$85
@ref109:
	.byte $f9,$85
@ref110:
	.byte $f9,$85
@ref111:
	.byte $f9,$85
@ref112:
	.byte $f9,$85
@ref113:
	.byte $f9,$85
@ref114:
	.byte $f9,$85
@ref115:
	.byte $f9,$85
@ref116:
	.byte $f9,$85
@ref117:
	.byte $f9,$85
@ref118:
	.byte $f9,$85
@ref119:
	.byte $f9,$85
@ref120:
	.byte $f9,$85
@ref121:
	.byte $f9,$85
@ref122:
	.byte $f9,$85
@ref123:
	.byte $f9,$85
@ref124:
	.byte $f9,$85
@ref125:
	.byte $f9,$85
@ref126:
	.byte $f9,$85
@ref127:
	.byte $f9,$85
@ref128:
	.byte $f9,$85
@ref129:
	.byte $f9,$85
@ref130:
	.byte $f9,$85
@ref131:
	.byte $f9,$85
@ref132:
	.byte $f9,$85
@ref133:
	.byte $f9,$85
@ref134:
	.byte $a1
	.byte $fd
	.word @song0ch4loop
