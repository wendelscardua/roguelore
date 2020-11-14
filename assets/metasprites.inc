metasprite_0_data:

	.byte - 8,- 8,$30,1
	.byte   0,- 8,$31,1
	.byte - 8,  0,$40,1
	.byte   0,  0,$41,1
	.byte 128

metasprite_1_data:

	.byte - 8,- 8,$32,1
	.byte   0,- 8,$33,1
	.byte - 8,  0,$42,1
	.byte   0,  0,$43,1
	.byte 128

metasprite_2_data:

	.byte - 8,  0,$30,1|OAM_FLIP_V
	.byte   0,  0,$31,1|OAM_FLIP_V
	.byte - 8,- 8,$40,1|OAM_FLIP_V
	.byte   0,- 8,$41,1|OAM_FLIP_V
	.byte 128

metasprite_3_data:

	.byte   0,- 8,$32,1|OAM_FLIP_H
	.byte - 8,- 8,$33,1|OAM_FLIP_H
	.byte   0,  0,$42,1|OAM_FLIP_H
	.byte - 8,  0,$43,1|OAM_FLIP_H
	.byte 128

metasprite_4_data:

	.byte - 8,- 8,$34,3
	.byte   0,- 8,$35,3
	.byte - 8,  0,$44,3
	.byte   0,  0,$45,3
	.byte 128

metasprite_5_data:

	.byte - 8,- 8,$36,2
	.byte   0,- 8,$37,2
	.byte - 8,  0,$46,2
	.byte   0,  0,$47,2
	.byte 128

metasprite_pointers:

	.word metasprite_0_data
	.word metasprite_1_data
	.word metasprite_2_data
	.word metasprite_3_data
	.word metasprite_4_data
	.word metasprite_5_data

