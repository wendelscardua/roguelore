.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"

.feature force_range
.linecont +

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF=$c000

; music/sfx constants
.enum music_track
  CanonInD
.endenum

.enum sfx
  Collision
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
.local skip
  save_regs
  LDA #music_track::track
  JSR FamiToneMusicPlay
  restore_regs
.endmacro

.macro PRINT string, ppuaddr
  save_regs
  LDA #<(string)
  STA addr_ptr
  LDA #>(string)
  STA addr_ptr+1
  LDA #<(ppuaddr)
  STA ppu_addr_ptr
  LDA #>(ppuaddr)
  STA ppu_addr_ptr+1
  JSR write_string
  restore_regs
.endmacro

.macro SCREEN_OFF
  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering
.endmacro

.macro SCREEN_ON
  LDA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK
.endmacro

.macro VERTICAL_PPUADDR
  LDA #%10001100
  STA PPUCTRL
.endmacro

.macro HORIZONTAL_PPUADDR
  LDA #%10001000
  STA PPUCTRL
.endmacro

; game config

MAX_DUNGEON_LEVELS = 20
MAX_AGENTS = 16

.enum agent_type
  saci
  corpo_seco
  mula_sem_cabeca
  boitata
  cuca
  mapinguari
.endenum

.enum direction
  up
  down
  left
  right
.endenum

; debug - macros for NintendulatorDX interaction
.ifdef DEBUG
.macro debugOut str
  sta $4040
  jmp :+
      .byte str, 0
:
.endmacro

.macro debugRegs
  STA debug_a
  STX debug_x
  STY debug_y
.endmacro

.define fHex8( addr ) 1, 0, <(addr), >(addr)
.define fDec8( addr ) 1, 1, <(addr), >(addr)
.define fHex16( addr ) 1, 2, <(addr), >(addr)
.define fDec16( addr ) 1, 3, <(addr), >(addr)
.else
.macro debugOut str
.endmacro
.macro debugRegs
.endmacro
.endif

.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186
.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat
.zeropage

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rng_seed
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2
sprite_ptr: .res 2 ; metasprite pointer

nmis: .res 1
old_nmis: .res 1

sprite_counter: .res 1

agents_type: .res MAX_AGENTS
agents_x: .res MAX_AGENTS ; (0..31)
agents_y: .res MAX_AGENTS ; (0..19)
agents_str: .res MAX_AGENTS
agents_int: .res MAX_AGENTS
agents_spd: .res MAX_AGENTS
agents_direction: .res MAX_AGENTS
agents_aux: .res MAX_AGENTS
agents_turn_counter: .res MAX_AGENTS

temp_x: .res 1
temp_y: .res 1
temp_acc: .res 1

.enum game_states
  waiting_to_start
  playing
  game_over
.endenum

game_state: .res 1

.segment "BSS"
; non-zp RAM goes here

;;; generated levels

; hardcoded level indices
dungeon_levels: .res MAX_DUNGEON_LEVELS
; coordinates of "up stairs"
dungeon_up_stairs_x: .res MAX_DUNGEON_LEVELS
dungeon_up_stairs_y: .res MAX_DUNGEON_LEVELS
; coordinates of "down stairs"
dungeon_down_stairs_x: .res MAX_DUNGEON_LEVELS
dungeon_down_stairs_y: .res MAX_DUNGEON_LEVELS

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  save_regs
  INC nmis
  restore_regs
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; horizontal mirroring (fix for everdrive)
  LDA #%00000001
  STA $a000

  SCREEN_ON

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  ; init FamiTone SFX
  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; init rng
  LDA #$a9
  STA rng_seed
  LDA #$73
  STA rng_seed+1

  SEI ; disable interrupts

  JSR go_to_title

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
.ifdef DEBUG
  LDA #%01011110  ; red tint
  STA PPUMASK
.endif
  JSR refresh_oam
  ; reset ppuaddr
  BIT PPUSTATUS
  JSR set_scroll

  ; new frame code
  JSR game_state_handler
.ifdef DEBUG
  LDA #%00011110  ; no tint
  STA PPUMASK
.endif
  JSR FamiToneUpdate
  JSR slow_updates
etc:
  JMP forever
.endproc

.proc refresh_oam
  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTS
.endproc

.proc set_scroll
  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
.endproc

.proc load_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_default_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #0
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #2
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #4
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #5
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #6
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #7
  STA BANK_DATA
  RTS
.endproc

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc slow_updates
  JSR rand
  RTS
.endproc

.proc go_to_title
  LDA #game_states::waiting_to_start
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  PLAY CanonInD

  RTS
.endproc

.proc go_to_playing
  LDA #game_states::playing
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_main
  STA rle_ptr
  LDA #>nametable_main
  STA rle_ptr+1
  JSR unrle

  ; game setup here

  JSR generate_dungeon_levels

  ; turn on screen

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  VBLANK

  SCREEN_ON

  ; PLAY CanonInD

  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state

  RTS
.endproc

.proc waiting_to_start
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_playing
:
  RTS
.endproc

.proc game_over
  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START|BUTTON_A|BUTTON_B)
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc playing
  JSR readjoy
  RTS
.endproc

; rolls a "d6"
; return: A = random number (1..6)
.proc roll_d6
reroll:
  JSR rand
  AND #%111
  BEQ reroll
  CMP #%111
  BEQ reroll
  RTS
.endproc

.proc generate_dungeon_levels
  LDX #0
dungeon_level_loop:
  JSR rand
  AND #%1111
  STA dungeon_levels, X
  TAY ; Y is used for collision below

reroll_up_stairs:
  JSR rand
  AND #%11111
  STA temp_x ; 0..31
  JSR rand
  AND #%1111
  CLC
  ADC #2
  STA temp_y ; 2..17 (0..19 is harder to roll)

  JSR dungeon_level_collision
  BNE reroll_up_stairs

  LDA temp_x
  STA dungeon_up_stairs_x, X
  LDA temp_y
  STA dungeon_up_stairs_y, X

reroll_down_stairs:
  JSR rand
  AND #%11111
  STA temp_x ; 0..31
  JSR rand
  AND #%1111
  CLC
  ADC #2
  STA temp_y ; 2..17 (0..19 is harder to roll)

  JSR dungeon_level_collision
  BNE reroll_down_stairs

  LDA temp_x
  STA dungeon_down_stairs_x, X
  LDA temp_y
  STA dungeon_down_stairs_y, X

  INX
  CPX #MAX_DUNGEON_LEVELS
  BNE dungeon_level_loop
  RTS
.endproc

; detects collision between coordinate and level tile
; input: Y  = hardcoded level index
;        temp_x, temp_y = tile coordinates (0..31, 0..19)
; cobbles: Y, temp_acc
; return: flag nonzero if collided with dungeon
.proc dungeon_level_collision
  LDA map_data_ptr_l, Y
  STA addr_ptr
  LDA map_data_ptr_h, Y
  STA addr_ptr+1

  ; byte index = (y * 32 + x) / 8
  ;            = y * 4 + x / 8

  LDA temp_y
  ASL
  ASL
  STA temp_acc
  LDA temp_x
  LSR
  LSR
  LSR
  CLC
  ADC temp_acc
  TAY
  LDA (addr_ptr), Y
  STA temp_acc

  ; bit index = x % 8
  LDA temp_x
  AND #%111
  TAY
  LDA temp_acc
  AND map_mask, Y

  RTS
.endproc

.proc write_string
  LDA PPUSTATUS
  LDA ppu_addr_ptr+1
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDY #$00
@loop:
  LDA (addr_ptr), Y
  CMP #$ff
  BEQ @exit
  STA PPUDATA
  INY
  JMP @loop
@exit:
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

.define game_state_handlers waiting_to_start-1, playing-1, game_over-1

game_state_handlers_l: .lobytes game_state_handlers
game_state_handlers_h: .hibytes game_state_handlers

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"

.include "../assets/metasprites.inc"

; mask bit for map data
map_mask:
.repeat 8, i
  .byte 1<<(7-i)
.endrepeat

; map data

.define map_data_pointers map_data_001, map_data_002, map_data_003, map_data_004, \
                          map_data_005, map_data_006, map_data_007, map_data_008, \
                          map_data_009, map_data_010, map_data_011, map_data_012, \
                          map_data_013, map_data_014, map_data_015, map_data_016
map_data_ptr_l: .lobytes map_data_pointers
map_data_ptr_h: .hibytes map_data_pointers

.repeat 16, map_index
.ident(.concat("map_data_", .sprintf("%03d", map_index + 1))): .incbin .concat("../assets/maps/", .sprintf("%03d", map_index + 1), ".bin")
.endrepeat

; masks for faster bounded rng

rng_mask:
.byte %0
.byte %1
.repeat 2
.byte %11
.endrepeat
.repeat 4
.byte %111
.endrepeat
.repeat 8
.byte %1111
.endrepeat
.repeat 16
.byte %11111
.endrepeat
.repeat 32
.byte %111111
.endrepeat
.repeat 64
.byte %1111111
.endrepeat
.repeat 128
.byte %11111111
.endrepeat

.segment "CHR"
.incbin "../assets/chr/bg-4k-main.chr"
.incbin "../assets/chr/sprites-4k.chr"

; 1k blocks
;  0 : bg-main
;  1 : bg-main
;  2 : bg-main
;  3 : bg-main
;  4 : sp
;  5 : sp
;  6 : sp
;  7 : sp

; banks
; 0 : 2k bg
; 1 : 2k bg
; 2 : 1k sp
; 3 : 1k sp
; 4 : 1k sp
; 5 : 1k sp
