.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"
.include "charmap.inc"
.include "vram-buffer.inc"

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

.macro INX_ACTION_QUEUE
  INX
  TXA
  AND #(MAX_ACTIONS - 1)
  TAX
.endmacro

.macro ENQUEUE_ACTION item
  .if (.match ({item}, x))
  TXA
  .elseif (.match ({item}, y))
  TYA
  .elseif (.match ({item}, a))
  .else
  LDA item
  .endif
  LDX action_queue_tail
  STA action_queue, X
  INX_ACTION_QUEUE
  STX action_queue_tail
.endmacro

; game config

MAX_DUNGEON_LEVELS = 20
MAX_AGENTS = 16
MAX_ACTIONS = 64 ; must be power of 2
ACTION_COUNTER = 6

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

.enum action_type
  move
  skill_a
  skill_b
.endenum

.enum playing_state
  ; we are decrementing action counters
  ; until at least one of them becomes negative
  action_counter
  ; player agent must act (negative counter), waiting input
  player_input
  ; processing actions for the other agents
  agents_input
  ; animating actions
  process_actions
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

.export nmis
.export old_nmis

nmis: .res 1
old_nmis: .res 1

sprite_counter: .res 1

agents_type: .res MAX_AGENTS
agents_x: .res MAX_AGENTS ; (0..31)
agents_y: .res MAX_AGENTS ; (0..19)
agents_lv: .res MAX_AGENTS
agents_str: .res MAX_AGENTS
agents_int: .res MAX_AGENTS
agents_spd: .res MAX_AGENTS
agents_direction: .res MAX_AGENTS
agents_hp: .res MAX_AGENTS
agents_max_hp: .res MAX_AGENTS
agents_aux: .res MAX_AGENTS
agents_action_counter: .res MAX_AGENTS
num_agents: .res 1

; experience, each byte = 2 decimal digits
player_xp: .res 3
; flag to tell if player has the "amulet of Yendor"
; (in this case his grandpa's cap)
yendor: .res 1

embers: .res 1
max_embers: .res 1
ember_direction: .res 1
ember_x: .res 1
ember_y: .res 1

action_queue_head: .res 1
action_queue_tail: .res 1

temp_x: .res 1
temp_y: .res 1
temp_acc: .res 1
temp_tile: .res 1
temp_flag: .res 1

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

current_dungeon_level: .res 1
current_playing_state: .res 1

action_queue: .res MAX_ACTIONS

.segment "PRGRAM"
.repeat 20, dungeon_level
.ident(.concat("dungeon_level_", .sprintf("%d", dungeon_level + 1) , "_agents")): .res 256
.endrepeat

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
  JSR flush_vram_buffer
  JSR refresh_oam
  ; reset ppuaddr
  BIT PPUSTATUS
  JSR set_scroll
  JSR FamiToneUpdate
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

  ; enable PRG RAM
  LDA #%10000000
  STA $a001

  reset_vram_stack

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
  ; new frame code
  JSR game_state_handler
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

.proc playing_state_handler
  LDX current_playing_state
  LDA playing_state_handlers_h, X
  PHA
  LDA playing_state_handlers_l, X
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

  ; PLAY CanonInD

  RTS
.endproc

.proc go_to_playing
  SCREEN_OFF

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

  LDA #0
  STA current_dungeon_level

  ; initial agent (player)
  LDA #agent_type::saci
  STA agents_type
  LDA #1
  STA agents_lv
  STA agents_str
  STA agents_int
  STA agents_spd
  JSR roll_d6
  CLC
  ADC #10
  STA agents_max_hp
  STA agents_hp
  LDA #direction::right
  STA agents_direction
  LDA #(ACTION_COUNTER-1)
  STA agents_action_counter
  LDA dungeon_up_stairs_x
  STA agents_x
  LDA dungeon_up_stairs_y
  STA agents_y

  LDA #0
  STA player_xp
  STA player_xp+1
  STA player_xp+2

  STA yendor

  JSR roll_d6
  STA embers
  STA max_embers
  LDA #$ff
  STA ember_direction

  LDA #1
  STA num_agents

  JSR refresh_stats

  .ifdef DEBUG
  write_string_to_vram $2382, string_action_counter
  .endif
  LDA #playing_state::action_counter
  STA current_playing_state

  JSR current_level_setup

  ; PLAY CanonInD

  RTS
.endproc

.proc refresh_stats
  LDA agents_str
  CLC
  ADC #$10
  STA temp_acc
  write_tile_to_vram $232f, temp_acc
  LDA agents_int
  CLC
  ADC #$10
  STA temp_acc
  write_tile_to_vram $234f, temp_acc
  LDA agents_spd
  CLC
  ADC #$10
  STA temp_acc
  write_tile_to_vram $236f, temp_acc

  write_decimal_to_vram $231c, embers

  write_decimal_to_vram $2308, agents_hp
  write_decimal_to_vram $230e, agents_max_hp
  write_decimal_to_vram $233c, agents_lv

  write_decimal_to_vram $2358, player_xp+2
  write_decimal_to_vram $235a, player_xp+1
  write_decimal_to_vram $235c, player_xp

  LDA current_dungeon_level
  CLC
  ADC #1
  STA temp_acc
  write_decimal_to_vram $237c, temp_acc
  RTS
.endproc

.proc current_level_setup
  SCREEN_OFF

  ; reset action queue
  LDA #0
  STA action_queue_head
  STA action_queue_tail

  JSR load_agents
  BNE skip_spawn

  LDA #1
  STA num_agents

  ; generate d6 enemies
  JSR roll_d6
  CLC
  ADC #8
  TAX
loop:
  JSR spawn_random_enemy
  DEX
  BNE loop

skip_spawn:

  JSR draw_current_dungeon_level

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc maybe_add_enemy
  LDA num_agents
  CMP #MAX_AGENTS
  BCC :+
  RTS
:
  JSR rand
  AND #%11111
  BEQ :+
  RTS
:
  JSR spawn_random_enemy
  RTS
.endproc

.proc spawn_random_enemy
  save_regs

  ; enemy level will be rand(1.. max(player.lv + dungeon.lv)), capped at 20
  LDX num_agents

  LDA current_dungeon_level
  CLC
  ADC #1
  CMP agents_lv
  BCS :+
  LDA agents_lv
:
  STA temp_acc
  TAX
reroll_level:
  JSR rand
  AND rand_mask, X
  BEQ reroll_level
  CMP temp_acc
  BEQ :+
  BCS reroll_level
:
  LDX num_agents
  STA agents_lv, X

  ; use lookup table to get appropriate enemy
  ; level * 8 + random 3 bits = chosen enemy
  .repeat 3
  ASL
  .endrepeat
  STA temp_acc
  JSR rand
  AND #%111
  ORA temp_acc
  TAY
  LDA random_enemy_table, Y
  STA agents_type, X

  ; stats
  JSR roll_stats_for_agent

  ; position

reroll_position:
  JSR rand
  AND #%11111
  STA temp_x ; 0..31
  JSR rand
  AND #%1111
  CLC
  ADC #2
  STA temp_y ; 2..17 (0..19 is harder to roll)

  LDX current_dungeon_level
  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BNE reroll_position

  ; check collision with previous agents
  LDX num_agents
loop:
  DEX
  BMI exit_loop
  LDA agents_x, X
  CMP temp_x
  BNE loop
  LDA agents_y, X
  CMP temp_y
  BNE loop
  JMP reroll_position
exit_loop:
  LDX num_agents
  LDA temp_x
  STA agents_x, X
  LDA temp_y
  STA agents_y, X
  LDA #(ACTION_COUNTER - 1)
  STA agents_action_counter, X

  INC num_agents

  restore_regs
  RTS
.endproc

.proc roll_stats_for_agent
  ; X = current agent

  LDA agents_type, X
  TAY
  LDA default_str, Y
  STA agents_str, X
  LDA default_int, Y
  STA agents_int, X
  LDA default_spd, Y
  STA agents_spd, X
  LDA default_hp, Y
  STA agents_max_hp, X
  STA agents_hp, X

  LDA agents_lv, X
  STA temp_acc
  LDA default_lv, Y
  CMP temp_acc
  BCS exit_loop ; if target lv < default, use target
  ; else, agent will level up until it reaches target
  STA agents_lv, X

loop:
  LDA agents_lv, X
  CMP temp_acc
  BCS exit_loop
  JSR level_up_agent
  JMP loop
exit_loop:
  RTS
.endproc

.proc draw_current_dungeon_level
  LDA #0
  STA temp_y
  LDA #$20
  STA ppu_addr_ptr
  LDA #$42
  STA ppu_addr_ptr+1
  BIT PPUSTATUS

row_loop:

  LDA ppu_addr_ptr
  STA PPUADDR
  LDA ppu_addr_ptr+1
  STA PPUADDR

  LDA #2
  STA temp_x
column_loop:
  LDX current_dungeon_level
  JSR _detect_stairs_here
  BEQ no_stairs
  STA PPUDATA
  JMP next
no_stairs:
  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BEQ free_cell
  LDA #$62
  STA PPUDATA
  JMP next
free_cell:
  LDA #$72
  STA PPUDATA
next:
  INC temp_x
  LDA temp_x
  CMP #30
  BNE column_loop

  LDA #$20
  CLC
  ADC ppu_addr_ptr+1
  STA ppu_addr_ptr+1
  BCC :+
  INC ppu_addr_ptr
:

  INC temp_y
  LDA temp_y
  CMP #20
  BNE row_loop

  RTS
.endproc

.proc _detect_stairs_here
  LDA dungeon_up_stairs_x, X
  CMP temp_x
  BNE check_down
  LDA dungeon_up_stairs_y, X
  CMP temp_y
  BNE check_down
  LDA #$73
  RTS
check_down:
  CPX #(MAX_DUNGEON_LEVELS-1)
  BEQ no_stairs
  LDA dungeon_down_stairs_x, X
  CMP temp_x
  BNE no_stairs
  LDA dungeon_down_stairs_y, X
  CMP temp_y
  BNE no_stairs
  LDA #$63
  RTS
no_stairs:
  LDA #0
  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state
  write_string_to_vram $2302, string_game_over
  RTS
.endproc

.proc go_to_bad_end
  SCREEN_OFF

  LDA #game_states::game_over
  STA game_state

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_bad_end
  STA rle_ptr
  LDA #>nametable_bad_end
  STA rle_ptr+1
  JSR unrle

  JSR refresh_stats

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_good_end
  SCREEN_OFF

  LDA #game_states::game_over
  STA game_state

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_good_end
  STA rle_ptr
  LDA #>nametable_good_end
  STA rle_ptr+1
  JSR unrle

  JSR refresh_stats

  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  STA PPUSCROLL
  STA PPUSCROLL

  VBLANK

  SCREEN_ON

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
  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-
  STX sprite_counter

  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START|BUTTON_SELECT)
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc playing
  JSR playing_state_handler
  JSR render_stuff
  RTS
.endproc

.proc render_stuff
  LDX #0
  STX sprite_counter
  JSR render_projectiles
  JSR render_agents
  JSR maybe_render_yendor

  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-
  RTS
.endproc

.proc render_projectiles
  LDA ember_direction
  BPL :+
  RTS
:
  LDX sprite_counter

  LDA ember_x
  .repeat 3
  ASL
  .endrepeat
  STA oam_sprites+Sprite::xcoord, X
  LDA ember_y
  .repeat 3
  ASL
  .endrepeat
  CLC
  ADC #$0f
  STA oam_sprites+Sprite::ycoord, X

  LDA #$10 ; ember tile
  STA oam_sprites+Sprite::tile, X
  LDA rng_seed
  AND #%10
  STA oam_sprites+Sprite::flag, X

  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  STX sprite_counter

  RTS
.endproc

.proc maybe_render_yendor
  LDA yendor
  BEQ no_yendor
  LDA #17
  STA temp_x
  LDA #26
  STA temp_y
  JMP render
no_yendor:
  LDX current_dungeon_level
  CPX #(MAX_DUNGEON_LEVELS - 1)
  BEQ last_dungeon
  RTS
last_dungeon:
  LDA dungeon_down_stairs_x, X
  STA temp_x
  LDA dungeon_down_stairs_y, X
  STA temp_y
render:
  LDX sprite_counter

  LDA temp_x
  .repeat 3
  ASL
  .endrepeat
  STA oam_sprites+Sprite::xcoord, X
  LDA temp_y
  .repeat 3
  ASL
  .endrepeat
  CLC
  ADC #$0f
  STA oam_sprites+Sprite::ycoord, X
  LDA yendor
  BNE no_oscillation
  LDA nmis
  AND #%1111
  BNE no_oscillation

  LDA rng_seed
  AND #%1
  BEQ :+
  DEC oam_sprites+Sprite::ycoord, X
:
  LDA rng_seed+1
  AND #%1
  BEQ no_oscillation
  INC oam_sprites+Sprite::ycoord, X
no_oscillation:

  LDA #$12 ; cap tile
  STA oam_sprites+Sprite::tile, X
  LDA #$02
  STA oam_sprites+Sprite::flag, X

  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  STX sprite_counter
  RTS
.endproc

; decrements action counter of agents by their speed
; when counter is negative, that agent will act this turn
.proc action_counter_handler
  LDX num_agents
  DEX
loop:
  LDA agents_action_counter, X
  SEC
  SBC agents_spd, X
  STA agents_action_counter, X
  BPL next
  CPX #$00
  BEQ player_input_time
  LDA current_playing_state
  CMP #playing_state::action_counter
  BNE next
  LDA #playing_state::agents_input
  STA current_playing_state
  JMP next
player_input_time:
  LDA #playing_state::player_input
  STA current_playing_state
  .ifdef DEBUG
  write_string_to_vram $2382, string_player_input
  .endif
next:
  DEX
  BPL loop
  LDA current_playing_state
  CMP #playing_state::agents_input
  BEQ :+
  RTS
:
  .ifdef DEBUG
  write_string_to_vram $2382, string_agents_input
  .endif
  RTS
.endproc

.proc player_input_handler
  JSR readjoy
  LDA pressed_buttons
  BNE :+
  RTS
:
  LDA agents_action_counter
  CLC
  ADC #ACTION_COUNTER
  STA agents_action_counter
  .ifdef DEBUG
  write_string_to_vram $2382, string_agents_input
  .endif
  LDA #playing_state::agents_input
  STA current_playing_state

  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  ENQUEUE_ACTION #0
  ENQUEUE_ACTION #action_type::skill_a
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  ENQUEUE_ACTION #0
  ENQUEUE_ACTION #action_type::skill_b
  RTS
:

  LDX #$ff
  STA temp_acc
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDX #direction::up
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDX #direction::down
:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDX #direction::left
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDX #direction::right
:

  CPX #$ff
  BNE :+
  ; no direction input
  RTS
:
  STX agents_direction

  LDA agents_x
  CLC
  ADC delta_x_lt, X
  STA temp_x
  LDA agents_y
  CLC
  ADC delta_y_lt, X
  STA temp_y

  LDX current_dungeon_level
  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BEQ :+
  ; can't walk through walls, no action happened
  RTS
:
  ENQUEUE_ACTION #0
  ENQUEUE_ACTION #action_type::move

  RTS
.endproc

.proc agents_input_handler
  LDX #1
@loop:
  CPX num_agents
  BCS @exit

  LDA agents_action_counter, X
  BPL @next

  ; restore action counter
  CLC
  ADC #ACTION_COUNTER
  STA agents_action_counter, X

  save_regs
  JSR get_input_for_agent
  restore_regs

@next:
  INX
  JMP @loop
@exit:
  .ifdef DEBUG
  write_string_to_vram $2382, string_process_actions
  .endif
  LDA #playing_state::process_actions
  STA current_playing_state
  RTS
.endproc

; enqueue action for current (X-indexed) agent if able
.proc get_input_for_agent
  LDA agents_type, X
  TAY
  LDA agent_ais_h, Y
  PHA
  LDA agent_ais_l, Y
  PHA
  RTS
.endproc

; move 75% of turns
; totally random direction
.proc corpo_seco_ai
  JSR rand
  AND #%11
  BNE move
  RTS
move:
  JSR rand
  AND #%11
  STA agents_direction, X
  TAY

  LDA agents_x, X
  CLC
  ADC delta_x_lt, Y
  STA temp_x

  LDA agents_y, X
  CLC
  ADC delta_y_lt, Y
  STA temp_y

  LDY current_dungeon_level
  LDA dungeon_levels, Y
  TAY
  JSR dungeon_level_collision
  BEQ no_collision
  RTS
no_collision:
  ENQUEUE_ACTION X
  ENQUEUE_ACTION #action_type::move
  RTS
.endproc

; straight line
; stay still 25% of the turns
; changes direction
; - when colliding with walls
; - when near player (melee attack)
; - with a small random chance
.proc mula_sem_cabeca_ai
  JSR rand
  AND #%11
  BNE move
  RTS
move:
  JSR _set_melee_direction

  ; small chance of randomizing direction
  LDA rng_seed
  AND #%1111100
  BNE after_redirect
  LDA rng_seed+1
  AND #%11
  STA agents_direction, X
after_redirect:
  LDY agents_direction, X

  LDA agents_x, X
  CLC
  ADC delta_x_lt, Y
  STA temp_x

  LDA agents_y, X
  CLC
  ADC delta_y_lt, Y
  STA temp_y

  LDY current_dungeon_level
  LDA dungeon_levels, Y
  TAY
  JSR dungeon_level_collision
  BEQ no_collision

  LDA rng_seed+1
  AND #%1100
  LSR
  LSR
  STA agents_direction, X
  RTS
no_collision:
  ENQUEUE_ACTION X
  ENQUEUE_ACTION #action_type::move

  RTS
.endproc

.segment "RODATA"
reverse_direction:
.byte direction::down, direction::up, direction::right, direction::left
.segment "CODE"

; zigzaging movement
; small chance of changind direction
; will melee attack if able
.proc boitata_ai
  JSR _set_melee_direction
  JSR rand
  ; small chance of rotating direction
  LDA rng_seed
  AND #%1111100
  BNE after_redirect
  LDA agents_direction, X
  CLC
  ADC #1
  AND #%11
  STA agents_direction, X
after_redirect:
  LDY agents_direction, X

  LDA agents_x, X
  CLC
  ADC delta_x_lt, Y
  STA temp_x

  LDA agents_y, X
  CLC
  ADC delta_y_lt, Y
  STA temp_y

  LDY current_dungeon_level
  LDA dungeon_levels, Y
  TAY
  JSR dungeon_level_collision
  BNE collision

  ENQUEUE_ACTION X
  ENQUEUE_ACTION #action_type::move
  RTS
collision:
  LDY agents_direction, X
  LDA reverse_direction, Y
  STA agents_direction, X
  RTS
.endproc

.proc cuca_ai
  JSR corpo_seco_ai ; TODO customize
  RTS
.endproc

.proc mapinguari_ai
  JSR corpo_seco_ai ; TODO customize
  RTS
.endproc

; change agent direction to attack player
; as long as the player is in melee range
; X = agent index
.proc _set_melee_direction
  LDA agents_x, X
  CMP agents_x
  BEQ maybe_vertical
  LDA agents_y, X
  CMP agents_y
  BEQ maybe_horizontal
  RTS
maybe_horizontal:
  LDA agents_x
  SEC
  SBC agents_x, X
  CMP #$01
  BEQ move_right
  CMP #$ff
  BEQ move_left
  RTS
move_right:
  LDA #direction::right
  STA agents_direction, X
  RTS
move_left:
  LDA #direction::left
  STA agents_direction, X
  RTS
maybe_vertical:
  LDA agents_y
  SEC
  SBC agents_y, X
  CMP #$01
  BEQ move_down
  CMP #$ff
  BEQ move_up
  RTS
move_down:
  LDA #direction::down
  STA agents_direction, X
  RTS
move_up:
  LDA #direction::up
  STA agents_direction, X
  RTS
.endproc

.macro copy_yx variable
  LDA variable, Y
  STA variable, X
.endmacro

; deletes dead agents
.proc garbage_collector
  JSR refresh_stats
  LDA agents_hp
  BNE :+
  JSR go_to_game_over
  RTS
:

  LDX #$1
  LDY num_agents
  DEY
@loop:
  CPX num_agents
  BCS @exit

  LDA agents_hp, X
  BNE @next

  copy_yx agents_type
  copy_yx agents_x
  copy_yx agents_y
  copy_yx agents_lv
  copy_yx agents_str
  copy_yx agents_int
  copy_yx agents_spd
  copy_yx agents_direction
  copy_yx agents_hp
  copy_yx agents_max_hp
  copy_yx agents_aux
  copy_yx agents_action_counter

  DEY
  DEC num_agents
  JMP @loop
@next:
  INX
  JMP @loop
@exit:
  RTS
.endproc

.proc ember_handler
  ; slow down ember movement
  LDA nmis
  AND #%1
  BEQ :+
  RTS
:

  LDX ember_direction

  LDA delta_x_lt, X
  CLC
  ADC ember_x
  STA temp_x

  LDA delta_y_lt, X
  CLC
  ADC ember_y
  STA temp_y

  LDY current_dungeon_level
  LDA dungeon_levels, Y
  TAY
  JSR dungeon_level_collision
  BEQ no_collision
  LDA #$ff
  STA ember_direction
  RTS
no_collision:
  LDA temp_x
  STA ember_x
  LDA temp_y
  STA ember_y

  LDX #$1
@loop:
  LDA agents_x, X
  CMP temp_x
  BNE @next
  LDA agents_y, X
  CMP temp_y
  BNE @next
  JMP hit_enemy
@next:
  INX
  CPX num_agents
  BNE @loop
  RTS
hit_enemy:
  LDA #$ff
  STA ember_direction

  JSR roll_d6
  CLC
  ADC agents_int
  STA temp_acc

  LDA agents_hp, X
  SEC
  SBC temp_acc
  BPL :+
  LDA #0
:
  STA agents_hp, X
  BEQ :+
  RTS
:
  JSR gain_xp_from_kill
  RTS
.endproc

.proc process_actions_handler
  LDA ember_direction
  BMI agent_actions
  JSR ember_handler
  RTS
agent_actions:
  LDX action_queue_head
  CPX action_queue_tail
  BNE :+
  ; no actions available
  JSR garbage_collector
  .ifdef DEBUG
  write_string_to_vram $2382, string_action_counter
  .endif
  JSR maybe_add_enemy
  LDA #playing_state::action_counter
  STA current_playing_state
  RTS
:

  ; TODO implement actions
  LDY action_queue, X ; Y = actor index
  INX_ACTION_QUEUE

  LDA action_queue, X
  PHA
  INX_ACTION_QUEUE
  STX action_queue_head
  PLA
  TAX
  LDA action_handlers_h, X
  PHA
  LDA action_handlers_l, X
  PHA
  RTS
.endproc

; Y = actor index
.proc move_handler
  ; TODO smooth sprite movement
  LDX agents_direction, Y

  LDA delta_x_lt, X
  CLC
  ADC agents_x, Y
  STA temp_x

  LDA delta_y_lt, X
  CLC
  ADC agents_y, Y
  STA temp_y

  LDX #$0
@loop:
  LDA agents_x, X
  CMP temp_x
  BNE @next
  LDA agents_y, X
  CMP temp_y
  BNE @next

  ; TODO smooth attack?
  ; TODO maybe attack between enemies?
  CPX #$0
  BEQ @melee
  CPY #$0
  BEQ @melee
  RTS
@melee:
  JSR melee_attack

  RTS
@next:
  INX
  CPX num_agents
  BNE @loop

  LDA agents_hp, Y
  BNE :+
  RTS
:
  LDA temp_x
  STA agents_x, Y
  LDA temp_y
  STA agents_y, Y

  CPY #$0
  BNE :+
  ; player-specific stuff
  JSR regenerate_hp
  JSR maybe_get_yendor
:
  RTS
.endproc

.proc maybe_get_yendor
  LDA yendor
  BEQ :+
  RTS
:
  LDX current_dungeon_level
  CPX #(MAX_DUNGEON_LEVELS - 1)
  BEQ :+
  RTS
:
  LDA agents_x
  CMP dungeon_down_stairs_x, X
  BEQ :+
  RTS
:
  LDA agents_y
  CMP dungeon_down_stairs_y, X
  BEQ :+
  RTS
:
  LDA #1
  STA yendor
  RTS
.endproc

; Y = agent index
.proc regenerate_hp
  LDA agents_hp, Y
  BNE :+
  RTS
:
  CMP agents_max_hp, Y
  BNE :+
  RTS
:
  STY temp_y
  JSR roll_d6
  CMP #5
  BCS regen
  RTS
regen:
  LDY temp_y
  LDA agents_hp, Y
  CLC
  ADC #1
  CMP agents_max_hp, Y
  BCC no_regen_cap
  LDA agents_max_hp, Y
no_regen_cap:
  STA agents_hp, Y
  RTS
.endproc

.proc regenerate_ember
  LDA embers
  CMP max_embers
  BCC :+
  RTS
:
  JSR roll_d6
  CMP #5
  BCS regen
  RTS
regen:
  INC embers
  RTS
.endproc

.proc skill_a_handler
  ; for the player, this shoots an ember projectile
  CPY #0
  BEQ :+
  RTS ; TODO - maybe add skils for enemies?
:
  LDA embers
  BNE :+
  RTS
:
  LDA agents_direction
  STA ember_direction
  LDA agents_x
  STA ember_x
  LDA agents_y
  STA ember_y
  DEC embers
  RTS
.endproc

; Y = agent index
.proc skill_b_handler
  ; for the player, this is an idle or interact skill
  CPY #0
  BEQ :+
  RTS ; TODO - maybe add skils for enemies?
:

  LDX current_dungeon_level
  ; check if down stairs
  LDA agents_x, Y
  CMP dungeon_down_stairs_x, X
  BNE no_down
  LDA agents_y, Y
  CMP dungeon_down_stairs_y, X
  BNE no_down

  JSR go_down
  RTS

no_down:
  ; check if up stairs
  LDA agents_x, Y
  CMP dungeon_up_stairs_x, X
  BNE no_up
  LDA agents_y, Y
  CMP dungeon_up_stairs_y, X
  BNE no_up

  JSR go_up
  RTS
no_up:
  ; idle
  JSR regenerate_hp
  RTS
.endproc

.proc go_down
  ; last level has the grandpa's cap instead of "down stairs"
  LDA current_dungeon_level
  CMP #(MAX_DUNGEON_LEVELS-1)
  BNE :+
  RTS
:

  JSR save_agents
  INC current_dungeon_level
  LDX current_dungeon_level
  LDA dungeon_up_stairs_x, X
  STA agents_x
  LDA dungeon_up_stairs_y, X
  STA agents_y
  JSR current_level_setup
  RTS
.endproc

.proc go_up
  ; above first level is saci's grandpa
  ; so the game ends, with or without his cap
  LDA current_dungeon_level
  BNE no_ending
  LDA yendor
  BEQ no_yendor
  JSR go_to_good_end
  RTS
no_yendor:
  JSR go_to_bad_end
  RTS
no_ending:
  JSR save_agents
  DEC current_dungeon_level
  LDX current_dungeon_level
  LDA dungeon_down_stairs_x, X
  STA agents_x
  LDA dungeon_down_stairs_y, X
  STA agents_y
  JSR current_level_setup
  RTS
.endproc

.macro save_agent_var variable
.local loop
.local exit_loop
  LDX #1
loop:
  CPX num_agents
  BEQ exit_loop
  LDA variable, X
  STA (addr_ptr), Y
  INY
  INX
  JMP loop
exit_loop:
.endmacro

.proc save_agents
  LDX current_dungeon_level
  LDA dungeon_level_agents_ptr_l, X
  STA addr_ptr
  LDA dungeon_level_agents_ptr_h, X
  STA addr_ptr+1
  LDY #0

  LDA num_agents
  STA (addr_ptr), Y
  INY

  save_agent_var agents_type
  save_agent_var agents_x
  save_agent_var agents_y
  save_agent_var agents_lv
  save_agent_var agents_str
  save_agent_var agents_int
  save_agent_var agents_spd
  save_agent_var agents_direction
  save_agent_var agents_hp
  save_agent_var agents_max_hp
  save_agent_var agents_aux
  save_agent_var agents_action_counter

  RTS
.endproc

.macro load_agent_var variable
.local loop
.local exit_loop
  LDX #1
loop:
  CPX num_agents
  BEQ exit_loop
  LDA (addr_ptr), Y
  STA variable, X
  INY
  INX
  JMP loop
exit_loop:
.endmacro

; returns not-zero if something was loaded
.proc load_agents
  LDX current_dungeon_level
  LDA dungeon_level_agents_ptr_l, X
  STA addr_ptr
  LDA dungeon_level_agents_ptr_h, X
  STA addr_ptr+1
  LDY #0

  LDA (addr_ptr), Y
  BNE :+
  RTS
:
  STA num_agents
  INY

  load_agent_var agents_type
  load_agent_var agents_x
  load_agent_var agents_y
  load_agent_var agents_lv
  load_agent_var agents_str
  load_agent_var agents_int
  load_agent_var agents_spd
  load_agent_var agents_direction
  load_agent_var agents_hp
  load_agent_var agents_max_hp
  load_agent_var agents_aux
  load_agent_var agents_action_counter
  LDA #1
  RTS
.endproc

; agent Y causes melee damage to agent X
.proc melee_attack
  ; dodge check
  ; if d6 < X.spd - Y.spd, dodge
  LDA agents_spd, X
  SEC
  SBC agents_spd, Y
  BMI no_dodge
  BEQ no_dodge
  STA temp_acc
  STY temp_y
  JSR roll_d6
  LDY temp_y
  CMP temp_acc
  BCS no_dodge
  RTS
no_dodge:
  ; damage = 1d6 + strength
  LDA agents_str, Y
  STA temp_acc
  STY temp_y
  JSR roll_d6
  LDY temp_y
  CLC
  ADC temp_acc
  STA temp_acc

  ; take from hp
  LDA agents_hp, X
  SEC
  SBC temp_acc
  BPL :+
  LDA #0
:
  STA agents_hp, X
  BEQ :+
  RTS
:
  ; gain xp from kill if player
  CPY #$0
  BEQ :+
  RTS
:

  JSR gain_xp_from_kill
  JSR regenerate_ember
  RTS
.endproc

; player gains xp from kill X-indexed agent
.proc gain_xp_from_kill
  LDA agents_lv, X
  TAX
  LDA lv_to_xp_l_lt, X
  CLC
  ADC player_xp
  CMP #100
  BCC :+
  SEC
  SBC #100
  INC player_xp+1
:
  STA player_xp

  LDA lv_to_xp_h_lt, X
  CLC
  ADC player_xp+1
  CMP #100
  BCC :+
  SEC
  SBC #100
  INC player_xp+2
:
  STA player_xp+1

  ; check if leveled up
  LDX agents_lv
  CPX #20
  BNE :+
  RTS
:

  LDA player_xp+2
  CMP xp_per_level_2, X
  BEQ check_1
  BCS level_up
  RTS
check_1:
  LDA player_xp+1
  CMP xp_per_level_1, X
  BEQ check_0
  BCS level_up
  RTS
check_0:
  LDA player_xp
  CMP xp_per_level_0, X
  BCS level_up
  RTS
level_up:
  LDX #0
  JSR level_up_agent

  INC max_embers
  RTS
.endproc

; increases agent level
; input X = agent index
; cobbles Y
.proc level_up_agent
  INC agents_lv, X

  ; add some max hp
  JSR roll_d6
  CLC
  ADC agents_max_hp, X
  STA agents_max_hp, X

  ; add random buff
  JSR rand
  AND #%11

  BEQ add_str
  CMP #1
  BEQ add_int
  CMP #2
  BEQ add_spd
add_hp:
  JSR roll_d6
  CLC
  ADC agents_max_hp, X
  STA agents_max_hp, X
  JMP check_caps
add_str:
  INC agents_str, X
  JMP check_caps
add_int:
  INC agents_int, X
  JMP check_caps
add_spd:
  INC agents_spd, X
  ; JMP check_caps
check_caps:
  LDA agents_str, X
  CMP #7
  BCC :+
  LDA #6
  STA agents_str, X
  INC agents_max_hp, X
:
  LDA agents_int, X
  CMP #7
  BCC :+
  LDA #6
  STA agents_int, X
  INC agents_max_hp, X
:
  LDA agents_spd, X
  CMP #7
  BCC :+
  LDA #6
  STA agents_spd, X
  INC agents_max_hp, X
:
  LDA agents_max_hp, X
  CMP #100
  BCC restore_hp
  LDA #99
  STA agents_max_hp, X
restore_hp:
  LDA agents_max_hp, X
  STA agents_hp, X
  RTS
.endproc

.proc render_agents
  LDY #0
  LDX sprite_counter
loop:
  LDA agents_x, Y
  .repeat 3
  ASL
  .endrepeat
  STA oam_sprites+Sprite::xcoord, X
  LDA agents_y, Y
  .repeat 3
  ASL
  .endrepeat
  CLC
  ADC #$0f
  STA oam_sprites+Sprite::ycoord, X

  TXA
  PHA

  LDX agents_type, Y
  ; TODO: implement cuca camo here

  LDA tile_per_agent_type, X
  STA temp_tile
  LDA flag_per_agent_type, X
  STA temp_flag
  LDA agents_direction, Y
  CMP #direction::left
  BNE :+
  LDA temp_flag
  ORA #OAM_FLIP_H
  STA temp_flag
:
  PLA
  TAX

  LDA temp_tile
  STA oam_sprites+Sprite::tile, X
  LDA temp_flag
  STA oam_sprites+Sprite::flag, X

  .repeat .sizeof(Sprite)
  INX
  .endrepeat
skip:
  INY
  CPY num_agents
  BNE loop
  STX sprite_counter
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

reroll_up_stairs:
  JSR rand
  AND #%11111
  STA temp_x ; 0..31
  JSR rand
  AND #%1111
  CLC
  ADC #2
  STA temp_y ; 2..17 (0..19 is harder to roll)

  LDY dungeon_levels, X
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

  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BNE reroll_down_stairs

  LDA temp_x
  STA dungeon_down_stairs_x, X
  LDA temp_y
  STA dungeon_down_stairs_y, X

  INX
  CPX #MAX_DUNGEON_LEVELS
  BNE dungeon_level_loop

  ; reset saved agents
  LDX #(MAX_DUNGEON_LEVELS-1)
@loop:
  LDA dungeon_level_agents_ptr_l, X
  STA addr_ptr
  LDA dungeon_level_agents_ptr_h, X
  STA addr_ptr+1
  LDY #0
  LDA #0
  STA (addr_ptr), Y
  DEX
  BPL @loop

  RTS
.endproc

; detects collision between coordinate and level tile
; input: Y  = hardcoded level index
;        temp_x, temp_y = tile coordinates (0..31, 0..19)
; cobbles: Y, temp_acc
; return: flag nonzero if collided with dungeon
.proc dungeon_level_collision
  ; check bounds first
  LDA temp_y
  BPL :+
  LDA #1
  RTS
:
  CMP #20
  BCC :+
  LDA #1
  RTS
:
  ; inside bounds, check map data

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

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

.define game_state_handlers waiting_to_start-1, playing-1, game_over-1

game_state_handlers_l: .lobytes game_state_handlers
game_state_handlers_h: .hibytes game_state_handlers

.define playing_state_handlers action_counter_handler-1, \
                               player_input_handler-1, \
                               agents_input_handler-1, \
                               process_actions_handler-1

playing_state_handlers_l: .lobytes playing_state_handlers
playing_state_handlers_h: .hibytes playing_state_handlers

.define action_handlers move_handler-1, \
                        skill_a_handler-1, \
                        skill_b_handler-1

action_handlers_l: .lobytes action_handlers
action_handlers_h: .hibytes action_handlers

.define agent_ais $0000, \
                  corpo_seco_ai-1, \
                  mula_sem_cabeca_ai-1, \
                  boitata_ai-1, \
                  cuca_ai-1, \
                  mapinguari_ai-1
agent_ais_l: .lobytes agent_ais
agent_ais_h: .hibytes agent_ais

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"
nametable_good_end: .incbin "../assets/nametables/good-end.rle"
nametable_bad_end: .incbin "../assets/nametables/bad-end.rle"

.include "../assets/metasprites.inc"

; delta coordinates per direction
delta_x_lt: .byte 0, 0, -1, 1
delta_y_lt: .byte -1, 1, 0, 0

; masks for more efficient ranged rand
rand_mask:
.byte %00000000 ; <= 0
.byte %00000001 ; <= 1
.repeat 2
.byte %00000011 ; <= 2, 3
.endrepeat
.repeat 4
.byte %00000111 ; <= 4..7
.endrepeat
.repeat 8
.byte %00001111 ; <= 8..15
.endrepeat
.repeat 16
.byte %00011111 ; <= 16..31
.endrepeat
.repeat 32
.byte %00111111 ; <= 32..65
.endrepeat
.repeat 64
.byte %01111111 ; <= 64..127
.endrepeat
.repeat 128
.byte %11111111 ; <= 128..255
.endrepeat

; level to exp look up
lv_to_xp_h_lt:
.repeat 21, i
.byte ((i-1)*(i-1) + 1) / 100
.endrepeat
lv_to_xp_l_lt:
.repeat 21, i
.byte ((i-1)*(i-1) + 1) .mod 100
.endrepeat

; exp for each level
; 000000
; 000010
; 000020
; 000030
; 000050
; 000080
; 000130
; 000210
; 000340
; 000550
; 000890
; 001440
; 002330
; 003770
; 006100
; 009870
; 015970
; 025840
; 041810
; 067650
; 109460
xp_per_level_2:
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 01
.byte 02
.byte 04
.byte 06
.byte 10
xp_per_level_1:
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 00
.byte 01
.byte 02
.byte 03
.byte 05
.byte 08
.byte 14
.byte 23
.byte 37
.byte 61
.byte 98
.byte 59
.byte 58
.byte 18
.byte 76
.byte 94
xp_per_level_0:
.byte 00
.byte 10
.byte 20
.byte 30
.byte 50
.byte 80
.byte 30
.byte 10
.byte 40
.byte 50
.byte 90
.byte 40
.byte 30
.byte 70
.byte 00
.byte 70
.byte 70
.byte 40
.byte 10
.byte 50
.byte 60


; agents stuff
tile_per_agent_type:
  .byte $00 ; saci
  .byte $01 ; corpo seco
  .byte $02 ; mula sem cabeca
  .byte $03 ; boitatá
  .byte $04 ; cuca
  .byte $05 ; mapinguari
flag_per_agent_type:
  .byte $00 ; saci
  .byte $01 ; corpo seco
  .byte $00 ; mula sem cabeca
  .byte $00 ; boitatá
  .byte $01 ; cuca
  .byte $02 ; mapinguari

random_enemy_table:
.incbin "../assets/data/enemy-table.bin"

; default stats per agent
;                   S   c   m   b   C   M
default_str: .byte  1,  1,  1,  2,  4,  5
default_int: .byte  1,  1,  1,  2,  3,  1
default_spd: .byte  1,  1,  2,  2,  2,  3
default_hp:  .byte 10, 10, 15, 20, 30, 50
default_lv:  .byte  1,  3,  3,  5,  8, 12

; strings
string_game_over: .byte "Saci morreu...", $00

; DEBUG
string_action_counter:  .byte "Waiting.", $00
string_player_input:    .byte "Input...", $00
string_agents_input:    .byte "Thinking", $00
string_process_actions: .byte "Acting..", $00

; mask bit for map data
map_mask:
.repeat 8, i
  .byte 1<<(7-i)
.endrepeat

; map data

map_data_ptr_l:
.repeat 16, map_index
  .byte .lobyte(.ident(.concat("map_data_", .sprintf("%03d", map_index + 1))))
.endrepeat

map_data_ptr_h:
.repeat 16, map_index
  .byte .hibyte(.ident(.concat("map_data_", .sprintf("%03d", map_index + 1))))
.endrepeat

.repeat 16, map_index
.ident(.concat("map_data_", .sprintf("%03d", map_index + 1))): .incbin .concat("../assets/maps/", .sprintf("%03d", map_index + 1), ".bin")
.endrepeat

; saved agents slot pointers
dungeon_level_agents_ptr_l:
.repeat 20, dungeon_level
  .byte .lobyte(.ident(.concat("dungeon_level_", .sprintf("%d", dungeon_level + 1) , "_agents")))
.endrepeat
dungeon_level_agents_ptr_h:
.repeat 20, dungeon_level
  .byte .hibyte(.ident(.concat("dungeon_level_", .sprintf("%d", dungeon_level + 1) , "_agents")))
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
