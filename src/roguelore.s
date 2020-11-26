.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"
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
agents_hp: .res MAX_AGENTS
agents_max_hp: .res MAX_AGENTS
agents_aux: .res MAX_AGENTS
agents_action_counter: .res MAX_AGENTS
num_agents: .res 1

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

  LDA #0
  STA current_dungeon_level
  JSR draw_current_dungeon_level

  LDA #agent_type::saci
  STA agents_type
  LDA dungeon_up_stairs_x
  STA agents_x
  LDA dungeon_up_stairs_y
  STA agents_y
  LDA #1
  STA agents_str
  STA agents_int
  STA agents_spd
  LDA #10
  STA agents_max_hp
  STA agents_hp
  LDA #direction::right
  STA agents_direction
  LDA #5
  STA agents_action_counter
  LDA #1
  STA num_agents

; debug enemies
  LDX #1
  LDA #agent_type::cuca
  STA agents_type, X
  LDA dungeon_down_stairs_x
  STA agents_x, X
  LDA dungeon_down_stairs_y
  STA agents_y, X
  LDA #1
  STA agents_str, X
  STA agents_int, X
  STA agents_spd, X
  LDA #10
  STA agents_max_hp, X
  STA agents_hp, X
  LDA #direction::right
  STA agents_direction, X
  LDA #5
  STA agents_action_counter, X
  LDA #2
  STA num_agents



  LDA #playing_state::action_counter
  STA current_playing_state

  LDA #0
  STA action_queue_head
  STA action_queue_tail

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
  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BEQ :+
  LDA #-$10
:
  CLC
  ADC #$72
  STA PPUDATA

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
  JSR playing_state_handler
  JSR render_stuff
  RTS
.endproc

.proc render_stuff
  LDX #0
  STX sprite_counter
  ; TODO render projectiles
  JSR render_agents
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

  LDA #playing_state::agents_input
  CPX #0
  BNE :+
  LDA #playing_state::player_input
:
  STA current_playing_state
next:
  DEX
  BPL loop
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
  ADC #5
  STA agents_action_counter

  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  ENQUEUE_ACTION #0
  ENQUEUE_ACTION #action_type::skill_a
  LDA #playing_state::agents_input
  STA current_playing_state
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  ENQUEUE_ACTION #0
  ENQUEUE_ACTION #action_type::skill_b
  LDA #playing_state::agents_input
  STA current_playing_state
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
  LDA #playing_state::agents_input
  STA current_playing_state
  
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
  ADC #$5
  STA agents_action_counter, X

  JSR get_input_for_agent

@next:
  INX
  JMP @loop
@exit:

  LDA #playing_state::process_actions
  STA current_playing_state
  RTS
.endproc

; enqueue action for current (X-indexed) agent if able
; (must preserve X)
; cobbles Y, temp_*
.proc get_input_for_agent
  ; TODO better "ai", different for each type
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

  TXA
  PHA

  LDX current_dungeon_level
  LDY dungeon_levels, X
  JSR dungeon_level_collision
  BEQ no_collision
  PLA
  TAX
  JMP get_input_for_agent
no_collision:
  PLA
  TAX

  ENQUEUE_ACTION X
  ENQUEUE_ACTION #action_type::move

  RTS
.endproc

; deletes dead agents
.proc garbage_collector
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

  LDA agents_type, Y
  STA agents_type, X
  LDA agents_x, Y
  STA agents_x, X
  LDA agents_y, Y
  STA agents_y, X
  LDA agents_str, Y
  STA agents_str, X
  LDA agents_int, Y
  STA agents_int, X
  LDA agents_spd, Y
  STA agents_spd, X
  LDA agents_direction, Y
  STA agents_direction, X
  LDA agents_hp, Y
  STA agents_hp, X
  LDA agents_max_hp, Y
  STA agents_max_hp, X
  LDA agents_aux, Y
  STA agents_aux, X
  LDA agents_action_counter, Y
  STA agents_action_counter, X
  DEY
  STY num_agents
  JMP @loop
@next:
  INX
  JMP @loop
@exit:

  RTS
.endproc

.proc process_actions_handler
  LDX action_queue_head
  CPX action_queue_tail
  BNE :+
  ; no actions available
  JSR garbage_collector
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

  LDA temp_x
  STA agents_x, Y
  LDA temp_y
  STA agents_y, Y

  RTS
.endproc

.proc skill_a_handler
  ; TODO
  RTS
.endproc

.proc skill_b_handler
  ; TODO
  RTS
.endproc

; agent Y causes melee damage to agent X
.proc melee_attack
  ; damage = 1d6 + strength
  LDA agents_str, Y
  STA temp_acc
  JSR roll_d6 ; cobbles Y
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
  ADC #$10
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

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"

.include "../assets/metasprites.inc"

; delta coordinates per direction
delta_x_lt: .byte 0, 0, -1, 1
delta_y_lt: .byte -1, 1, 0, 0

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
