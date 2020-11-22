.segment "ZEROPAGE"
.import vram_buffer_sp
.import main_sp

.segment "CODE"
.import flush_vram_buffer

.macro switch_to_vram_stack
  TSX
  STX main_sp
  LDX vram_buffer_sp
  TXS
.endmacro

.macro switch_to_main_stack
  TSX
  STX vram_buffer_sp
  LDX main_sp
  TXS
.endmacro

.macro reset_vram_stack
  TSX
  STX main_sp  
  LDX #$7f
  STX vram_buffer_sp
  TXS
  LDA #0
  PHA
  LDX main_sp
  TXS
.endmacro

.macro write_tile_to_vram ppu_addr, tile
  LDA #tile
  PHA
  LDA #.lobyte(ppu_addr)
  PHA
  LDA #.hibyte(ppu_addr)
  PHA
.endmacro
