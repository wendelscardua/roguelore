.segment "ZEROPAGE"
.import vram_buffer_sp
.import main_sp

.segment "STACK"
.import vram_buffer

.segment "RODATA"
.import _tens
.import _ones

.segment "CODE"
.import flush_vram_buffer

.macro switch_to_vram_stack
  TSX
  STX main_sp
  LDX #0
  TXS
.endmacro

.macro switch_to_main_stack
  LDX main_sp
  TXS
.endmacro

.macro reset_vram_stack
  .local reset_loop
  PHA
  TXA
  PHA
  LDA #0
  STA vram_buffer_sp
  STA vram_buffer
  PLA
  TAX
  PLA
.endmacro

; cobbles X
.macro write_tile_to_vram ppu_addr, tile
.local wait_nmi
.local no_flush
  LDX vram_buffer_sp
  CPX #($7f - 5)
  BCC no_flush
  LDA old_nmis
wait_nmi:
  CMP nmis
  BEQ wait_nmi
no_flush:
  LDA #.hibyte(ppu_addr)
  STA vram_buffer, X
  INX
  LDA #.lobyte(ppu_addr)
  STA vram_buffer, X
  INX
  LDA tile
  STA vram_buffer, X
  INX
  LDA #$00 ; trailer tile
  STA vram_buffer, X
  INX
  STA vram_buffer, X ; end of stack marker
  STX vram_buffer_sp
.endmacro

; input: ppuaddr and 2-digit decimal
; cobbles X, Y, A
.macro write_decimal_to_vram ppu_addr, decimal
.local wait_nmi
.local no_flush
  LDX vram_buffer_sp
  CPX #($7f - 5)
  BCC no_flush
  LDA old_nmis
wait_nmi:
  CMP nmis
  BEQ wait_nmi
no_flush:
  LDA #.hibyte(ppu_addr)
  STA vram_buffer, X
  INX
  LDA #.lobyte(ppu_addr)
  STA vram_buffer, X
  INX
  LDA decimal
  TAY
  LDA _tens, Y  
  STA vram_buffer, X
  INX
  LDA _ones, Y
  STA vram_buffer, X
  INX
  LDA #$00
  STA vram_buffer, X
  INX
  STA vram_buffer, X
  STX vram_buffer_sp
.endmacro

.macro write_string_to_vram ppu_addr, str_ptr
  .local no_flush
  .local wait_nmi
  PHA
  TXA
  PHA
  LDX vram_buffer_sp
  CPX #($7f - 5)
  BCC no_flush
  LDA old_nmis
wait_nmi:
  CMP nmis
  BEQ wait_nmi
no_flush:
  LDA #.hibyte(ppu_addr)
  ORA #$80
  STA vram_buffer, X
  INX
  LDA #.lobyte(ppu_addr)
  STA vram_buffer, X
  INX
  LDA #.lobyte(str_ptr)
  STA vram_buffer, X
  INX
  LDA #.hibyte(str_ptr)
  STA vram_buffer, X
  INX  
  LDA #$00
  STA vram_buffer, X
  STX vram_buffer_sp
  PLA
  TAX
  PLA
.endmacro
