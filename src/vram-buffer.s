.include "constants.inc"

.export vram_buffer
.export vram_buffer_sp
.export main_sp
.export flush_vram_buffer

.segment "ZEROPAGE"
vram_buffer_sp: .res 1
main_sp: .res 1

.segment "STACK"
vram_buffer: .res 128

.segment "CODE"

.proc flush_vram_buffer
  LDA vram_buffer_sp
  CMP #$7f
  BNE :+
  RTS
:
  TSX
  STX main_sp
  LDX vram_buffer_sp
  TXS

  BIT PPUSTATUS
loop:
  PLA
  BEQ exit_loop
  STA PPUADDR
  PLA
  STA PPUADDR
  PLA
  STA PPUDATA
  JMP loop
exit_loop:
  TSX
  STX vram_buffer_sp
  LDX main_sp
  STX main_sp
  TXS

  RTS
.endproc
