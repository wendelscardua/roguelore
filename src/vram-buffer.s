.include "constants.inc"

.export vram_buffer
.export vram_buffer_sp
.export main_sp
.export flush_vram_buffer

.segment "ZEROPAGE"
vram_buffer_sp: .res 1
main_sp: .res 1
str_ptr: .res 2

.segment "STACK"
vram_buffer: .res 128

.segment "CODE"

.proc flush_vram_buffer
  LDA vram_buffer_sp
  BNE :+
  RTS
:
  TSX
  STX main_sp
  LDX #0
  STX vram_buffer_sp
  DEX
  TXS

  BIT PPUSTATUS
loop:
  PLA
  BEQ exit_loop
  CMP #$80
  BCS string
  STA PPUADDR
  PLA
  STA PPUADDR
  PLA
  STA PPUDATA
  JMP loop
string:
  AND #%00111111
  STA PPUADDR
  PLA
  STA PPUADDR
  PLA
  STA str_ptr
  PLA
  STA str_ptr+1
  LDY #0
strloop:
  LDA (str_ptr), Y
  BEQ exit_loop
  STA PPUDATA
  INY
  JMP strloop
exit_loop:
  LDX main_sp
  TXS

  BIT PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  RTS
.endproc
