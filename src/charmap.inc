.repeat 27, i
  .charmap $40 + i, $20 + i
  .charmap $60 + i, $40 + i
.endrepeat

.charmap $20, $1a

.repeat 10
  .charmap $30 + i, $10 + i
.endrepeat