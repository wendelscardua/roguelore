.repeat 27, i
  .charmap $40 + i, $20 + i
  .charmap $60 + i, $40 + i
.endrepeat

.charmap $20, $1a
.charmap $2e, $0e

.repeat 10, i
  .charmap $30 + i, $10 + i
.endrepeat