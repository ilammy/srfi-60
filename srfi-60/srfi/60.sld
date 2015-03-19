(define-library (srfi 60)
  (export logand logior logxor lognot logtest logcount logbit?
   bitwise-and bitwise-ior bitwise-xor bitwise-not bitwise-if
   bitwise-merge ash arithmetic-shift bit-count bit-set? copy-bit
   any-bits-set? log2-binary-factors first-set-bit integer-length
   bit-field copy-bit-field rotate-bit-field reverse-bit-field
   integer->list list->integer booleans->integer)

  (import (scheme base))

  (include "60/reference-implementation.scm"))
