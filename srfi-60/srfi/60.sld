(define-library (srfi 60)
  (export bitwise-and bitwise-ior bitwise-xor bitwise-if
   arithmetic-shift bit-count bit-set? first-set-bit any-bits-set?
   copy-bit integer-length bit-field copy-bit-field rotate-bit-field
   reverse-bit-field integer->list list->integer booleans->integer
   (rename bitwise-and logand) (rename any-bits-set? logtest)
   (rename bitwise-ior logior) (rename bit-count     logcount)
   (rename bitwise-xor logxor) (rename bit-set?      logbit?)
   (rename bit-not bitwise-not)(rename bit-not       lognot)
   (rename bitwise-if bitwise-merge)
   (rename arithmetic-shift ash)
   (rename first-set-bit log2-binary-factors))

  (import (scheme base)
          (scheme case-lambda))

  (include "60/portable-implementation.scm"))
