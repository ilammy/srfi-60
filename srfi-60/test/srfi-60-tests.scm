(import (chibi) (chibi test) (srfi 60))

(test-begin "srfi-60")

(test-group "Bitwise Operations"
  (test 5 (bitwise-and 5))
  (test #b1000 (bitwise-and #b1100 #b1010))
  (test #b0100 (bitwise-and #b0100 #b0110 #b0110 #b0101 #b1101 #b0111))

  (test 9 (bitwise-ior 9))
  (test #b1110 (bitwise-ior #b1100 #b1010))
  (test #b1011 (bitwise-ior #b0001 #b0000 #b1010 #b1001 #b1010 #b1000))

  (test -3 (bitwise-xor -3))
  (test #b0110 (bitwise-xor #b1100 #b1010))
  (test #b1100 (bitwise-xor #b0101 #b1110 #b0001 #b1010 #b0101 #b1001))

  (test  0 (bitwise-not -1))
  (test -1 (bitwise-not  0))
  (test -2 (bitwise-not  1))
  (test (- #b0110011) (bitwise-not #b110010))
  (test #b011001 (bitwise-and #b111111 (bitwise-not #b100110)))
  (test 1278781 (bitwise-not (bitwise-not 1278781)))

  (test #b0101 (bitwise-if #b1111 #b0101 #b1010))
  (test #b1010 (bitwise-if #b0000 #b0101 #b1010))
  (test #b1100 (bitwise-if #b1100 #b1111 #b0000))
  (test #b1011 (bitwise-if #b1010 #b1111 #b0011))
  (test #b11001010 (bitwise-if #b11110000 #b11001100 #b10101010))

  (test #f (any-bits-set? #b0100 #b1011))
  (test #t (any-bits-set? #b0100 #b0111))
  (test #t (any-bits-set? #b0101 #b0111))
  (test #f (any-bits-set? #b0101 #b1010)))

(test-group "Integer Properties"
  (test 0 (bit-count 0))
  (test 1 (bit-count 1))
  (test 1 (bit-count 2))
  (test 2 (bit-count 3))
  (test 7 (bit-count #b1011101101))
  (test 1 (bit-count -3)) ; -3 = #b1...101
  (test 2 (bit-count -4)) ; -4 = #b1...100
  (test 1 (bit-count -5)) ; -5 = #b1...011

  (test 0 (integer-length 0))
  (test 1 (integer-length 1))
  (test 2 (integer-length 2))
  (test 2 (integer-length 3))
  (test 3 (integer-length 4))
  (test 3 (integer-length 5))
  (test 4 (integer-length 8))
  (test 4 (integer-length #b1111))
  (test 3 (integer-length #b0111))
  (test 6 (integer-length -42)) ; -42 = #b1...010110
  (test #t (= (integer-length 178963123) (integer-length -178963123)))

  (test -1 (first-set-bit 0))
  (test 0 (first-set-bit  1))  (test 0 (first-set-bit  -1))
  (test 1 (first-set-bit  2))  (test 1 (first-set-bit  -2))
  (test 0 (first-set-bit  3))  (test 0 (first-set-bit  -3))
  (test 2 (first-set-bit  4))  (test 2 (first-set-bit  -4))
  (test 0 (first-set-bit  5))  (test 0 (first-set-bit  -5))
  (test 1 (first-set-bit  6))  (test 1 (first-set-bit  -6))
  (test 0 (first-set-bit  7))  (test 0 (first-set-bit  -7))
  (test 3 (first-set-bit  8))  (test 3 (first-set-bit  -8))
  (test 0 (first-set-bit  9))  (test 0 (first-set-bit  -9))
  (test 1 (first-set-bit 10))  (test 1 (first-set-bit -10))
  (test 0 (first-set-bit 11))  (test 0 (first-set-bit -11))
  (test 2 (first-set-bit 12))  (test 2 (first-set-bit -12))
  (test 0 (first-set-bit 13))  (test 0 (first-set-bit -13))
  (test 1 (first-set-bit 14))  (test 1 (first-set-bit -14))
  (test 0 (first-set-bit 15))  (test 0 (first-set-bit -15))
  (test 4 (first-set-bit 16))  (test 4 (first-set-bit -16)))

(test-group "Bit Within Word"
  (test #t (bit-set? 0 #b1101))
  (test #f (bit-set? 1 #b1101))
  (test #t (bit-set? 2 #b1101))
  (test #t (bit-set? 3 #b1101))
  (test #f (bit-set? 4 #b1101))

  (test #f (bit-set? 0 -6)) ; -6 = #b1...010
  (test #t (bit-set? 1 -6))
  (test #f (bit-set? 2 -6))
  (test #t (bit-set? 3 -6))
  (test #t (bit-set? 4 -6))

  (test #f (bit-set? 0 0))
  (test #f (bit-set? 1 0))
  (test #f (bit-set? 2 0))

  (test #b0001 (copy-bit 0 #b0000 #t))
  (test #b0100 (copy-bit 2 #b0000 #t))
  (test #b1011 (copy-bit 2 #b1111 #f))
  (test #b0000 (copy-bit 0 #b0000 #f))
  (test #b1101 (copy-bit 3 #b1101 #t))
  (test (bitwise-not #b1101) (copy-bit 2 (bitwise-not #b1001) #f)))

(test-group "Field of Bits"
  (test #b01010 (bit-field #b1101101010 0 4))
  (test #b00101 (bit-field #b1101101010 1 5))
  (test #b11010 (bit-field #b1101101010 2 7))
  (test #b01101 (bit-field #b1101101010 3 8))
  (test #b10110 (bit-field #b1101101010 4 9))
  (test #b00000 (bit-field #b1101101010 5 5))

  (test #b000001101100000 (copy-bit-field #b000001101101010  0 0 4))
  (test #b000001101101111 (copy-bit-field #b000001101101010 -1 0 4))
  (test #b110100111110000 (copy-bit-field #b110100100010000 -1 5 9))
  (test #b1101000 (copy-bit-field 0 #b11101101 3 7))
  (test #b1100100 (copy-bit-field #b1100000 #b11101101 2 4))
  (test #b1110110101 (copy-bit-field #b1 #b11101101 2 10))

  (test #b1000 (arithmetic-shift #b0001  3))
  (test #b1010 (arithmetic-shift #b1010  0))
  (test #b0101 (arithmetic-shift #b1010 -1))
  (test #b0000 (arithmetic-shift #b0011 -2))
  (test (bitwise-not #b0000) (arithmetic-shift (bitwise-not #b0011) -2))
  (test (bitwise-not #b101111) (arithmetic-shift (bitwise-not #b1011) 2))

  (test #b10 (rotate-bit-field #b0100  3 0 4))
  (test #b10 (rotate-bit-field #b0100 -1 0 4))
  (test #b110100010010000 (rotate-bit-field #b110100100010000 -1 5 9))
  (test #b110100000110000 (rotate-bit-field #b110100100010000  1 5 9))
  (test #b11110110 (rotate-bit-field #b11101101 -1 0 8))
  (test #b11101101 (rotate-bit-field #b11101101  0 0 8))
  (test #b11011011 (rotate-bit-field #b11101101  1 0 8))
  (test #b10110111 (rotate-bit-field #b11101101  2 0 8))
  (test #b01101111 (rotate-bit-field #b11101101  3 0 8))
  (test #b11011110 (rotate-bit-field #b11101101  4 0 8))
  (test #b10111101 (rotate-bit-field #b11101101  5 0 8))
  (test #b11011101 (rotate-bit-field #b11101101  6 3 8))
  (test #b11011011 (rotate-bit-field #b11101101 65 0 8))
  (test #b11101101 (rotate-bit-field #b11101101  4 1 1))

  (test #xe5 (reverse-bit-field #xa7 0 8))
  (test #b10110111 (reverse-bit-field #b11101101 0 8))
  (test #b11101101 (reverse-bit-field #b11101101 3 3))
  (test #b11101011 (reverse-bit-field #b11101101 0 4))
  (test #b00000000 (reverse-bit-field #b00000000 3 6))
  (test #b11111101 (reverse-bit-field #b11111101 0 1)))

(test-group "Bits as Booleans"
  (test '(#t #t #f #t #f #t #t #f #t) (integer->list #b110101101))
  (test '()      (integer->list 0))
  (test '(#t)    (integer->list 1))
  (test '(#t #f) (integer->list 2))
  (test '(#t #t) (integer->list 3))
  (test '(#t #f #t #f #t #f #f) (integer->list 84))
  (test                     '() (integer->list 84 0))
  (test             '(#t #f #f) (integer->list 84 3))
  (test    '(#f #t #f #t #f #f) (integer->list 84 6))
  (test '(#f #f #f #f #f
          #t #f #t #f #t #f #f) (integer->list 84 12))

  (test #b110101101 (list->integer '(#t #t #f #t #f #t #t #f #t)))
  (test 0 (list->integer '()))
  (test 1 (list->integer '(#t)))
  (test 2 (list->integer '(#t #f)))
  (test 3 (list->integer '(#t #t)))
  (test 84 (list->integer                '(#t #f #t #f #t #f #f)))
  (test 84 (list->integer '(#f #f #f #f #f #t #f #t #f #t #f #f)))

  (test #b110101101 (booleans->integer #t #t #f #t #f #t #t #f #t))
  (test 0 (booleans->integer))
  (test 1 (booleans->integer #t))
  (test 2 (booleans->integer #t #f))
  (test 3 (booleans->integer #t #t))
  (test 84 (booleans->integer                #t #f #t #f #t #f #f))
  (test 84 (booleans->integer #f #f #f #f #f #t #f #t #f #t #f #f)))

;; It would be too stupid to test these as real procedures
(test-group "Aliases"
  (test-assert (eqv? bitwise-and logand))
  (test-assert (eqv? bitwise-ior logior))
  (test-assert (eqv? bitwise-xor logxor))
  (test-assert (eqv? bitwise-not lognot))
  (test-assert (eqv? first-set-bit log2-binary-factors))
  (test-assert (eqv? any-bits-set? logtest))
  (test-assert (eqv? bitwise-if bitwise-merge))
  (test-assert (eqv? bit-count logcount))
  (test-assert (eqv? bit-set? logbit?))
  (test-assert (eqv? arithmetic-shift ash)))

(test-end)

(test-exit)
