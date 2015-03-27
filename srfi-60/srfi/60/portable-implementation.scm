;; portable-implementation.scm -- bitwise arithmetic taken literally
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-60/blob/master/LICENSE

;; Bitwise Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bitwise-and
  (case-lambda
    ((n)      n)
    ((a b)   (bit-and a b))
    (numbers (logical-reduce bit-and numbers -1))))

(define bitwise-ior
  (case-lambda
    ((n)     n)
    ((a b)   (bit-ior a b))
    (numbers (logical-reduce bit-ior numbers 0))))

(define bitwise-xor
  (case-lambda
    ((n)     n)
    ((a b)   (bit-xor a b))
    (numbers (logical-reduce bit-xor numbers 0))))

(define (logical-reduce bit-op numbers default)
  (if (null? numbers)
      default
      (let loop ((result (car numbers))
                 (left (cdr numbers)))
        (if (null? left)
            result
            (loop (bit-op result (car left))
                  (cdr left))))))

(define table-0001
  #u8( 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
       0  1  0  1  0  1  0  1  0  1  0  1  0  1  0  1
       0  0  2  2  0  0  2  2  0  0  2  2  0  0  2  2
       0  1  2  3  0  1  2  3  0  1  2  3  0  1  2  3
       0  0  0  0  4  4  4  4  0  0  0  0  4  4  4  4
       0  1  0  1  4  5  4  5  0  1  0  1  4  5  4  5
       0  0  2  2  4  4  6  6  0  0  2  2  4  4  6  6
       0  1  2  3  4  5  6  7  0  1  2  3  4  5  6  7
       0  0  0  0  0  0  0  0  8  8  8  8  8  8  8  8
       0  1  0  1  0  1  0  1  8  9  8  9  8  9  8  9
       0  0  2  2  0  0  2  2  8  8 10 10  8  8 10 10
       0  1  2  3  0  1  2  3  8  9 10 11  8  9 10 11
       0  0  0  0  4  4  4  4  8  8  8  8 12 12 12 12
       0  1  0  1  4  5  4  5  8  9  8  9 12 13 12 13
       0  0  2  2  4  4  6  6  8  8 10 10 12 12 14 14
       0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15))

(define table-0100
  #u8( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
       0  0  2  2  4  4  6  6  8  8 10 10 12 12 14 14
       0  1  0  1  4  5  4  5  8  9  8  9 12 13 12 13
       0  0  0  0  4  4  4  4  8  8  8  8 12 12 12 12
       0  1  2  3  0  1  2  3  8  9 10 11  8  9 10 11
       0  0  2  2  0  0  2  2  8  8 10 10  8  8 10 10
       0  1  0  1  0  1  0  1  8  9  8  9  8  9  8  9
       0  0  0  0  0  0  0  0  8  8  8  8  8  8  8  8
       0  1  2  3  4  5  6  7  0  1  2  3  4  5  6  7
       0  0  2  2  4  4  6  6  0  0  2  2  4  4  6  6
       0  1  0  1  4  5  4  5  0  1  0  1  4  5  4  5
       0  0  0  0  4  4  4  4  0  0  0  0  4  4  4  4
       0  1  2  3  0  1  2  3  0  1  2  3  0  1  2  3
       0  0  2  2  0  0  2  2  0  0  2  2  0  0  2  2
       0  1  0  1  0  1  0  1  0  1  0  1  0  1  0  1
       0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0))

(define table-0010
  #u8( 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
       1  0  1  0  1  0  1  0  1  0  1  0  1  0  1  0
       2  2  0  0  2  2  0  0  2  2  0  0  2  2  0  0
       3  2  1  0  3  2  1  0  3  2  1  0  3  2  1  0
       4  4  4  4  0  0  0  0  4  4  4  4  0  0  0  0
       5  4  5  4  1  0  1  0  5  4  5  4  1  0  1  0
       6  6  4  4  2  2  0  0  6  6  4  4  2  2  0  0
       7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0
       8  8  8  8  8  8  8  8  0  0  0  0  0  0  0  0
       9  8  9  8  9  8  9  8  1  0  1  0  1  0  1  0
      10 10  8  8 10 10  8  8  2  2  0  0  2  2  0  0
      11 10  9  8 11 10  9  8  3  2  1  0  3  2  1  0
      12 12 12 12  8  8  8  8  4  4  4  4  0  0  0  0
      13 12 13 12  9  8  9  8  5  4  5  4  1  0  1  0
      14 14 12 12 10 10  8  8  6  6  4  4  2  2  0  0
      15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0))

(define table-0111
  #u8( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
       1  1  3  3  5  5  7  7  9  9 11 11 13 13 15 15
       2  3  2  3  6  7  6  7 10 11 10 11 14 15 14 15
       3  3  3  3  7  7  7  7 11 11 11 11 15 15 15 15
       4  5  6  7  4  5  6  7 12 13 14 15 12 13 14 15
       5  5  7  7  5  5  7  7 13 13 15 15 13 13 15 15
       6  7  6  7  6  7  6  7 14 15 14 15 14 15 14 15
       7  7  7  7  7  7  7  7 15 15 15 15 15 15 15 15
       8  9 10 11 12 13 14 15  8  9 10 11 12 13 14 15
       9  9 11 11 13 13 15 15  9  9 11 11 13 13 15 15
      10 11 10 11 14 15 14 15 10 11 10 11 14 15 14 15
      11 11 11 11 15 15 15 15 11 11 11 11 15 15 15 15
      12 13 14 15 12 13 14 15 12 13 14 15 12 13 14 15
      13 13 15 15 13 13 15 15 13 13 15 15 13 13 15 15
      14 15 14 15 14 15 14 15 14 15 14 15 14 15 14 15
      15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15))

(define table-0110
  #u8( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
       1  0  3  2  5  4  7  6  9  8 11 10 13 12 15 14
       2  3  0  1  6  7  4  5 10 11  8  9 14 15 12 13
       3  2  1  0  7  6  5  4 11 10  9  8 15 14 13 12
       4  5  6  7  0  1  2  3 12 13 14 15  8  9 10 11
       5  4  7  6  1  0  3  2 13 12 15 14  9  8 11 10
       6  7  4  5  2  3  0  1 14 15 12 13 10 11  8  9
       7  6  5  4  3  2  1  0 15 14 13 12 11 10  9  8
       8  9 10 11 12 13 14 15  0  1  2  3  4  5  6  7
       9  8 11 10 13 12 15 14  1  0  3  2  5  4  7  6
      10 11  8  9 14 15 12 13  2  3  0  1  6  7  4  5
      11 10  9  8 15 14 13 12  3  2  1  0  7  6  5  4
      12 13 14 15  8  9 10 11  4  5  6  7  0  1  2  3
      13 12 15 14  9  8 11 10  5  4  7  6  1  0  3  2
      14 15 12 13 10 11  8  9  6  7  4  5  2  3  0  1
      15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0))

(define (table-uint-reduce table num1 num2)
  (map-uint 4
    (lambda (nibble1 nibble2)
      (bytevector-u8-ref table (+ (* 16 nibble1) nibble2)))
    num1 num2))

(define (bit-and num1 num2)
  (cond ((and (negative? num1) (negative? num2))
         (bit-not (table-uint-reduce table-0111 (bit-not num1) (bit-not num2))))
        ((negative? num1)
         (table-uint-reduce table-0100 (bit-not num1) num2))
        ((negative? num2)
         (table-uint-reduce table-0100 (bit-not num2) num1))
        (else
         (table-uint-reduce table-0001 num1 num2))))

(define (bit-ior num1 num2)
  (cond ((and (negative? num1) (negative? num2))
         (bit-not (table-uint-reduce table-0001 (bit-not num1) (bit-not num2))))
        ((negative? num1)
         (bit-not (table-uint-reduce table-0010 (bit-not num1) num2)))
        ((negative? num2)
         (bit-not (table-uint-reduce table-0010 (bit-not num2) num1)))
        (else
         (table-uint-reduce table-0111 num1 num2))))

(define (bit-xor num1 num2)
  (cond ((and (negative? num1) (negative? num2))
         (table-uint-reduce table-0110 (bit-not num1) (bit-not num2)))
        ((negative? num1)
         (bit-not (table-uint-reduce table-0110 (bit-not num1) num2)))
        ((negative? num2)
         (bit-not (table-uint-reduce table-0110 num1 (bit-not num2))))
        (else
         (table-uint-reduce table-0110 num1 num2))))

(define (bitwise-not n)
  (bit-not n))

(define (bit-not n)
  (- -1 n))

(define (bitwise-if mask true false)
  (bit-ior (bit-and mask true) (bit-and (bitwise-not mask) false)))

(define (any-bits-set? mask num)
  (not (zero? (bitwise-and mask num))))

;; Integer Properties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define table-bit-count
  #u8(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4
      1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
      1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
      1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
      2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
      3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
      3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
      4 5 5 6 5 6 6 7 5 6 6 7 6 7 7 8))

(define table-integer-length
  #u8(0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4
      5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
      6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8))

(define table-integer-length
  #u8(0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4
      5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
      6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8))

(define table-first-bit
  #u8(0 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      6 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      7 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      6 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      5 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0))

(define (bit-count num)
  (if (negative? num)
      (ubit-count (bit-not num))
      (ubit-count num)))

(define (ubit-count num)
  (fold-uint 8 0
    (lambda (sum byte)
      (+ sum (bytevector-u8-ref table-bit-count byte)))
    num))

(define (integer-length num)
  (if (negative? num)
      (uinteger-length (bit-not num))
      (uinteger-length num)))

(define (uinteger-length num)
  (let loop ((num num) (count 0))
    (if (< num 256)
        (+ count (bytevector-u8-ref table-integer-length num))
        (loop (quotient num 256)
              (+ count 8)))))

(define (first-set-bit num)
  (if (zero? num) -1
      (let loop ((num num) (count 0))
        (if (zero? (modulo num 256))
            (loop (arithmetic-shift num -8)
                  (+ count 8))
            (+ count (bytevector-u8-ref table-first-bit (modulo num 256)))))))

;; Bit Within Word ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nth-bit index) (expt 2 index))

(define (bit-set? index num)
  (any-bits-set? (nth-bit index) num))

(define (copy-bit index num bit)
  (if bit
      (bit-ior num (nth-bit index))
      (bit-and num (bit-not (nth-bit index)))))

;; Field of Bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mask-of count)
  (- (nth-bit count) 1))

(define (mask-at start end)
  (arithmetic-shift (mask-of (- end start)) start))

(define (bit-field num start end)
  (if (<= end start) 0
      (bit-and (arithmetic-shift num (- start))
               (mask-of (- end start)))))

(define (copy-bit-field num-to num-from start end)
  (if (<= end start) num-to
      (bitwise-if
        (mask-at start end)
        (arithmetic-shift num-from start)
        num-to)))

(define (arithmetic-shift num count)
  (if (negative? count)
      (if (negative? num)
          (bit-not (quotient (bit-not num) (nth-bit (- count))))
          (quotient num (nth-bit (- count))))
      (* num (nth-bit count))))

(define (rotate-bit-field num count start end)
  (if (<= end start) num
      (let ((count (modulo count (- end start))))
        (define mask (mask-at start end))
        (define chunk (arithmetic-shift (bit-and num mask) (- start)))
        (bitwise-if mask
          (arithmetic-shift
            (bit-ior (arithmetic-shift chunk count)
                    (arithmetic-shift chunk (- count (- end start))))
            start)
          num))))

(define (reverse-bit-field num start end)
  (if (<= end start) num
      (let ((mask (mask-at start end)))
        (bit-ior
          (arithmetic-shift
            (reverse-bits (- end start)
              (arithmetic-shift (bit-and num mask) (- start)))
            start)
          (bit-and num (bit-not mask))))))

(define (ceiling-quotient p q)
  (quotient (+ p q -1) q))

(define (reverse-bits length num)
  (arithmetic-shift
    (reverse-bytes (ceiling-quotient length 8) num)
    (if (zero? (modulo length 8)) 0
        (- (modulo length 8) 8))))

(define table-reverse
  #u8( 0 128 64 192 32 160  96 224 16 144 80 208 48 176 112 240
       8 136 72 200 40 168 104 232 24 152 88 216 56 184 120 248
       4 132 68 196 36 164 100 228 20 148 84 212 52 180 116 244
      12 140 76 204 44 172 108 236 28 156 92 220 60 188 124 252
       2 130 66 194 34 162  98 226 18 146 82 210 50 178 114 242
      10 138 74 202 42 170 106 234 26 154 90 218 58 186 122 250
       6 134 70 198 38 166 102 230 22 150 86 214 54 182 118 246
      14 142 78 206 46 174 110 238 30 158 94 222 62 190 126 254
       1 129 65 193 33 161  97 225 17 145 81 209 49 177 113 241
       9 137 73 201 41 169 105 233 25 153 89 217 57 185 121 249
       5 133 69 197 37 165 101 229 21 149 85 213 53 181 117 245
      13 141 77 205 45 173 109 237 29 157 93 221 61 189 125 253
       3 131 67 195 35 163  99 227 19 147 83 211 51 179 115 243
      11 139 75 203 43 171 107 235 27 155 91 219 59 187 123 251
       7 135 71 199 39 167 103 231 23 151 87 215 55 183 119 247
      15 143 79 207 47 175 111 239 31 159 95 223 63 191 127 255))

(define (reverse-bytes byte-count num)
  (let loop ((count byte-count) (num num) (result 0))
    (if (zero? count)
        result
        (loop (- count 1)
              (quotient num 256)
              (+ (* result 256)
                 (bytevector-u8-ref table-reverse (modulo num 256)))))))

;; Bits as Booleans ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define integer->list
  (case-lambda
    ((num)        (integer->list* num (integer-length num)))
    ((num length) (integer->list* num length))))

(define (integer->list* num length)
  (let ((bit (if (negative? num) 0 1)))
    (let loop ((num (if (negative? num) (bit-not num) num))
               (length length) (result '()))
        (if (zero? length)
            result
            (loop (quotient num 2)
                  (- length 1)
                  (cons (= bit (modulo num 2)) result))))))

(define (list->integer booleans)
  (fold
    (lambda (boolean num)
      (+ (arithmetic-shift num 1) (if boolean 1 0)))
    0 booleans))

(define (booleans->integer . values)
  (list->integer values))

;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-uint power proc . nums)
  (define chunk (expt 2 power))
  (define (mask n) (modulo n chunk))
  (define (shr n) (quotient n chunk))
  (let loop ((nums nums) (shift 1) (result 0))
    (if (every zero? nums)
        result
        (loop (map shr nums)
              (* chunk shift)
              (+ result (* shift (apply proc (map mask nums))))))))

(define (fold-uint power seed proc . nums)
  (define chunk (expt 2 power))
  (define (mask n) (modulo n chunk))
  (define (shr n) (quotient n chunk))
  (let loop ((nums nums) (seed seed))
    (if (every zero? nums)
        seed
        (loop (map shr nums)
              (apply proc seed (map mask nums))))))
