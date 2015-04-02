;; portable-implementation.scm -- bitwise arithmetic taken literally
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-60/blob/master/LICENSE

;; Bitwise Operations (monadic) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bit-not n)
  (- -1 n))

;; Primitive bit shifts and masks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nth-bit index) (expt 2 index))

(define (ashl num count) (* num (nth-bit count)))

(define (ashr/+ num count) (quotient num (nth-bit count)))
(define (ashr/- num count) (bit-not (ashr/+ (bit-not num) count)))
(define (ashr num count)
  (if (negative? num) (ashr/- num count) (ashr/+ num count)))

(define (mask num length) (modulo num (nth-bit length)))
(define (~mask num length) (- (nth-bit length) 1 (modulo num (nth-bit length))))

;; Bitwise Operations (variadic) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Bitwise Operations (dyadic);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (table-uint-reduce table num1 num2)
  (let loop ((num1 num1) (num2 num2) (shift 1) (result 0))
    (if (and (zero? num1) (zero? num2))
        result
        (loop (ashr/+ num1 4) (ashr/+ num2 4) (ashl shift 4)
              (+ result
                 (* shift
                    (nibble-ref table (mask num1 4) (mask num2 4))))))))

; table-uint-reduce must be applied to non-negative integers, so each
; operation using it should check whether its arguments are negative and
; use appropriate identities to get proper value from negated arguments.
(define-syntax sign-cond
  (syntax-rules (++ +- -+ --)
    ((_ (num1 num2)
        (++ expr++ ...) (-+ expr-+ ...) (+- expr+- ...) (-- expr-- ...))
     (cond ((and (negative? num1) (negative? num2)) expr-- ...)
           ((negative? num1)                        expr-+ ...)
           ((negative? num2)                        expr+- ...)
           (else                                    expr++ ...)))))

; A ∧ B = ¬A ↚ B = A ↛ ¬B = ¬(¬A ∨ ¬B)
(define (bit-and num1 num2)
  (sign-cond (num1 num2)
    (++          (table-uint-reduce table-0001          num1           num2))
    (-+          (table-uint-reduce table-0100 (bit-not num1)          num2))
    (+-          (table-uint-reduce table-0010          num1  (bit-not num2)))
    (-- (bit-not (table-uint-reduce table-0111 (bit-not num1) (bit-not num2))))))

; A ∨ B = ¬(¬A ↛ B) = ¬(A ↚ ¬B) = ¬(¬A ∧ ¬B)
(define (bit-ior num1 num2)
  (sign-cond (num1 num2)
    (++          (table-uint-reduce table-0111          num1           num2))
    (-+ (bit-not (table-uint-reduce table-0010 (bit-not num1)          num2)))
    (+- (bit-not (table-uint-reduce table-0100          num1  (bit-not num2))))
    (-- (bit-not (table-uint-reduce table-0001 (bit-not num1) (bit-not num2))))))

; A ↮ B = ¬(¬A ↮ B) = ¬(A ↮ ¬B) = (¬A ↮ ¬B)
(define (bit-xor num1 num2)
  (sign-cond (num1 num2)
    (++          (table-uint-reduce table-0110          num1           num2))
    (-+ (bit-not (table-uint-reduce table-0110 (bit-not num1)          num2)))
    (+- (bit-not (table-uint-reduce table-0110          num1  (bit-not num2))))
    (--          (table-uint-reduce table-0110 (bit-not num1) (bit-not num2)))))

; (not (zero? (bit-and mask num))), but we can do it without temporary values
(define (any-bits-set? mask num)
  (sign-cond (mask num)
    (++ (any-bits-set/uint? table-0001          mask           num))
    (-+ (any-bits-set/uint? table-0100 (bit-not mask)          num))
    (+- (any-bits-set/uint? table-0010          mask  (bit-not num)))
    (-- (any-bits-set/uint? table-0111 (bit-not mask) (bit-not num)))))

(define (any-bits-set/uint? table num1 num2)
  (let loop ((num1 num1) (num2 num2))
    (if (and (zero? num1) (zero? num2))
        #f
        (if (zero? (nibble-ref table (mask num1 4) (mask num2 4)))
            (loop (ashr/+ num1 4) (ashr/+ num2 4))
            #t))))

;; Bitwise Operations (triadic) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Obvious inversion to have non-negative mask
(define (bitwise-if mask true false)
  (if (negative? mask)
      (bitwise-if/uint-mask (bit-not mask) false true)
      (bitwise-if/uint-mask          mask  true false)))

; if(M, A, B) = (M ∧ A) ∨ (¬M ∧ B)
;             = (M ∧ A) + (¬M ∧ B) because (M ∧ A) ∧ (¬M ∧ B) = 0
;
;     (M ∧  A) ∨ (¬M ∧  B)
; =   (M ↛ ¬A) ∨ (¬M ∧  B)
; = ¬((M ↛  A) ∨ (¬M ∧ ¬B))
; = ¬((M ∧ ¬A) ∨ (¬M ∧ ¬B))
(define (bitwise-if/uint-mask mask true false)
  (sign-cond (true false)
    (++          (table-uint-reduce/if table-0001 table-0001 mask          true           false))
    (-+          (table-uint-reduce/if table-0010 table-0001 mask (bit-not true)          false))
    (+- (bit-not (table-uint-reduce/if table-0010 table-0001 mask          true  (bit-not false))))
    (-- (bit-not (table-uint-reduce/if table-0001 table-0001 mask (bit-not true) (bit-not false))))))

(define (table-uint-reduce/if table-t table-f bit-mask true false)
  (let loop ((bit-mask bit-mask) (true true) (false false) (shift 1) (result 0))
    (if (and (zero? true) (zero? false))
        result
        (loop (ashr/+ bit-mask 4) (ashr/+ true 4) (ashr/+ false 4) (ashl shift 4)
              (+ result
                 (* shift
                    (+ (nibble-ref table-t  (mask bit-mask 4) (mask true 4))
                       (nibble-ref table-f (~mask bit-mask 4) (mask false 4)))))))))

;; Integer Properties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bit-count num)
  (if (negative? num)
      (ubit-count (bit-not num))
      (ubit-count num)))

(define (ubit-count num)
  (let loop ((num num) (sum 0))
    (if (zero? num)
        sum
        (loop (ashr/+ num 8)
              (+ sum (byte-ref table-bit-count (mask num 8)))))))

(define (integer-length num)
  (if (negative? num)
      (uinteger-length (bit-not num))
      (uinteger-length num)))

(define (uinteger-length num)
  (let loop ((num num) (count 0))
    (if (< num 256)
        (+ count (byte-ref table-integer-length num))
        (loop (ashr/+ num 8)
              (+ count 8)))))

(define (first-set-bit num)
  (if (zero? num) -1
      (let loop ((num num) (count 0))
        (if (zero? (mask num 8))
            (loop (ashr num 8) (+ count 8))
            (+ count (byte-ref table-first-bit (mask num 8)))))))

;; Bit Within Word ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In two's complement even numbers always have zero as their lowest bit
(define (bit-set? index num)
  (= 1 (mask (ashr num index) 1)))

(define (xor p q) (or (and p q) (not (or p q))))

; Quickly set unset bit with +, unset set bit with -
(define (copy-bit index num bit)
  (if (xor bit (bit-set? index num))
      num
      (if bit
          (+ num (nth-bit index))
          (- num (nth-bit index)))))

;; Field of Bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax let-checked
  (syntax-rules ()
    ((_ (length (start end) default) body ...)
     (let ((length (- end start)))
       (if (<= length 0)
           default
           (begin body ...))))))

; Shift right and chop the chunk we want
(define (bit-field num start end)
  (let-checked (length (start end) 0)
    (if (negative? num)
        (~mask (ashr/+ (bit-not num) start) length)
        (mask (ashr/+ num start) length))))

; Concat num-to/after-end $ num-from/interesting-field $ num-to/before-start
(define (copy-bit-field num-to num-from start end)
  (let-checked (length (start end) num-to)
    (if (negative? num-to)
        (+ (ashl (ashr/- num-to end) end)
           (ashl (bit-field num-from 0 length) start)
           (~mask (bit-not num-to) start))
        (+ (ashl (ashr/+ num-to end) end)
           (ashl (bit-field num-from 0 length) start)
           (mask num-to start)))))

(define (arithmetic-shift num count)
  (if (negative? count)
      (ashr num (- count))
      (ashl num count)))

; Extract the chunk, rotate, place it back
(define (rotate-bit-field num count start end)
  (let-checked (length (start end) num)
    (let ((count (modulo count length)))
      (copy-bit-field num
        (rotate-bits (bit-field num start end) length count)
        start end))))

; Add two overflowing non-intersecting parts, mask out garbage
(define (rotate-bits num length count)
  (if (negative? count)
      (mask (+ (ashl num (- length count)) (ashr/+ num count)) length)
      (mask (+ (ashl num count) (ashr/+ num (- length count))) length)))

; Extract the chunk, reverse, place it back
(define (reverse-bit-field num start end)
  (let-checked (length (start end) num)
    (copy-bit-field num
      (reverse-bits length (bit-field num start end))
      start end)))

(define (ceiling-quotient p q) (quotient (+ p q -1) q))
(define (ceiling-modulo p q) (- (modulo (- p 1) q) q -1))

; First reverse whole bytes and their order, then trim excess bits
(define (reverse-bits length num)
  (arithmetic-shift
    (reverse-bytes (ceiling-quotient length 8) num)
    (ceiling-modulo length 8)))

(define (reverse-bytes byte-count num)
  (let loop ((count byte-count) (num num) (result 0))
    (if (zero? count)
        result
        (loop (- count 1)
              (ashr/+ num 8)
              (+ (ashl result 8)
                 (byte-ref table-reverse (mask num 8)))))))

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
            (loop (ashr/+ num 1)
                  (- length 1)
                  (cons (= bit (mask num 1)) result))))))

(define (list->integer booleans)
  (let loop ((booleans booleans) (result 0) (buffer 0) (buffer-bits 0))
    (if (null? booleans) ; are we done?
        (+ (ashl result buffer-bits) buffer)
        (if (= buffer-bits 16) ; is the buffer full?
            (loop booleans (+ (ashl result 16) buffer)
                  0 0)
            (loop (cdr booleans) result
                  (+ (ashl buffer 1) (if (car booleans) 1 0))
                  (+ 1 buffer-bits))))))

(define (booleans->integer . values)
  (list->integer values))

;; Bit operations lookup tables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (byte-ref table byte)
  (bytevector-u8-ref table byte))

(define (nibble-ref table high-nibble low-nibble)
  (bytevector-u8-ref table (+ (ashl high-nibble 4) low-nibble)))

;; Conjunction
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

;; Converse nonimplication
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

;; Material nonimplication
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

;; Disjuction
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

;; Exclusive disjunction
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

;; Number of set bits in a byte
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

;; Index of the leftmost set bit + 1
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

;; Index of the rightmost set bit
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

;; Reversed bits in a byte
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
