#lang rosette/safe
(provide (all-defined-out))

; Floating-point arithmetic for Rosette

; These bit-blasted definitions were lifted from Z3's FPA->BV converter:
; https://github.com/Z3Prover/z3/blob/be424d9cbbf1d9d0a212ca744c15199291945189/src/ast/fpa/fpa2bv_converter.cpp
; Other resources:
;  - http://www.spark-2014.org/uploads/smt-lib-fp.pdf
;  - http://smtlib.cs.uiowa.edu/theories-FloatingPoint.shtml
;  - https://github.com/xesscorp/Floating_Point_Library-JHU

; These functions require "ex" and "si" arguments, which correspond to the
; number of bits in the exponent and significand (including the hidden bit),
; respectively.

; Internal float layout:
; [1 bit]      [ex bits]      [si-1 bits]
;  ^ sign       ^ exponent     ^ mantissa magnitude
; (0 for +)

; TODO list:
;   fp2bv ex si e width       |
;   bv2fp width e ex si       |
;   fpabs ex si e             | X
;   fpeq ex si e1 e2          | X
;   fpneg ex si e             |
;   fpadd ex si e1 e2         |
;   fpsub ex si e1 e2         | X
;   fpmul ex si e1 e2         |
;   fpdiv ex si e1 e2         |
;   fpnan ex si               | X
;   fpzero ex si              | X
;   fpone ex si               | X
;   fpiszero ex si e          | X
;   fpfloor ex si e           |
;   fpceil ex si e            |
;   fplt ex si e1 e2          | X
;   fple ex si e1 e2          | X
;   fpgt ex si e1 e2          | X
;   fpge ex si e1 e2          | X

; -----------------------------------------------------------------------------
; CREATION

; Create a float literal from a Racket number
(define (mk-float val) 0)

; Create a double literal from a Racket number
(define (mk-double val) 0)

; -----------------------------------------------------------------------------
; INTERNAL HELPERS AND CONSTANTS

; Returns a bitvector of (ex) bits corresponding to the exponent
(define (fp-exponent ex si f)
  (extract (- (+ ex si) 2) (- si 1) f))

; Returns a bitvector of (si-1) bits corresponding to the mantissa
(define (fp-mantissa ex si f)
  (extract (- si 2) 0 f))

; Boolean indicating positive (true) or negative (false)
(define (fp-sign ex si f)
  (let ([n (- (+ ex si) 1)])
    (equal? (bv 0 1) (extract n n f))))

; Reduction-or: are any bits in bv 1?
(define (bvredor w bv)
  (not (equal? bv (bv 0 w))))

; Reduction-and: are all bits in bv 1?
(define (bvredand w bv)
  (equal? bv (bv -1 w)))

; all 1s
(define (fp-top ex) (bv -1 ex))

; all 0s
(define (fp-bot ex) (bv 0 ex))

; 1000...
(define (fp-min-exp ex) (concat (bv 1 1) (bv 0 (- ex 1))))

; 0111...
(define (fp-max-exp ex) (bvnot (fp-min-exp ex)))

; -----------------------------------------------------------------------------
; USEFUL CONSTANTS

; alias for +0
(define (fp-zero ex si)
  (fp-pzero ex si))

; +0
(define (fp-pzero ex si)
  ; 0 0000... 0000...
  (bv 0 (+ ex si)))

; -0
(define (fp-nzero ex si)
  ; 1 0000... 0000...
  (concat (bv 1 1) (bv 0 (- (+ ex si) 1))))

(define (fp-nan ex si)
  ; 0 1111... 1111...
  (concat (bv 0 1) (fp-top ex) (bv 1 (- si 1))))

(define (fp-one ex si)
  ; 0 0111... 0000...
  (concat (bv 0 1) (bv 0 1) (fp-top (- ex 1)) (bv 1 (- si 1))))

; -----------------------------------------------------------------------------
; BASIC TESTS

; is f +0 or -0?
; sign bit doesn't matter, exp and mantissa must both be zero
(define (fp-is-zero ex si f)
  (equal? (extract (- (+ ex si) 2) 0 f) (bv 0 (- (+ ex si) 1))))

; is f NaN?
; exp == 1^n, sig != 0
(define (fp-is-nan ex si f)
  (and
    (equal? (fp-top ex) (fp-exponent ex si f))
    (not (equal? (bv 0 (- si 1)) (fp-mantissa ex si f)))))

; -----------------------------------------------------------------------------
; COMPARISONS

; float equality (NOTE: not the same as equal?, see e.g. +/-0)
(define (fp-eq ex si f1 f2)
  (if (or (fp-is-nan ex si f1) (fp-is-nan ex si f2))
    #f
    (if (and (fp-is-zero ex si f1) (fp-is-zero ex si f2)) #t
      (if (not (equal? (fp-sign ex si f1) (fp-sign ex si f2))) #f
        (and
          (equal? (fp-exponent ex si f1) (fp-exponent ex si f2))
          (equal? (fp-mantissa ex si f1) (fp-mantissa ex si f2)))))))

; float less-than
(define (fp-lt ex si f1 f2)
  (cond
    [(or (fp-is-nan ex si f1) (fp-is-nan ex si f2))    #f]
    [(and (fp-is-zero ex si f1) (fp-is-zero ex si f2)) #f]

    ; f1 >= +0
    [(fp-sign ex si f1)
      (and
        (fp-sign ex si f2) ; f1 >= +0
        (or
          (bvult (fp-exponent ex si f1) (fp-exponent ex si f2)) ; f1 exp < f2 exp
          (and                                                  ; f1 exp = f2 exp and f1 mantissa < f2 mantissa
            (equal? (fp-exponent ex si f1) (fp-exponent ex si f2))
            (bvult (fp-mantissa ex si f1) (fp-mantissa ex si f2)))))]

    ; f1 <= -0 (comparisons reversed)
    [#t
      (or
        (fp-sign ex si f2) ; f1 >= +0
        (bvult (fp-exponent ex si f2) (fp-exponent ex si f1)) ; f2 exp < f1 exp
        (and                                                  ; f2 exp = f1 exp and f2 mantissa < f1 mantissa
          (equal? (fp-exponent ex si f2) (fp-exponent ex si f1))
          (bvult (fp-mantissa ex si f2) (fp-mantissa ex si f1))))]))

; float greater-than
(define (fp-gt ex si f1 f2)
  (if (or (fp-is-nan ex si f1) (fp-is-nan ex si f2))
    #f
    (if (and (fp-is-zero ex si f1) (fp-is-zero ex si f2)) #f
      (not (fp-le ex si f1 f2)))))

; other comparisons
(define (fp-le ex si f1 f2)
  (or
    (fp-eq ex si f1 f2)
    (fp-lt ex si f1 f2)))
(define (fp-ge ex si f1 f2)
  (or
    (fp-eq ex si f1 f2)
    (fp-gt ex si f1 f2)))
(define (fp-ne ex si f1 f2)
  (or
    (fp-lt ex si f1 f2)
    (fp-gt ex si f1 f2)))

; -----------------------------------------------------------------------------
; ARITHMETIC
; (using round-nearest-even rounding mode)

(define (fp-neg ex si f)
  (cond
    [(fp-is-nan ex si f) f]
    [#t (fp-sub ex si (fp-zero ex si) f)]))

(define (fp-abs ex si f)
  (cond
    [(fp-is-nan ex si f) f]
    [(fp-lt ex si f (fp-zero ex si)) (fp-neg ex si f)]
    [#t f]))

(define (fp-sub ex si x y)
  ; x - y = x + (-y), even for floats!
  (fp-add ex si x (fp-neg ex si y)))
