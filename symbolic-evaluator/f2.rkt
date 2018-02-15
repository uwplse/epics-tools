#lang racket

(require (lib "rosette/base/core/term.rkt"))
(require (lib "rosette/base/core/union.rkt"))
(require (only-in (lib "rosette/base/core/bool.rkt") @assert instance-of?))

(define-lifted-type @single-flonum?
  #:base single-flonum?
  #:is-a? (instance-of? single-flonum? @single-flonum?)
  #:methods
  [(define (solvable-default self) #f)
   (define (type-eq?    self u v) (= u v))
   (define (type-equal? self u v) (= u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? single-flonum?) v]
       [(term _ (== self)) v]
       [(union : [g (and (or (? single-flonum?) (term _ (== self))) u)] _ ...)
        (@assert g (thunk (raise-argument-error caller "expected a single-flonum?" v)))
        u]
       [_  (@assert #f (thunk (raise-argument-error caller "expected a single-flonum?" v)))]))])

; (define (fp.isZero x)
;   (match x
;     [(? single-flonum?) (zero? x)]
;     [_ (expression @fp.isZero x)]))

; (define-lifted-operator @fp.isZero fp.isZero)

; (define ($= x y) (expression @= x y))
; (define-lifted-operator @= $= T*->boolean?)

; (define (! x)
;   (match x
;     [(? boolean?) (not x)]
;     [(expression (== @!) y) y]
;     [_ (expression @! x)]))
; (define-lifted-operator @! !)
; (define-lifted-operator @fp.isZero fp.isZero)
