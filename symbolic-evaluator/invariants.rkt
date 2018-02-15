#lang s-exp rosette/safe
(provide (all-defined-out))

(require "db.rkt")

(define (is-invariant prop)
  ; prop is true in the initial state
  (db-init-concrete)
  (assert (prop))

  ; whenever prop is true and some record processes, prop is still true
  (for-each (lambda (r)
    (printf "checking record ~s\n" r)
    (db-init-symbolic)
    (define precond (prop))
    (process r)
    (assert (=> precond (prop)))) (all-records)))
