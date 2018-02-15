#lang racket

; This file is deliberately #lang racket so that calling set! on the `trace`
; behaves unlifted (i.e., does not respect the symbolic path condition).
; We use get-trace to restore the correct behavior by filtering out messages
; in the trace that were written with untrue path conditions.
; We must import `struct` from Rosette to make `evaluate` work on our structs.

(require (only-in rosette pc evaluate struct))
(provide (all-defined-out))

(struct StartProcessing (record-name) #:transparent)
(struct EndProcessing (record-name) #:transparent)
(struct SkipProcessing (record-name) #:transparent)
(struct WriteField (record-name field-name value) #:transparent)

(define trace (list))

(define (record-trace msg)
  (set! trace (cons (cons (pc) msg) trace)))

(define (reset-trace)
  (unless (equal? (pc) #t)
    (error 'reset-trace "can't reset trace without an empty PC"))
  (set! trace (list)))

(define (get-trace sol)
  (for/list ([e (reverse (evaluate trace sol))] #:when (car e))
    (cdr e)))
