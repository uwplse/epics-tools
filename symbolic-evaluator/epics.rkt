#lang rosette
(provide (all-defined-out))

; Epics record processing routines implemented in Rosette

(define (process-calc get-field set-field process rname) #t)
(define (process-calcout get-field set-field process rname) #t)
(define (process-acalcout get-field set-field process rname) #t)
(define (process-scalcout get-field set-field process rname) #t)
(define (process-seq get-field set-field process rname) #t)
(define (process-fanout get-field set-field process rname) #t)
(define (process-dfanout get-field set-field process rname) #t)
(define (process-waveform get-field set-field process rname) #t)
(define (process-ai get-field set-field process rname) #t)
(define (process-bi get-field set-field process rname) #t)
(define (process-mbbi get-field set-field process rname) #t)
(define (process-longin get-field set-field process rname) #t)
(define (process-stringin get-field set-field process rname) #t)
(define (process-ao get-field set-field process rname) #t)
(define (process-bo get-field set-field process rname) #t)
(define (process-mbbo get-field set-field process rname) #t)
(define (process-longout get-field set-field process rname) #t)
(define (process-stringout get-field set-field process rname) #t)
