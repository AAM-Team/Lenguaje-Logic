#lang racket

(struct expressed () #:transparent)

(struct bool-expressed expressed (val) #:transparent)

(define (expressed->bool val)
  (match val
    [(bool-expressed x) x]
    [else
     (error 'expressed->bool "no es un booleano: ~a" val)]))

(provide
 (contract-out
  [expressed? (-> any/c boolean?)]
  [struct bool-expressed ([val boolean?])]
  [expressed->bool (-> expressed? boolean?)]))