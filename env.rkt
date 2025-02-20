#lang racket

(require "denval.rkt")

(struct environment () #:transparent)

(struct empty-env environment () #:transparent)
(struct extend-env environment (var val parent) #:transparent)

(provide
 (contract-out
  [environment? (-> any/c boolean?)]
  [struct empty-env ()]
  [struct extend-env ([var symbol?] [val denoted?] [parent environment?])]))


