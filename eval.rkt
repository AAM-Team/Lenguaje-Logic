#lang racket

(require "ast.rkt"
         "env.rkt"
         "expval.rkt"
         "denval.rkt")

(define (lookup id env)
  (match env
    [(empty-env)
     #f]
    [(extend-env (== id) val parent)
     val]
    [(extend-env var val parent)
     (lookup id parent)]
    [else
     (error 'lookup "Not an environment: ~a" env)]))

(define (value-of prop env)
  (match prop
    [(l-atom var)
     (define val (lookup var env))
     (if (not val) (error 'value-of "Undefined variable: ~a" var)
        (bool-expressed
         (denoted->bool val)))]
    [(l-not expr)
     (if (equal? #t (expressed->bool (value-of expr env))) (bool-expressed #f)
        (bool-expressed #t))]
    [(l-and left right)
     (if (equal? #t (and (expressed->bool (value-of left env)) (expressed->bool (value-of right env)))) (bool-expressed #t)
         (bool-expressed #f))]
    [(l-or left right)
     (if (equal? #t (or (expressed->bool (value-of left env)) (expressed->bool (value-of right env)))) (bool-expressed #t)
         (bool-expressed #f))]
    [(l-impl antecedent consequent)
     (if (equal? #f (expressed->bool (value-of consequent env))) (bool-expressed #f)
         (bool-expressed #t))]))
(provide
 (contract-out
  [value-of (-> logic? environment? expressed?)]))