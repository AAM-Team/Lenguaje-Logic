#lang racket

(require rackunit
         "eval.rkt"
         "ast.rkt"
         "expval.rkt"
         "denval.rkt"
         "env.rkt")

(define eval-tests
  (test-suite
   "Evaluator test suite"
   ;; Pruebas básicas para l-atom
   (test-case "atom evaluation"
     (check-equal?
      (value-of (l-atom 'p)
                (extend-env 'p (bool-denoted #t) (empty-env)))
      (bool-expressed #t))
     (check-exn #rx"Undefined variable"
                (lambda ()
                  (value-of (l-atom 'q) (empty-env)))))

   ;; Pruebas para l-not
   (test-case "not evaluation"
     (check-equal?
      (value-of (l-not (l-atom 'p))
                (extend-env 'p (bool-denoted #t) (empty-env)))
      (bool-expressed #f))
     (check-equal?
      (value-of (l-not (l-atom 'q))
                (extend-env 'q (bool-denoted #f) (empty-env)))
      (bool-expressed #t))
     (check-exn #rx"Undefined variable"
                (lambda ()
                  (value-of (l-not (l-atom 'r)) (empty-env)))))

   ;; Pruebas para l-and
   (test-case "and evaluation"
     (check-equal?
      (value-of (l-and (l-atom 'p) (l-atom 'q))
                (extend-env 'p (bool-denoted #t)
                            (extend-env 'q (bool-denoted #t) (empty-env))))
      (bool-expressed #t))
     (check-equal?
      (value-of (l-and (l-atom 'p) (l-atom 'q))
                (extend-env 'p (bool-denoted #t)
                            (extend-env 'q (bool-denoted #f) (empty-env))))
      (bool-expressed #f)))

   ;; Pruebas para l-or
   (test-case "or evaluation"
     (check-equal?
      (value-of (l-or (l-atom 'p) (l-atom 'q))
                (extend-env 'p (bool-denoted #t)
                            (extend-env 'q (bool-denoted #f) (empty-env))))
      (bool-expressed #t))
     (check-equal?
      (value-of (l-or (l-atom 's) (l-atom 'r))
                (extend-env 's (bool-denoted #f)
                            (extend-env 'r (bool-denoted #f) (empty-env))))
      (bool-expressed #f)))

   ;; Pruebas para l-impl (implicación)
   (test-case "Impl evaluation"
     (check-equal?
      (value-of (l-impl (l-atom 'antecedent) (l-atom 'consequent))
                (extend-env 'antecedent (bool-denoted #f)
                            (extend-env 'consequent (bool-denoted #t) (empty-env))))
      (bool-expressed #t))
     (check-equal?
      (value-of (l-impl (l-atom 'antecedent) (l-atom 'consequent))
                (extend-env 'antecedent (bool-denoted #t)
                            (extend-env 'consequent (bool-denoted #f) (empty-env))))
      (bool-expressed #f)))

   ;; Prueba de error: estructura no válida
   (test-case "Invalid structure"
     (check-exn #rx"Not a logic expression"
                (lambda ()
                  (value-of 'not-a-logic (empty-env)))))))

(provide
 (contract-out
  [eval-tests test-suite?]))