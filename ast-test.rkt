#lang racket

(require rackunit
         "ast.rkt")

(define ast-tests
  (test-suite
   "Abstract syntax tree test suite"
   (test-case "l-atom expressions"
     (check-not-exn (lambda ()
                      (l-atom 'p)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-atom 30))); un l-atom no puede recibir numeros
     (let ([atom (l-atom 'var)])
       (check-true (logic? atom))
       (check-true (l-atom? atom))
       (check-equal? (l-atom-var atom) 'var)))
   (test-case "l-not expressions"
     (check-not-exn (lambda ()
                      (l-not (l-atom 'p))))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-not 'not-a-logic)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-not 331)))
     (let ([not (l-not (l-atom 'var))])
       (check-true (logic? not))
       (check-true (l-not? not))
       (check-true (l-atom? (l-not-expr not)))))
   (test-case "l-and expressions"
     (check-not-exn (lambda ()
                      (l-and (l-atom 'p) (l-atom 'q))))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-and 'not-a-logic (l-atom 'q))))
     (let ([and (l-and (l-atom 'left) (l-atom 'right))])
       (check-true (logic? and))
       (check-true (l-and? and))
       (check-true (l-atom? (l-and-left and)))
       (check-true (l-atom? (l-and-right and)))))
   (test-case "l-or expressions"
     (check-not-exn (lambda ()
                      (l-or (l-atom 'p) (l-atom 'q))))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-or 'not-logic (l-atom 'r))))
     (let ([or (l-or (l-atom 'left) (l-atom 'right))])
       (check-true (logic? or))
       (check-true (l-or? or))
       (check-true (l-atom? (l-or-left or)))
       (check-true (l-atom? (l-or-right or)))))
   (test-case "l-impl expressions"
     (check-not-exn (lambda ()
                      (l-impl (l-atom 'p) (l-atom 'q))))
     (check-exn exn:fail:contract?
                (lambda ()
                  (l-impl 'not-logic (l-atom 'r))))
     (let ([impl (l-impl (l-atom 'antecedent) (l-atom 'consequent))])
       (check-true (logic? impl))
       (check-true (l-impl? impl))
       (check-true (l-atom? (l-impl-antecedent impl)))
       (check-true (l-atom? (l-impl-consequent impl)))))))

(provide
 (contract-out
  [ast-tests test-suite?]))