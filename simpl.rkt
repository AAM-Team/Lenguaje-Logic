#lang racket

;;Doble negación linea 19.
;;Idempotencia para operador and (linea 25)
;;Idempotencia para operador or (linea 36)
;;Ley de absorción para operador and (linea 27-30)
;;Ley de absorción para operador or (linea 38-41)
;;Simplificación de implicación (linea 45-46)

(require "ast.rkt")

(define (simplify expression)
  (match expression
    [(l-atom var)
     expression]
    [(l-not expr)
     (define simplexpr (simplify expr))
     (match (l-not simplexpr)
       [(l-not (l-not simplexpr)) simplexpr]
       [else
        (l-not simplexpr)])]
    [(l-and left right)
     (define sleft (simplify left))
     (define sright (simplify right))
     (if (equal? sleft sright) sleft
         (match (l-and sleft sright)
           [(l-and sleft (l-or sleft sright))
            sleft]
           [(l-and sright (l-or sleft sright))
            sright]
           [(l-and sleft (l-and sleft sright))
            (simplify (l-and (l-and sleft sleft) sright))]
           [(l-and sright (l-and sleft sright))
            (simplify (l-and (l-and sright sright) sleft))]
           [(l-and (l-and sleft sright) sleft)
            (simplify (l-and (l-and sleft sleft) sright))]
           [(l-and (l-and sleft sright) sright)
            (simplify (l-and (l-and sright sright) sleft))]
           [else
            (l-and sleft sright)]))]
    [(l-or left right)
     (define sleft (simplify left))
     (define sright (simplify right))
     (if (equal? sleft sright) sleft
         (match (l-or sleft sright)
           [(l-or sleft (l-and sleft sright))
            sleft]
           [(l-or sright (l-and sleft sright))
            sright]
           ;;
           [(l-or sleft (l-or sleft sright))
            (simplify (l-or (l-or sleft sleft) sright))]
           [(l-or sright (l-or sleft sright))
            (simplify (l-or (l-or sright sright) sleft))]
           [(l-or (l-or sleft sright) sleft)
            (simplify (l-or (l-or sleft sleft) sright))]
           [(l-or (l-or sleft sright) sright)
            (simplify (l-or (l-or sright sright) sleft))]
           [(l-or (l-not expr1) (l-not expr2)) ;;leyes de demorgan
            (l-not (l-and expr1 expr2))]
           [else
            (l-or sleft sright)]))]
    [(l-impl antecedent consequent)
     (define santecedent (simplify antecedent))
     (define sconsequent (simplify consequent))
     (simplify (l-or (l-not santecedent) sconsequent))] 
    ))

(provide
 (contract-out
  [simplify (-> logic? logic?)]))