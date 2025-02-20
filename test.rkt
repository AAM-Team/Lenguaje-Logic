#lang racket

(require "ast.rkt"
         "expval.rkt"
         "denval.rkt"
         "env.rkt"
         "eval.rkt"
         "simpl.rkt")

;;PRUEBAS DEL EVALUADOR.
(define prueba-1
  (l-not (l-atom 'p))) ;; ~p negación de p

(value-of prueba-1 (extend-env 'p (bool-denoted #t) (empty-env))) ;;se evalua la expresión con valor verdadero en p

(define prueba-2
  (l-and (l-atom 'p) (l-atom 'q))) ;; (p^q) la conjunción de p y q

(value-of prueba-2 (extend-env 'p (bool-denoted #t) (extend-env 'q (bool-denoted #t) (empty-env)))) ;;se evalua la expresión
;;con valor verdadero en p y con valor verdadero en q

(define prueba-3
  (l-impl (l-atom 'p) (l-atom 'q))) ;; (p -> q) la implicación de p y q

(value-of prueba-3
          (extend-env 'p (bool-denoted #t) (extend-env 'q (bool-denoted #f) (empty-env))))
;;se evalua la expresión con valor verdadero en p y con valor falso en q

;;PRUEBAS DEL SIMPLIFICADOR.
(define prueba-4
  (l-not (l-not (l-atom 'p)))) ;; ~(~p) doble negación de p

(simplify prueba-4) ;;se manda a llamar el simplificador en la doble negación de p.

(define prueba-5
  (l-and (l-atom 'q) (l-atom 'q))) ;; p^p conjunción de una misma proposición

(simplify prueba-5) ;;se manda a llamar el simplificador en la conjunción de una misma proposición.

(define prueba-6
  (l-and (l-atom 'r) (l-or (l-atom 'r) (l-atom 'q)))) ;; (r ^ (r v q)) la conjunción de r con la disyunción de r y q.

(simplify prueba-6)