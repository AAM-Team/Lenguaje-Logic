#lang racket

(require rackunit
         "denval.rkt")

(define denval-tests
  (test-suite
   "Denoted values test suite"

   ;; Pruebas para valores booleanos denotados
   (test-case "Boolean denoted values"
     ;; Casos válidos
     (check-not-exn (lambda ()
                      (bool-denoted #t)))
     (check-not-exn (lambda ()
                      (bool-denoted #f)))

     ;; Casos inválidos (tipos incorrectos)
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-denoted 0)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-denoted 'false)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-denoted "not a boolean")))

     ;; Verificaciones de propiedades
     (let ([denval (bool-denoted #t)])
       ;; Confirmar que el valor es reconocido como un `denoted`
       (check-true (denoted? denval))
       ;; Confirmar que el valor es reconocido como un `bool-denoted`
       (check-true (bool-denoted? denval))
       ;; Confirmar el valor almacenado
       (check-equal? (bool-denoted-val denval) #t)
       ;; Convertir el valor denotado a booleano
       (check-equal? (denoted->bool denval) #t)))))
       ;; Confirmar que no se puede convertir a otro tipo no definido

(provide
 (contract-out
  [denval-tests test-suite?]))
