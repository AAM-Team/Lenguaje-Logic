#lang racket

(require rackunit
         "expval.rkt")

(define expval-tests
  (test-suite
   "Expressed values test suite"

   ;; Pruebas para valores booleanos expresados
   (test-case "Boolean expressed values"
     ;; Casos válidos
     (check-not-exn (lambda ()
                      (bool-expressed #t)))
     (check-not-exn (lambda ()
                      (bool-expressed #f)))

     ;; Casos inválidos (tipos incorrectos)
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-expressed 0)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-expressed 'false)))
     (check-exn exn:fail:contract?
                (lambda ()
                  (bool-expressed "not a boolean")))

     ;; Verificaciones de propiedades
     (let ([expval (bool-expressed #t)])
       ;; Confirmar que el valor es reconocido como un `expressed`
       (check-true (expressed? expval))
       ;; Confirmar que el valor es reconocido como un `bool-expressed`
       (check-true (bool-expressed? expval))
       ;; Confirmar el valor almacenado
       (check-equal? (bool-expressed-val expval) #t)
       ;; Convertir el valor expresado a booleano
       (check-equal? (expressed->bool expval) #t)
       ;; Confirmar que no se puede convertir a otro tipo (por ejemplo, entero)
       (check-exn exn:fail?
                  (lambda ()
                    (expressed->bool expval)))))))

(provide
 (contract-out
  [expval-tests test-suite?]))
