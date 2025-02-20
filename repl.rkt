#lang racket

(require racket/gui
         "ast.rkt"
         "simpl.rkt"
         "parse.rkt")

;La interfaz solo simplifica expresiones, para evaluarlas se tiene que hacer mediante la terminal.

(define frame
  (new frame% [label "Lenguaje LOGIC"]
       [min-width 450]
       [min-height 1000]))

(define menu-bar
  (new menu-bar% [parent frame]))

(define menu-edit
  (new menu% [parent menu-bar]
       [label "Editar"]))

(append-editor-operation-menu-items menu-edit #f)

(define menu-font
  (new menu% [parent menu-bar]
       [label "Fuente"]))

(append-editor-font-menu-items menu-font)

(define layout
  (new vertical-pane% [parent frame]
       [vert-margin 10]
       [horiz-margin 10]
       [spacing 10]
       [alignment '(center top)]))

(define history-canvas
  (new editor-canvas% [parent layout]
       [style '(auto-vscroll auto-hscroll)]))

(define append-readonly-text%
  (class text%
    (inherit insert last-position)
    (define editing? #f)
    (define/augment (can-delete? s l)
      #f)
    (define/augment (can-insert? s l)
      editing?)
    (super-new)
    (define/public (insert-append str)
      (set! editing? #t)
      (insert str (last-position))
      (set! editing? #f))))

(define history-text
  (new append-readonly-text%))

(send history-canvas set-editor history-text)
(send history-text set-tabs null 2 #f)

(define input-canvas
  (new editor-canvas% [parent layout]
       [min-height 100]
       [stretchable-height #f]
       [style '(auto-vscroll auto-hscroll)]))

(define input-text
  (new text%))

(send input-canvas set-editor input-text)
(send input-text set-tabs null 2 #f)

(define button-panel
  (new horizontal-pane% [parent layout]
       [spacing 10]
       [alignment '(center top)]))

(define send-button
  (new button% [parent button-panel]
       [label "Simpl"]
       [callback (lambda (button event)
                   (define code (send input-text get-text))
                   (send input-text erase)
                   (send history-text insert-append "Entrada:\n")
                   (send history-text insert-append code)
                   (send history-text insert-append "\nSalida:\n")
                   (send history-text insert-append (simpl-string code))
                   (send history-text insert-append "\n--------------------\n"))]))

(define (simpl-string str)
  (let ([parsed (parse-string str)])
    (define (format-expression expr)
      (cond
        [(l-atom? expr)
         (symbol->string (l-atom-var expr))]
        [(l-not? expr)
         (string-append "~" (format-expression (l-not-expr expr)))]
        [(l-and? expr)
         (string-append "(" (format-expression (l-and-left expr)) " and "
                        (format-expression (l-and-right expr)) ")")]
        [(l-or? expr)
         (string-append "(" (format-expression (l-or-left expr)) " or "
                        (format-expression (l-or-right expr)) ")")]
        [(l-impl? expr)
         (string-append "(" (format-expression (l-impl-antecedent expr)) " impl "
                        (format-expression (l-impl-consequent expr)) ")")]
        [else (error "Unexpected structure" expr)]))
    (format "~a" (format-expression (simplify parsed)))))


(send frame show #t)