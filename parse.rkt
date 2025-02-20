#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in re: parser-tools/lex-sre)
         "ast.rkt")

(define-tokens data (VAR))
(define-empty-tokens delim (OPAREN CPAREN EOF))
(define-empty-tokens reserved (NOT AND OR IMPL))

(define logic-lexer
  (lexer-src-pos
   [(re:or #\newline #\return #\tab #\space #\vtab)
    (return-without-pos (logic-lexer input-port))]
   ["~" 'NOT]
   ["and" 'AND]
   ["or" 'OR]
   ["impl" 'IMPL]
   ["(" 'OPAREN]
   [")" 'CPAREN]
   [(re:seq (re:or (re:/ "a" "z")
                   (re:/ "A" "Z"))
            (re:* (re:or (re:/ "a" "z")
                         (re:/ "A" "Z")
                         (re:/ "0" "9")
                         "_")))
    (token-VAR (string->symbol lexeme))]
   [(eof) 'EOF]))

(define logic-parser
  (parser
   [src-pos]
   [start <Expr>]
   [end EOF]
   [error void]
   [tokens data delim reserved]
   [grammar
    [<Expr> [(VAR)
             (l-atom $1)]
            [(NOT <Expr>)
             (l-not $2)]
            [(OPAREN <Expr> AND <Expr> CPAREN)
             (l-and $2 $4)]
            [(OPAREN <Expr> OR <Expr> CPAREN)
             (l-or $2 $4)]
            [(OPAREN <Expr> IMPL <Expr> CPAREN)
             (l-impl $2 $4)]]]))

(define (parse-port in)
  (logic-parser (lambda () (logic-lexer in))))

(define (parse-string str)
  (parse-port (open-input-string str)))

(define (parse-file path)
  (call-with-input-file path parse-port))

(provide
 (contract-out
  [parse-port (-> input-port? logic?)]
  [parse-string (-> string? logic?)]
  [parse-file (-> string? logic?)]))
   
   