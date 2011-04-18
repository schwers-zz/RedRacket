#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide ->redstring)


(define-tokens regtokens (LITERAL))
(define-empty-tokens regsyms (LPAREN RPAREN LBRACK RBRACK ANY COMMA DASH PLUS STAR NOT OR HUH EOF))

(define rex-literal (lexer [any-char (token-LITERAL lexeme)] [(eof) (token-EOF)]))

(define rexer
  (lexer
   [(:: #\\ any-char) (token-LITERAL (cadr (string->list lexeme)))]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\[ (token-LBRACK)]
   [#\] (token-RBRACK)]
   [#\. (token-ANY)]
   [#\, (token-COMMA)]
   [#\- (token-DASH)]
   [#\+ (token-PLUS)]
   [#\* (token-STAR)]
   [#\^ (token-NOT)]
   [#\| (token-OR)]
   [#\? (token-HUH)]
   [any-char (token-LITERAL (string->list lexeme))]
   [(eof) (token-EOF)]))

(define redparser
  (parser
   (start regexp)
   (end EOF)
   (tokens regtokens regsyms)
   (error (lambda (tok-ok? tok-name tok-value)
            (error 'redracket
                   (if tok-ok?
                       (format "Unexpected token ~s" tok-name)
                       (format "Invalid token ~s" tok-name)))))
   ;; taken from the grammer at docs.racket-lang.org/reference/regexp.html
   (grammar
    (regexp [(pces) $1]
            [(regexp OR regexp) (list 'OR $1 $3)])

    (pces   [(pce) (cons $1 null)]
            [(pce pces) (cons $1 $2)])

    (pce    [(repeat) $1]
            [(atom) $1])

    (repeat [(atom STAR) (list 'STAR $1)]
            [(atom PLUS) (list 'PLUS $1)]
            [(atom HUH)  (list 'HUH $1)])

    (atom   [(LPAREN regexp RPAREN) (list 'parend $2)]
            [(LBRACK NOT rng RBRACK) (list 'notrange $3)]
            [(LBRACK rng RBRACK) (list 'range $2)]
            [(ANY) (list 'anything)]
            [(LITERAL) (list $1)])

    (rng    [(RBRACK) null]
            [(DASH) (list 'dash)]
            [(mrng) $1]
            [(mrng DASH) (list 'dash $1)])

    (mrng   [(RBRACK lrng) (list 'rbrack_range $2)]
            [(DASH lrng) (list 'dash_range $2)]
            [(liring) (list 'litrange $1)])

    (liring [(LITERAL) $1]
            [(LITERAL DASH LITERAL) (list 'range $1 $3)]
            [(liring lrng) (list 'range $1 $2)])

    (lrng   [(NOT) #\^]
            [(LITERAL DASH LITERAL) (list 'range $1 $3)]
            [(NOT lrng) (list 'range #\^ $2)]
            [(liring) $1]))))

(define (->redstring input)
  (let ((ip (open-input-string input)))
    (port-count-lines! ip)
    (redparser (lambda () (rexer ip)))))

