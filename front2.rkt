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
   (start s)
   (end EOF)
   (tokens regtokens regsyms)
   (error (lambda (tok-ok? tok-name tok-value)
            (error 'redracket
                   (if tok-ok?
                       (format "Unexpected token ~s" tok-name)
                       (format "Invalid token ~s" tok-name)))))
   ;; taken from the grammer at docs.racket-lang.org/reference/regexp.html
   (grammar
    (s      [(regexp) (list $1)])

    (regexp [(pces) (make-concat $1)]
            [(regexp OR regexp) (make-or $1 $3)])

    (pces   [(pce) (cons $1 null)]
            [(pce pces) (cons $1 $2)])

    (pce    [(repeat) $1]
            [(atom) $1])

    (repeat [(atom STAR) (make-star $1)]
            [(atom PLUS) (make-plus $1)]
            [(atom HUH)  (make-huh $1)])

    (atom   [(LPAREN regexp RPAREN) $2]
            [(LBRACK NOT rng RBRACK) (make-comp $3)]
            [(LBRACK rng RBRACK) $2]
            [(ANY) ANYTHING]
            [(LITERAL) (get-literal $1)])

    (rng    [(RBRACK) null]
            [(DASH) #\-]
            [(mrng) $1]
            [(mrng DASH) (make-range $1 #\-)])

    (mrng   [(RBRACK lrng) (make-range #\} $2)]
            [(DASH lrng) (make-range #\- $2)]
            [(liring) $1])

    (liring [(LITERAL) (get-literal $1)]
            [(LITERAL DASH LITERAL) (make-literal-range $1 $3)]
            [(liring lrng) (make-union (list $1 $2))])

    (lrng   [(NOT) #\^]
            [(LITERAL DASH LITERAL) (make-literal-range $1 $3)]
            [(NOT lrng) (make-comp $2)]
            [(liring) $1]))))


;; Smart constructors and constants for regexp constructs
(define ANYTHING (list 'char-complement))
(define SPACE (char->integer (string-ref "   " 1)))
(define (get-literal x) (car x))

(define (make-concat x)
  (if (and (list? x) (= (length x) 1))
      (car x)
      (cons 'concatentation x)))

(define (make-union lst)
  (if (= 1 (length lst))
      (car lst)
      (cons 'union (reverse lst))))

(define (literal-norm x)
  (if (and (list? x) (= 1 (length x)))
      (car x)
      x))

(define (make-literal-range x y)
  (let ([x* (literal-norm x)]
        [y* (literal-norm y)])
    (make-range x* y*)))

(define (make-range x y)
  (and (validate-range x y)
       (list 'char-range x y)))

(define (validate-range x y)
  (let ((x (char->integer x))
        (y (char->integer y)))
    (or (and (< x y)
             (not (= x SPACE))
             (not (= y SPACE)))
        (error 'regexp-make "invalid range syntax"))))

(define (make-or x y) (make-union (list x y)))

(define (make-comp x) (list 'complement x))

(define (make-rep lo hi x) (list 'repetition lo hi x))
(define (make-star x) (make-rep 0 +inf.0 x))
(define (make-plus x) (make-rep 1 +inf.0 x))
(define (make-huh x)  (make-rep 0 1 +inf.0 x))



(define (->redstring input)
  (let ((ip (open-input-string input)))
    (port-count-lines! ip)
    (redparser (lambda () (rexer ip)))))

