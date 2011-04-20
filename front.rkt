#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide ->redstring ->insredstring add-anything)


(define-tokens regtokens (LITERAL))
(define-empty-tokens regsyms (LPAREN RPAREN LBRACK RBRACK LCURL RCURL ANY COMMA DASH PLUS STAR NOT OR HUH EOF))

(define rex-literal (lexer [any-char (token-LITERAL lexeme)] [(eof) (token-EOF)]))

(define rexer
  (lexer
   [(:: #\\ any-char) (token-LITERAL (cadr (string->list lexeme)))]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\[ (token-LBRACK)]
   [#\] (token-RBRACK)]
   [#\{ (token-LCURL)]
   [#\} (token-RCURL)]
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
   (precs (left OR)
          (right NOT LITERAL DASH))

   ;; taken from the grammer at docs.racket-lang.org/reference/regexp.html
   (grammar
    (s      [(regexp) (list $1)])

    (regexp [(pces) (make-concat $1)]
            [(regexp OR regexp) (make-union $1 $3)])

    (pces   [(pce) (cons $1 null)]
            [(pce pces) (cons $1 $2)])

    (pce    [(repeat) $1]
            [(atom) $1])

    (repeat [(atom STAR) (make-star $1)]
            [(atom PLUS) (make-plus $1)]
            [(atom HUH)  (make-huh $1)]
            [(atom rep)  ($2 $1)])

    (atom   [(LPAREN regexp RPAREN) $2]
            [(LBRACK NOT rng RBRACK) (make-comp $3)]
            [(LBRACK rng RBRACK) $2]
            [(ANY) ANYTHING]
            [(LITERAL) (get-literal $1)])

    (rep    [(LCURL num COMMA rstrep) (make-rep-fun $2 $4)]
            [(LCURL COMMA rstrep) (make-rep-fun 0 $3)])

    (rstrep [(num RCURL) $1]
            [(RCURL) +inf.0])

    (num    [(LITERAL) (cons (get-literal $1) null)]
            [(LITERAL num) (cons (get-literal $1) $2)])

    (rng    [(RBRACK) null]
            [(DASH) #\-]
            [(mrng) $1]
            [(mrng DASH) (make-range $1 #\-)])

    (mrng   [(RBRACK lrng) (make-range #\} $2)]
            [(DASH lrng) (make-range #\- $2)]
            [(liring) $1])

    (liring [(LITERAL) (get-literal $1)]
            [(LITERAL DASH LITERAL) (make-literal-range $1 $3)]
            [(liring lrng) (make-union $1 $2)])

    (lrng   [(NOT) #\^]
            [(LITERAL DASH LITERAL) (make-literal-range $1 $3)]
            [(NOT lrng) (make-comp $2)]
            [(liring) $1]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart constructors and constants for regexp constructs
(define ANYTHING (list 'char-complement))
(define SPACE (char->integer (string-ref "   " 1)))
(define (get-literal x) (literal-norm x))

(define (get-num x)
  (cond [(number? x) x]
        [(list? x) (build-num x)]
        [else (error 'regexp-make "~s not a number in repetition" x)]))

(define (build-num x)
  (define (build-num* x acc)
    (if (null? x) acc
        (let ([num? (- (char->integer (car x)) 48)])
          (if (<= num? 9)
              (build-num* (cdr x)
                          (+ (* acc 10) num?))
              (error 'regexp-make "~s not a number in repetition" x)))))
  (build-num* x 0))

(define (make-concat x)
  (if (and (list? x) (= (length x) 1))
      (car x)
      (cons 'concatenation  x)))

(define (make-union x y)
  (append '(union) (unionize x y)))

(define (unionize x y)
 (map literal-norm
      (append (list (remove-sym x 'union))
              (fix-union (remove-sym y 'union)))))

(define (remove-sym thing sym)
  (if (list? thing)
      (if (and (>= (length thing) 2)
               (eq? sym (car thing)))
          (cdr thing)
          thing)
      (list thing)))

(define (fix-union thing)
  (if (list? thing)
      (if (and (>= (length thing) 2)
               (eq? 'concatenation (car thing)))
          (list thing)
          thing)
      (list thing)))

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

(define (make-comp x) (list 'complement x))

(define (make-rep lo hi x)
  (and (valid-rep lo hi)
       (list 'repetition lo hi x)))

(define (valid-rep lo hi)
  (if (<= lo hi) #t
      (error 'regexp-make "invalid number for repetition")))

(define (make-rep-fun lo hi) (lambda (x) (make-rep (get-num lo) (get-num hi) x)))

(define (make-star x) (make-rep 0 +inf.0 x))
(define (make-plus x) (make-rep 1 +inf.0 x))
(define (make-huh x)  (make-rep 0 1 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Function for redracket.rkt

(define (add-anything x)
  (define dot-star (list 'concatenation (list 'repetition 0 +inf.0 '(char-complement))))
  (and (list? x)
      (and (list? (car x))
           (let ([hd (caar x)])
             (if (eq? hd 'concatenation)
                 (list (append dot-star (cdar x)))
                 (list (append dot-star (list (car x)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outward interface
(define (->redstring input)
  (define (begins-with?)
    (if (eq? (string-ref input 0) #\^)
        (values #f 1)
        (values #t 0)))
  (define (ends-with?)
    (let ([end (- (string-length input) 1)])
      (if (eq? (string-ref input end) #\$)
          (values #f end)
          (values #t (+ end 1)))))
  (let-values
      ([(start*? start-pos) (begins-with?)]
       [(end*?   end-pos)   (ends-with?)])
    (let ([ip (open-input-string (substring input start-pos end-pos))])
      (port-count-lines! ip)
      (values start*? end*? (redparser (lambda () (rexer ip)))))))

(define (->insredstring input)
  (let-values ([(start*? end*? result) (->redstring input)])
    (list start*? end*? result)))

