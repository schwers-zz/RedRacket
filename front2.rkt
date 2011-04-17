#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide ->redstring)


(define-tokens regtokens (LITERAL))
(define-empty-tokens regsyms (LPAREN RPAREN LBRACK RBRACK ANY COMMA DASH PLUS STAR NOT HUH EOF))

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
   [#\? (token-HUH)]
   [any-char (token-LITERAL (string->list lexeme))]
   [(eof) (token-EOF)]))


(define (->redstring input) 'dummy_value)