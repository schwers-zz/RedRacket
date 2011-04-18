#lang racket
;; Redracket
(require "red.rkt"
         "front.rkt"
         racket/unsafe/ops
         (for-syntax "red2.rkt"
                     "front.rkt"
                     racket
                     racket/unsafe/ops)
         (for-meta 2 "red2.rkt"
                   "front.rkt"
                   racket
                   racket/unsafe/ops))

(provide dfa-match?)

;; For testing purposes -- numbers seem odd right now
(define (inspect string)
  (let-values ([(start*? end*? dfa-in) (->redstring string)])
    (let ([dfa-in (if start*? (add-anything dfa-in) dfa-in)])
      (printf "~s~n" dfa-in)
      (syntax->datum (dfa-expand (build-test-dfa dfa-in) end*?)))))

(define (add-anything x)
  (define (insert-dot-star x)
    (list (list 'concatenation (list 'repetition 0 +inf.0 '(char-complement)) x)))
  (and (list? x)
      (and (list? (car x))
           (let ([hd (caar x)])
             (cond [(eq? hd 'union) (insert-dot-star (car x))]
                   [(eq? hd 'concatenation) (insert-dot-star (cdar x))]
                   [else (error 'regexp-make "HUH?")])))))


;; The macro for user level
(define-syntax (dfa-match? stx)
  (syntax-case stx ()
    [(_ regexp string)
     (with-syntax
      ([dfa-matcher
        (let-values ([(start*? end*? dfa-in) (->redstring (syntax->datum #'regexp))])
          (dfa-expand (build-test-dfa dfa-in) end*?))])
      #'(dfa-matcher string))]))

#|
;;Example tests
(equal? (dfa-match? "a+" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") #t)
(equal? (dfa-match? "c(a|d)*r" "caadadadaddaadadadadadadddddaaaaar") #t)
(equal? (dfa-match? "schwers\\.r@gmail\\.com" "aosihdfasdfschwers.r@gmail.comaskdfas") #t)
(equal? (dfa-match? "schwers\\.r@gmail\\.com" "schwers.r@gmail.com") #t)
(equal? (dfa-match? "(node\\.js|ruby)" "lashdfasdfnode.jsihasdfiasd") #t)
(equal? (dfa-match? "(red|racket)(red|racket)" "redracket") #t)
|#



