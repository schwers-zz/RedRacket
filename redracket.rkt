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
  (syntax->datum (dfa-expand (build-test-dfa (->redstring string)))))


;; The macro for user level
(define-syntax (dfa-match? stx)
  (syntax-case stx ()
    [(_ regexp string)
     (with-syntax
      ([dfa-matcher (dfa-expand (build-test-dfa (->redstring (syntax->datum #'regexp))))])
      #'(dfa-matcher string))]))


;;Example tests
(equal? (dfa-match? "a+" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") #t)
(equal? (dfa-match? "c(a|d)*r" "caadadadaddaadadadadadadddddaaaaar") #t)
(equal? (dfa-match? "schwers\\.r@gmail\\.com" "aosihdfasdfschwers.r@gmail.comaskdfas") #t)
(equal? (dfa-match? "schwers\\.r@gmail\\.com" "schwers.r@gmail.com") #t)
(equal? (dfa-match? "(node\\.js|ruby)" "lashdfasdfnode.jsihasdfiasd") #t)
(equal? (dfa-match? "(red|racket)(red|racket)" "redracket") #t)



