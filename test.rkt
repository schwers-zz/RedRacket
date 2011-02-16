#lang racket

(require "retest.rkt")
(require racket/unsafe/ops)


(define (explode str)
  (let loop ((i 0) (n (unsafe-string-length str)) (acc '()))
    (cond [(unsafe-fx= i n) (reverse acc)]
          [else (loop (unsafe-fx+ 1 i) n (cons (unsafe-string-ref str i) acc))])))


(define a-str (make-string 10000000 #\a))
(define a-list (explode a-str))

(define a (benchmark t4))
(define (f1) (a a-list))
(time (f1))


(define (f2)
  (regexp-match? "^a*$" a-str))

(time (f2))
