;; Test Suite and test DFA's
(module test racket
   (require "red.rkt"
            mzlib/integer-set
            racket/unsafe/ops)
   ;; Again, just to make testing from REPL easier....
   (provide
    (combine-out (all-defined-out)
                 (all-from-out "red.rkt")
                 (all-from-out mzlib/integer-set)))

   ;; Build a set of functions corresponding to an RE->DFA
   ;; use this like (define f1 (benchmark dfa))
   ;;               (f1 "someinput")
   (define-syntax benchmark
     (syntax-rules ()
       [(_ dfa)
        (eval-syntax (dfa-expand dfa))]))

   ;; Around for turning a string into an equivalent char
   ;; list, strictly for the 'old' version of dfa-expand
   (define (explode str)
     (let loop ((i 0) (n (unsafe-string-length str)) (acc '()))
       (cond [(unsafe-fx= i n) (reverse acc)]
             [else (loop (unsafe-fx+ 1 i)
                         n
                         (cons (unsafe-string-ref str i) acc))])))


   ;; Tests -- DFA + regexp + input, and an associated test


   ;; A DFA-MATCH is an output of dfa-expand
   ;; with contract : (listof char) -> boolean

   ;; : DFA-MATCH RE (listof char) regexp-string string string Nat -> Unit
   (define (compare-speed dfa-match dfa-in re re-in description size)
     (printf "Now testing: ~a ~n" description)
     (printf "Input size approx: ~a ~n" size)
     (printf "Built in re matcher: ~n")
     (time (regexp-match? re re-in))
     (printf "DFA-Match: ~n")
     (time (dfa-match dfa-in))
     (printf "end test~n~n"))


   ;; .*schwers.r@gmail.com.*
   (define *email*
     (benchmark
      (build-test-dfa
       '((concatenation (repetition 0 +inf.0 (char-range "0" "z"))
                        #\s #\c #\h #\w #\e #\r #\s #\. #\r
                        #\@ #\g #\m #\a #\i #\l #\. #\c #\o #\m
                        (repetition 0 +inf.0 (char-range "0" "z")))))))


   (define s1 (make-string 100000 #\a))
   (define s2 (make-string 1000000 #\a))
   (define email "schwers.r@gmail.com")
   (define str1 (string-append s1 email s1))
   (define str2 (string-append s2 email s2))
   (define str1-list (explode str1))
   (define str2-list (explode str2))

   (define (t1)
     (compare-speed *email* str1-list  "schwers.r@gmail.com" str1
                   "*schwers.r@gmail.com*" 20000))


   (define (t2)
     (compare-speed *email* str2-list "schwers.r@gmail.com" str2
                    "*schwers.r@gmail.com*" 2000000))

   ;; ^a*$
   (define a*-only (benchmark (build-test-dfa '((repetition 0 +inf.0 #\a)))))

   (define a*-only-list (explode s2))

   (define (t3)
     (compare-speed a*-only a*-only-list "^a*$" str2
                     "only a* -- ^a*$" 2000000))

   ;; (listof tests)
   (define all-tests (list t1 t2 t3))

   (define (run-tests)
     (map (lambda (x) (x)) all-tests))

   (define (run-tests-log-to name)
     (with-output-to-file (string-append "testdata/"name)
       run-tests))

   (run-tests-log-to "testlog1.txt")
)