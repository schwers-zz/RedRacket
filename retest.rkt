;; Test Suite and test DFA's
(module retest racket
   (require "red.rkt"
            racket/unsafe/ops
            (for-syntax "red.rkt"
                        racket
                        racket/unsafe/ops))
   ;; Again, just to make testing from REPL easier....
   (provide
    (combine-out (all-defined-out)
                 (all-from-out "red.rkt")))
   ;; Build a set of functions corresponding to an RE->DFA
   ;; use this like (define f1 (benchmark dfa))
   ;;               (f1 "someinput")

   ;; Tests -- DFA + regexp + input, and an associated test


   ;; A DFA-MATCH is an output of dfa-expand
   ;; with contract : (listof char) -> boolean

   ;; : DFA-MATCH RE (listof char) regexp-string string string Nat -> Unit
   (define (compare-speed dfa-match re input description)
     (printf "Now testing: ~a ~n" description)
     (printf "Input size approx: ~a ~n" (string-length input))
     (printf "Built in re matcher: ~n")
     (time (regexp-match? re input))
     (printf "DFA-Match: ~n")
     (time (dfa-match input))
     (printf "end test~n~n"))


   (define (buildByTwos str pow)
     (if (<= pow 0) str
         (buildByTwos (string-append str str) (- pow 1))))

   (define s1 (buildByTwos "a" 20))
   (define s2 (buildByTwos "a" 26))
   (define email "schwers.r@gmail.com")
   (define str1 (string-append s1 email s1))
   (define str2 (string-append s2 email s2))


   ;; .*schwers.r@gmail.com.*
   (define-for-syntax *email*-stx
     (dfa-expand
      (build-test-dfa
       '((concatenation (repetition 0 +inf.0 (char-complement))
                        #\s #\c #\h #\w #\e #\r #\s #\. #\r
                        #\@ #\g #\m #\a #\i #\l #\. #\c #\o #\m
                        (repetition 0 +inf.0 (char-complement)))))))


   (define-syntax (bench-*email* stx)
     (syntax-case stx ()
       [(_) *email*-stx]))


   (define *email* (bench-*email*))

   (define (t1)
     (compare-speed *email* #rx"^.*schwers.r@gmail.com.*$" str1
                   "*schwers.r@gmail.com*"))


   (define (t2)
     (compare-speed *email* #rx"^.*schwers.r@gmail.com.*" str2
                    "*schwers.r@gmail.com*"))

   ;; ^a*$
   (define-for-syntax a*-only-stx
     (dfa-expand
      (build-test-dfa '((repetition 0 +inf.0 #\a)))))

   (define-syntax (bench-a*-only stx)
     (syntax-case stx ()
       [(_) a*-only-stx]))

   (define a*-only (bench-a*-only))

   (define (t3)
     (compare-speed a*-only "^a*$" s2
                    "only a* -- ^a*$"))

   ;; (listof tests)
   (define all-tests (list t1 t3))

   (define (run-tests)


     (map (lambda (x) (x)) all-tests))

   (define (run-tests-log-to name)
     (with-output-to-file name)
       (lambda ()
         (printf "Started passing the next string-ref~n~n")
         (run-tests)))

   ;;(run-tests-log-to "testdata.txt")
)