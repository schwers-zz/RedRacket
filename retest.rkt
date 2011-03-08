;; Test Suite and test DFA's
(module retest racket
   (require "red.rkt"
            racket/unsafe/ops
            (for-syntax "red.rkt"
                        racket
                        racket/unsafe/ops))
   ;; Again, just to make testing from REPL easier....
   (provide
    (combine-out (all-defined-out)))

   ;; control parameters for tests
   (define regexp-tests? #t)
   (define dfa-tests? #t)

   (define all-tests '())

   ;; : symbol -> boolean ; returns true if the test should be run
   (define (run-test? which)
     (or (and (equal? which 'regexp) regexp-tests?)
         (and (equal? which 'dfa) dfa-tests?)))

   ;; : (string -> boolean) string string symbol -> Unit
   ;; takes a function of a string, which returns boolean based on a match
   ;; runs the test according to the controls, and prints results
   (define (test-match matcher input test which)
     (if (run-test? which)
       (begin
         (set! all-tests
               (append all-tests
                       (list (lambda ()
                               (printf "TESTING: ~s~n" test)
                               (printf "Input size: ~a~n" (string-length input))
                               (printf "Matcher : ~a~n" which)
                               (time (matcher input))
                               (printf "~n")))))
               (printf "ADDED ~a:~a to tests~n" which test))
         (printf "~a:~a was not added~n" which test)))

   (define (test-dfa dfa input test) (test-match dfa input test 'dfa))

   (define (test-reg regexp input test)
     (test-match (lambda (str) (regexp-match? regexp str))
                 input test 'regexp))


   ;; : string number -> string
   ;; helper function to build large strings
   (define (buildByTwos str pow)
     (if (<= pow 0) str
         (buildByTwos (string-append str str) (- pow 1))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

   (define s1 (buildByTwos "a" 20))
   (define s2 (buildByTwos "a" 26))
   (define email "schwers.r@gmail.com")
   (define str1 (string-append s1 email s1))
   (define str2 (string-append s2 email s2))


   (define *email*-desc ".*schwers.r@gmail.com.*")
   (test-dfa *email* str1 *email*-desc)
   (test-dfa *email* str2 *email*-desc)

   (define *email*-regex  #rx"schwers.r@gmail.com")
   (test-reg *email*-regex str1 *email*-desc)
   (test-reg *email*-regex str2 *email*-desc)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; ^a*$
   (define-for-syntax a*-only-stx
     (dfa-expand
      (build-test-dfa '((repetition 0 +inf.0 #\a)))))

   (define-syntax (bench-a*-only stx)
     (syntax-case stx ()
       [(_) a*-only-stx]))

   (define a*-only (bench-a*-only))

   (define a*-passing "a* -- should be #t")
   (test-dfa a*-only s1 a*-passing)
   (test-dfa a*-only s2 a*-passing)

   (define a*-regex #rx"^a*$")
   (test-reg a*-regex s1 a*-passing)
   (test-reg a*-regex s2 a*-passing)

   (define (run-tests)
     (map (lambda (x) (x)) all-tests))

   (define (run-tests-log-to name)
     (with-output-to-file name)
       (lambda ()
         (printf "Testdata from retest.rkt~n~n")
         (run-tests)))

   ;;(run-tests-log-to "testdata.txt")
)