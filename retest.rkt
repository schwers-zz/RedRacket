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
  (define (build-by-twos str pow)
    (if (<= pow 0) str
        (build-by-twos (string-append str str) (- pow 1))))

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

  (define s1 (build-by-twos "a" 20))
  (define s2 (build-by-twos "a" 26))
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

   (define a*-pass "a* -- should be match")
   (test-dfa a*-only s1 a*-pass)
   (test-dfa a*-only s2 a*-pass)

   (define a*-regex #rx"^a*$")
   (test-reg a*-regex s1 a*-pass)
   (test-reg a*-regex s2 a*-pass)

   (define a*-fail "a* -- should fail")
   (define a*-fail-fast (string-append a*-fail ".. fast?"))
   (define a*-fail-slow (string-append a*-fail ".. slow?"))

   (test-dfa a*-only (string-append "Z" s1) a*-fail-fast)
   (test-dfa a*-only (string-append s1 "Z") a*-fail-slow)
   (test-dfa a*-only (string-append "Z" s2) a*-fail-fast)
   (test-dfa a*-only (string-append s2 "Z") a*-fail-slow)

   (test-reg a*-regex (string-append "Z" s1) a*-fail-fast)
   (test-reg a*-regex (string-append s1 "Z") a*-fail-slow)
   (test-reg a*-regex (string-append "Z" s2) a*-fail-fast)
   (test-reg a*-regex (string-append s2 "Z") a*-fail-slow)


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; validating email
   (define-for-syntax valid-email-stx
     (dfa-expand
      (build-test-dfa
       '((concatenation (repetition 1 +inf.0 (union (char-range #\a #\z)
                                                    (char-range #\0 #\9)
                                                    #\. #\_ #\% #\+ #\-))
                        #\@
                        (repetition 1 +inf.0 (union (char-range #\a #\z)
                                                     (char-range #\0 #\9)
                                                     #\. #\-))
                        (repetition 2 4 (char-range #\a #\z)))))))

   (define-syntax (bench-valid-email stx)
     (syntax-case stx ()
       [(_) valid-email-stx]))

   (define valid-email (bench-valid-email))

   (define em1 "john.test.email@random.test.domain.uk")
   (define em2 "this.is.a.test.right.")
   (define em3 (build-by-twos em2 15))
   (define em4 (string-append em3 "yup@." em3))

   (define email-desc "validate email test, should match")
   (test-dfa valid-email em1 email-desc)
   (test-dfa valid-email (string-append em4 "com") email-desc)
   (test-dfa valid-email (string-append em4 "test") email-desc)
   (test-dfa valid-email (string-append em4 "de") email-desc)


   (define email-regex
     #rx"[a-z0-9\\.]_\\%\\+\\-]+@[a-z0-9\\.\\-]+\\.([a-z][a-z]|[a-z][a-z][a-z]|[a-z][a-z][a-z][a-z])")

   (test-reg email-regex em1 email-desc)
   (test-reg email-regex (string-append em4 ".com") email-desc)
   (test-reg email-regex (string-append em4 ".test") email-desc)
   (test-reg email-regex (string-append em4 ".uk") email-desc)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; web address

   (define-for-syntax *web-stx*
     (dfa-expand
      (build-test-dfa
       '(#\w #\w #\w #\.
         (concatenation  (repetition 1 +inf.0 (char-range #\a #\z))
                         #\.
                         (repetition 2 4 (char-range #\a #\z)))))))

   (define-syntax (bench-web stx)
     (syntax-case stx ()
       [(_) *web-stx*]))

   (define web (bench-web))

   (define webfiller (string-append "www." (build-by-twos "lambdafxxfxoiasdf" 10) ".com"))

   (test-dfa web webfiller "website url, should pass")

   (define web-regex #rx"^www\\.[a-z]+\\.([a-z][a-z]|[a-z][a-z]|[a-z][a-z][a-z][a-z])$")

   (test-reg web-regex webfiller "website url, should pass")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Running tests, with optional file output
   (define (run-tests)
     (map (lambda (x) (x)) all-tests)
     (printf "ALL tests completed"))

   (define (log-to name)
     (with-output-to-file name
       (lambda ()
         (printf "Performance test data~n~n")
         (run-tests))))

   (log-to "data5.txt")
)