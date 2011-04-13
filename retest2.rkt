;; Rewriting tests to use the new front-end
(module retest2 racket
  (require "redracket.rkt"
           "tester.rkt")
  (provide (all-defined-out))

  ;; quick and dirty helper functions
  (define (as n) (build-by-twos "a" n))
  (define email "schwers.r@gmail.com")
  (define (aemaila n) (let ((s (as n))) (string-append s email s)))
  (define (bademail n) (let ((s (as n))) (string-append s "notmy@email.com" s)))
  (define (schenanigans n) (let ((s (as n))) (string-append s "s" s)))
  (define (parengen n) (let ((s (as n))) (string-append "(" s ")")))

  (define email-test "Testing with the regexp 'schwers.r@gmail.com'")
  (define (email-dfa? x) (dfa-match? "schwers\\.r@gmail\\.com" x))
  (define (email-rgx? x) (regexp-match? #rx"schwers.r@gmail.com" x))
  (test email-dfa? email-rgx? aemaila email-test #t)
  (test email-dfa? email-rgx? bademail email-test #f)
  (test email-dfa? email-rgx? schenanigans email-test #f)

  (define asrepeat "Testing with the regexp '^a+$'")
  (define (asrept-dfa? x) (dfa-match? "^a+$" x))
  (define (asrept-rgx? x) (regexp-match? "^a+$" x))
  (test asrept-dfa? asrept-rgx? as asrepeat #t)
  (test asrept-dfa? asrept-rgx? schenanigans asrepeat #f)


  (define parend "Testing with the regexp '^(a+)$'")
  (define (parend-dfa? x) (dfa-match? "^\\(a+\\)$" x))
  (define (parend-rgx? x) (regexp-match? #rx"^\\(a+\\)$" x))
  (test parend-dfa? parend-rgx? parengen parend #t)
  (test parend-dfa? parend-rgx? schenanigans parend #f)

  (define valid-email "Testing for a valid email")
  ;; TODO : Add ranges to the matcher
  ;; ALSO : Numbers / result of (run-tests) seem odd...
)
