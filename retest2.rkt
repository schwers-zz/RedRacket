;; Rewriting tests to use the new front-end
(module retest2 racket
  (require "redracket.rkt"
           "tester.rkt")
  (provide (all-defined-out))

  ;; quick and dirty helper functions
  (define (as n) (build-by-twos "a" n))
  (define (email n) (let ((s (as n))) (string-append s "schwers.r@gmail.com" s)))
  (define (bademail n) (let ((s (as n))) (string-append s "notmy@email.com" s)))
  (define (parengen n) (string-append "(" (as n) ")"))
  (define (email-str n) (string-append (build-by-twos "this.is.a.test.right" n) "@yup.com"))
  (define (weburl n) (string-append "www." (build-by-twos "lambdafxxfxoiasdf" n) ".com"))
  (define (date-f1 n) "05/05/1981")
  (define (date-f2 n) "12December2012")
  (define date-runs 10000)

  (define (schenanigans builder)
    (lambda (n)
      (let* ((s (builder n))
             (l (string-length s))
             (p (quotient l 2)))
        (string-append "@#$%.NO.LONGER.VALID."
                       (substring s 0 p)
                       "((()))"
                       (substring s p l)
                       ".VALID.NO.LONGER.%$#@"))))


  (define email-test "Testing with the regexp 'schwers.r@gmail.com'")
  (define (email-dfa? x) (dfa-match?       "schwers\\.r@gmail\\.com" x))
  (define (email-rgx? x) (regexp-match? #rx"schwers.r@gmail.com" x))
  (test email-dfa? email-rgx? email email-test #t)
  (test email-dfa? email-rgx? bademail email-test #f)
  (test email-dfa? email-rgx? (schenanigans email) email-test #f)

  (define asrepeat "Testing with the regexp '^a+$'")
  (define (asrept-dfa? x) (dfa-match?    "^a+$" x))
  (define (asrept-rgx? x) (regexp-match? "^a+$" x))
  (test asrept-dfa? asrept-rgx? as asrepeat #t)
  (test asrept-dfa? asrept-rgx? (schenanigans as) asrepeat #f)


  (define parend "Testing with the regexp '^(a+)$'")
  (define (parend-dfa? x) (dfa-match?       "^\\(a+\\)$" x))
  (define (parend-rgx? x) (regexp-match? #rx"^\\(a+\\)$" x))
  (test parend-dfa? parend-rgx? parengen parend #t)
  (test parend-dfa? parend-rgx? (schenanigans parengen) parend #f)

  ;; TODO : see if there's a way to fix do {2,4} in #rx vs #px
  (define valid-email "Testing for a valid email")
  (define (emailv-dfa? x) (dfa-match?       "^[a-z0-9\\.\\_\\%\\+\\-]+@[a-z0-9\\.\\-]+\\.[a-z]{2,4}$" x))
  (define (emailv-rgx? x) (regexp-match? #px"^[a-z0-9\\.\\_\\%\\+\\-]+@[a-z0-9\\.\\-]+\\.[a-z]{2,4}$" x))
  (test emailv-dfa? emailv-rgx? email-str valid-email #t)
  (test emailv-dfa? emailv-rgx? (schenanigans email-str) valid-email #f)

  (define valid-url "Testing for a valid url")
  (define (url-dfa? x) (dfa-match?       "^www\\.[a-z]+\\.[a-z]{2,4}$" x))
  (define (url-rgx? x) (regexp-match? #px"^www\\.[a-z]+\\.[a-z]{2,4}$" x))
  (test url-dfa? url-rgx? weburl valid-url #t)
  (test url-dfa? url-rgx? (schenanigans weburl) valid-url #f)

  (define date-dd/mm/yyyy "Testing for a valid date dd/mm/yyyy")
  (define (datef1-dfa? x) (dfa-match?       "^[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]$" x))
  (define (datef1-rgx? x) (regexp-match? #rx"^[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]$" x))
  (test datef1-dfa? datef1-rgx? date-f1 date-dd/mm/yyyy #t #:times date-runs)

  (define date-ddmonthyyyy "Testing for a valid date ddmonthyyyy")
  (define (datef2-dfa? x) (dfa-match?       "^[0-9][0-9][A-Za-z]{3,9}[0-9][0-9][0-9][0-9]$" x))
  (define (datef2-rgx? x) (regexp-match? #rx"^[0-9][0-9][A-Za-z]{3,9}[0-9][0-9][0-9][0-9]$" x))
  (test datef2-dfa? datef2-rgx? date-f2 date-ddmonthyyyy #t #:times date-runs)


  ;; ALSO : Numbers / result of (run-tests) seem odd...
)
