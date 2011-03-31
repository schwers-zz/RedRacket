;; Test Suite and test DFA's
(module retest racket
  (require "red.rkt"
           "tester.rkt"
           racket/unsafe/ops
           (for-syntax "red.rkt"
                       racket
                       racket/unsafe/ops))
  (provide (all-defined-out))

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

  (define (as n) (build-by-twos "a" n))
  (define email "schwers.r@gmail.com")
  (define (aemaila n) (let ((s (as n))) (string-append s email s)))

  (define *email*-desc ".*schwers.r@gmail.com.*")
  (define (*email*-regex str) (regexp-match? #rx"schwers.r@gmail.com" str))
  (test *email* *email*-regex aemaila *email*-desc #t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; a* , (a+)
  (define-for-syntax a*-only-stx
    (dfa-expand (build-test-dfa '((repetition 0 +inf.0 #\a)))))

   (define-syntax (bench-a*-only stx)
     (syntax-case stx ()
       [(_) a*-only-stx]))

   (define a*-only (bench-a*-only))
   (define (a*-regex str) (regexp-match?  #rx"a*" str))

   (test a*-only a*-regex as "a* -- should pass" #t)

   (define-for-syntax a+paren-stx
     (dfa-expand (build-test-dfa '((concatenation #\( (repetition 1 +inf.0 #\a) #\))))))

   (define-syntax (bench-a+paren stx)
     (syntax-case stx ()
       [(_) a+paren-stx]))

   (define (a+paren-regx str) (regexp-match? #rx"\\(a+\\)" str))
   (define a+paren (bench-a+paren))
   (define (a+paren-str n) (let ((s (build-by-twos "a" n))) (string-append "(" s ")")))

   (test a+paren a+paren-regx a+paren-str "(a+) -- should pass" #t)

   (define (a+paren-fail n) (let ((s (build-by-twos "a" n))) (string-append "(" s "Z)")))
   (test a+paren a+paren-regx a+paren-fail "(a+) -- should fail" #f)

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

   (define em1 (lambda (n) "john.test.email@random.test.domain.uk"))
   (define em2 "this.is.a.test.right.")
   (define (email-str n) (let ((s (build-by-twos em2 n))) (string-append s "com")))
   (define (email-regex str)
     (regexp-match? #rx"[a-z0-9\\.\\_\\%\\+\\-]+@[a-z0-9\\.\\-]+\\.([a-z][a-z]|[a-z][a-z][a-z]|[a-z][a-z][a-z][a-z])"
                    str))

   (define email-desc "validate email test, should match")
   (test valid-email email-regex email-str "validate email -- should pass" #t)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; web address

   (define-for-syntax *web-stx*
     (dfa-expand
      (build-test-dfa
       '((concatenation
          #\w #\w #\w #\.
          (repetition 1 +inf.0 (char-range #\a #\z))
          #\.
          (repetition 2 4 (char-range #\a #\z)))))))

   (define-syntax (bench-web stx)
     (syntax-case stx ()
       [(_) *web-stx*]))

   (define web (bench-web))

   (define (webfiller n) (string-append "www." (build-by-twos "lambdafxxfxoiasdf" n) ".com"))
   (define (web-regex str) (regexp-match? #rx"^www\\.[a-z]+\\.([a-z][a-z]|[a-z][a-z]|[a-z][a-z][a-z][a-z])" str))

   (test web web-regex webfiller "website url -- should pass" #t)



   ;;(log-to "charted-3.csv")
)
