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

  (define (id x) x)

  (define (num-elements lo hi inc) (add1 (/ (- hi lo) inc)))

  (define (make-elements lo hi inc)
    (build-list (num-elements lo hi inc) (lambda (x) (+ lo (* inc x)))))

  (define (average lon) (/ (apply + lon) (length lon)))

  ;; Particulary useful
  (define (doall proc stuff) (map (lambda (x) (apply proc x)) stuff))

  ;; TEST FLAGS
  (define num-test-runs 20)
  (define all-tests (box empty))
  (define test-lo 10)
  (define test-hi 20)
  (define test-inc 1)

  ;; Mutationy stuff
  (define (set-res-acc! place value) (set-box! place (append (unbox place) (list value))))
  (define (set-inc! place inc) (set-box! place (+ (unbox place) inc)))
  (define (reset! place) (set-box! place 0))
  (define (add-test! test) (set-res-acc! all-tests test))

  (define (add-avg-time! place results)
    (set-box! place (append (unbox place) (list (/ (unbox results) num-test-runs)))))

  ;; Printing CSV stuff
  (define (doublequote str) (string-append "\"" str "\""))
  (define (seperate str) (string-append (doublequote str) ";"))

  (define (print-seperated nums title type)
    (define (print-seperated* nums)
      (let ((num (number->string (car nums))))
        (if (null? (cdr nums))
          (printf (string-append (doublequote num) "~n"))
          (begin (printf (seperate num))
                 (print-seperated* (cdr nums))))))
    (if (null? nums)
        (printf "HUH?")
        (begin (printf (seperate (string-append type title)))
               (print-seperated* nums))))

  (define (print-test-data cpus rels gbcs test type)
    (doall print-seperated (list (list (unbox cpus) "CPU AVERAGE" type)
                                 (list (unbox rels) "REAL AVERAGE" type)
                                 (list (unbox gbcs) "GARBAGE AVERAGE" type))))

  ;; (: build-test : (String -> Bool) (Natrual-> String) String Naturalx3)
  (define (build-test matcher input type test lo hi inc)
    (lambda ()
      (printf (string-append "~n" (doublequote test) "~n"))
      (printf (seperate "Size of Input"))
      (let ([cpu-time (box empty)] [rel-time (box empty)] [gbc-time (box empty)]
            [sizes (make-elements lo hi inc)]
            [cpu-avg (box 0)] [rel-avg (box 0)] [gbc-avg (box 0)]
            [test-runs (build-list num-test-runs id)])
        (for-each (lambda (size)
                    (let ([string (list (input size))])
                      (printf (seperate (number->string (string-length (car string)))))
                      (for-each (lambda (run)
                                  (let-values ([(res cpu rel gbc)
                                                (time-apply matcher string)])
                                    (doall set-inc! (list (list cpu-avg cpu)
                                                          (list rel-avg rel)
                                                          (list gbc-avg gbc)))))
                                test-runs)
                    (doall add-avg-time! (list (list cpu-time cpu-avg)
                                               (list rel-time rel-avg)
                                               (list gbc-time gbc-avg)))
                    (map reset! (list cpu-avg rel-avg gbc-avg))))
                  (make-elements lo hi inc))
        (printf (string-append (doublequote " ") "~n"))
        (print-test-data cpu-time rel-time gbc-time test type))))


  ;; (: test : (String -> Bool) (String -> Bool) (-> String) String)
  (define (test dfa rgx input test)
    (add-test! (build-test dfa input "DFA: " test test-lo test-hi test-inc))
    (add-test! (build-test rgx input "RGX: " test test-lo test-hi test-inc)))

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

  (define (as n) (build-by-twos "a" n))
  (define email "schwers.r@gmail.com")
  (define (aemaila n) (let ((s (as n))) (string-append s email s)))

  (define *email*-desc ".*schwers.r@gmail.com.*")
  (define (*email*-regex str) (regexp-match? #rx"schwers.r@gmail.com" str))
  (test *email* *email*-regex aemaila *email*-desc)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; a* , (a+)
  (define-for-syntax a*-only-stx
    (dfa-expand (build-test-dfa '((repetition 0 +inf.0 #\a)))))

   (define-syntax (bench-a*-only stx)
     (syntax-case stx ()
       [(_) a*-only-stx]))

   (define a*-only (bench-a*-only))
   (define (a*-regex str) (regexp-match?  #rx"a*" str))

   (test a*-only a*-regex as "a* -- should pass")

   (define-for-syntax a+paren-stx
     (dfa-expand (build-test-dfa '((concatenation #\( (repetition 1 +inf.0 #\a) #\))))))

   (define-syntax (bench-a+paren stx)
     (syntax-case stx ()
       [(_) a+paren-stx]))

   (define (a+paren-regx str) (regexp-match? #rx"\\(a+\\)" str))
   (define a+paren (bench-a+paren))
   (define (a+paren-str n) (let ((s (build-by-twos "a" n))) (string-append "(" s ")")))

   (test a+paren a+paren-regx a+paren-str "(a+) -- should pass")

   (define (a+paren-fail n) (let ((s (build-by-twos "a" n))) (string-append "(" s "Z)")))
   (test a+paren a+paren-regx a+paren-fail "(a+) -- should fail")

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
     (regexp-match? #rx"[a-z0-9\\.]_\\%\\+\\-]+@[a-z0-9\\.\\-]+\\.([a-z][a-z]|[a-z][a-z][a-z]|[a-z][a-z][a-z][a-z])"
                    str))

   (define email-desc "validate email test, should match")
   (test valid-email email-regex email-str "validate email -- should pass")

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

   (define (webfiller n) (string-append "www." (build-by-twos "lambdafxxfxoiasdf" n) ".com"))
   (define (web-regex str) (regexp-match? #rx"^www\\.[a-z]+\\.([a-z][a-z]|[a-z][a-z]|[a-z][a-z][a-z][a-z])$" str))

   (test web web-regex webfiller "website url -- should pass")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Running tests, with optional file output
   (define (run-tests)
     (map (lambda (x) (x)) (unbox all-tests))
     (printf "ALL tests completed"))

     (define (log-to name)
       (with-output-to-file name
         (lambda ()
           (run-tests))))

  (log-to "charted.csv")
)
