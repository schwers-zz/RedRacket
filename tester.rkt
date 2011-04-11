;; Tester suite functions
(module tester racket
        (provide (all-defined-out))

  (define (id x) x)
  (define (square x) (* x x))

  (define (num-elements lo hi inc) (add1 (/ (- hi lo) inc)))

  (define (make-elements lo hi inc)
    (build-list (num-elements lo hi inc) (lambda (x) (+ lo (* inc x)))))

  (define (build-by-twos str pow)
    (if (<= pow 0) str
        (build-by-twos (string-append str str) (- pow 1))))

  ;; Adapter from mean-variance found at nklein.com
  (define (mean-variance lon)
    (if (null? lon)
        (cons 0 0)
        (let loop ([n 1] [xs (car lon)] [x2s (square (car lon))] [lon (cdr lon)])
          (if (null? lon)
              (let ([mean (/ xs n)])
                (cons mean (- (/ x2s n) (square mean))))
              (loop (+ n 1) (+ xs (car lon)) (+ x2s (square (car lon))) (cdr lon))))))

  ;; Particulary useful
  (define (doall proc stuff) (map (lambda (x) (apply proc x)) stuff))

  ;; TEST FLAGS
  (define num-test-runs 20)
  (define all-tests (box empty))
  (define test-lo 10)
  (define test-hi 20)
  (define test-inc 1)
  (define all_at_once? #t)

  ;; Mutationy stuff
  (define (set-app! place value) (set-box! place (append (unbox place) (list value))))
  (define (set-inc! place inc) (set-box! place (+ (unbox place) inc)))
  (define (reset! place) (set-box! place empty))

  (define (add-test! test) (set-app! all-tests test))

  (define (add-stats! place results)
      (set-app! place (mean-variance results)))

  ;; Printing CSV stuff
  (define (doublequote str) (string-append "\"" str "\""))
  (define (seperate str) (string-append (doublequote str) ";"))
  (define end (string-append (doublequote "") "~n"))

  (define (print-seperated nums title type)
    (define (make-str x) (seperate (number->string (exact->inexact x))))
    (define (print-seperated* nums means varies)
      (if (null? nums)
          (begin (printf (string-append means end)) (printf (string-append varies end)))
          (let ([mean (make-str (caar nums))] [vari (make-str (cdar nums))])
            (print-seperated* (cdr nums)
                              (string-append means mean)
                              (string-append varies vari)))))
    (if (null? nums)
        (printf "HUH?")
        (print-seperated* nums
                          (seperate (string-append type title "MEAN"))
                          (seperate (string-append type title "VARIANCE")))))


  (define (print-test-stats cpus rels gbcs test type)
    (doall print-seperated (list (list (unbox cpus) "CPU" type)
                                 (list (unbox rels) "REAL" type)
                                 (list (unbox gbcs) "GARBAGE" type))))

  (define (bool->string b) (if b "Passed" "Failed"))

  (define (print-as-expect lob)
    (define (print-as-expected* lob)
      (if (null? lob) (printf end)
          (begin (printf (seperate (bool->string (car lob))))
                 (print-as-expected* (cdr lob)))))
    (if (null? lob) (printf "HUH?") (print-as-expected* lob)))

  (define (->all_at_once test)
    (lambda (string)
      (for-each (lambda (run) (test string))
                (build-list num-test-runs id))))

  (define (make-test-matcher matcher)
    (if all_at_once? (->all_at_once matcher)
        matcher))

  (define (normalize num) (if all_at_once? (/ num num-test-runs) num))

  ;; The actual test thats run
  ;; (: build-test : (String -> Bool) (Natrual-> String) String Naturalx3)
  (define (build-test matcher input type test lo hi inc should-be)
    (lambda ()
      (printf (string-append "~n" (doublequote test) "~n"))
      (printf (seperate "Size of Input"))
      (let ([isexpect? (box empty)] [cpu-stats (box empty)]
            [rel-stats (box empty)] [gbc-stats (box empty)]
            [sizes (make-elements lo hi inc)]
            [expects (box true)]
            [cpus (box empty)] [rels (box empty)] [gbcs (box empty)]
            [test-runs (build-list num-test-runs id)]
            [matcher (make-test-matcher matcher)])
        (for-each (lambda (size)
                    (let ([string (list (input size))])
                      (printf (seperate (number->string (string-length (car string)))))
                      (for-each (lambda (run)
                                  (let-values ([(res cpu rel gbc) (time-apply matcher string)])
                                    (set-box! expects (and (unbox expects)
                                                           (equal? (car res) should-be)))
                                    (doall set-app! (list (list cpus (normalize cpu))
                                                          (list rels (normalize rel))
                                                          (list gbcs (normalize gbc))))))
                                test-runs)
                      (doall add-stats! (list (list cpu-stats (unbox cpus))
                                              (list rel-stats (unbox rels))
                                              (list gbc-stats (unbox gbcs))))
                      (set-app! isexpect? expects)
                      (map reset! (list cpus rels gbcs))
                      (set-box! expects true)))
                  (make-elements lo hi inc))
        (printf (string-append (doublequote " ") "~n"))
        (print-test-stats cpu-stats rel-stats gbc-stats test type)
        (print-as-expect (unbox isexpect?)))))


  ;; (: test : (String -> Bool) (String -> Bool) (-> String) String)
  (define (test dfa rgx input test expect)
    (add-test! (build-test dfa input "DFA: " test test-lo test-hi test-inc expect))
    (add-test! (build-test rgx input "RGX: " test test-lo test-hi test-inc expect)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Running tests, with optional file output
  (define (run-tests)
    (map (lambda (x) (x)) (unbox all-tests))
    (printf "ALL tests completed"))

  (define (log-to name)
    (with-output-to-file name
         (lambda ()
           (run-tests))))

)