 ;; Matching Regular Expressions with Derivatives

(module matcher racket
  (require parser-tools/private-lex/re
           parser-tools/private-lex/deriv
           parser-tools/private-lex/util
           mzlib/integer-set)
  ;; Only for Repl testing, promise :) ...
  (provide
   (combine-out (all-defined-out)
                (all-from-out parser-tools/private-lex/re)
                (all-from-out parser-tools/private-lex/deriv)
                (all-from-out parser-tools/private-lex/util)))

  (define (build-and-print x)
    (print-dfa (build-test-dfa x)))

  (define (build-test-dfa rs)
    (let ((c (make-cache)))
      (build-dfa (map (lambda (x) (cons (->re x c) 'action))
                      rs)
                 c)))


  ;; Macro for turning a DFA into a set of mutually recursive functions
  (define-syntax rematch
    (syntax-rules ()
      [(_ (dfa num-states init final trans))
       (let-syntax
           ([trans-help
             (syntax-rules (quote)
               [(_ (quote ((src (set dest) (... ...)) (... ...))))
                (letrec ([(state-id src)
                         (lambda (stream) 
                           ;; This should really call a helper function to
                           ;; to see if src is in the set of final states....
                           (cond [(empty? stream) false]
                                 [else
                                  (let ([hd (first stream)] [tl (rest stream)])
                                    (cond [(member? hd set) (dest tl)]
                                          (... ...)
                                          [else false]))]))]
                         (... ...))
                  (state-id init))])])
         (trans-help trans))]))

  (define (state-id x) (string->symbol (number->string x)))



  (rematch (dfa 5 0 '() '((0 (1 2) (3 4)) (1 (5 6) (7 8)))))





    ;; Test DFA's From deriv.rkt, making sure we can build everything
  (define t1 (build-test-dfa null))
  (define t2 (build-test-dfa `(#\a)))
  (define t3 (build-test-dfa `(#\a #\b)))
  (define t4 (build-test-dfa `((repetition 0 +inf.0 #\a)
                               (repetition 0 +inf.0 (concatenation #\a #\b)))))
  (define t5 (build-test-dfa `((concatenation (repetition 0 +inf.0 (union #\0 #\1)) #\1))))
  (define t6 (build-test-dfa `((repetition 0 +inf.0 (repetition 0 +inf.0 #\a))
                               (repetition 0 +inf.0 (concatenation #\b (repetition 1 +inf.0 #\b))))))
  (define t7 (build-test-dfa `((concatenation (repetition 0 +inf.0 #\a) (repetition 0 +inf.0 #\b)
                                              (repetition 0 +inf.0 #\c) (repetition 0 +inf.0 #\d)
                                                (repetition 0 +inf.0 #\e)))))
  (define t8
    (build-test-dfa `((concatenation (repetition 0 +inf.0 (union #\a #\b)) #\a (union #\a #\b)
                                     (union #\a #\b) (union #\a #\b) (union #\a #\b)))))
  (define t9 (build-test-dfa `((concatenation "/*"
                                              (complement (concatenation (intersection) "*/" (intersection)))
                                                "*/"))))
  (define t11 (build-test-dfa `((complement "1"))))
  (define t12 (build-test-dfa `((concatenation (intersection (concatenation (repetition 0 +inf.0 "a") "b")
                                                             (concatenation "a" (repetition 0 +inf.0 "b")))
                                               "ab"))))
  (define x (build-test-dfa `((union " " "\n" ",")
                              (concatenation (repetition 0 1 "-") (repetition 1 +inf.0 (char-range "0" "9")))
                              (concatenation "-" (repetition 1 +inf.0 "-"))
                              "["
                              "]")))
  (define y (build-test-dfa
             `((repetition 1 +inf.0
                           (union (concatenation "|" (repetition 0 +inf.0 (char-complement "|")) "|")
                                  (concatenation "|" (repetition 0 +inf.0 (char-complement "|"))))))))
  (define t13 (build-test-dfa `((intersection (concatenation (intersection) "111" (intersection))
                                              (complement (union (concatenation (intersection) "01")
                                                                 (repetition 1 +inf.0 "1")))))))
  (define t14 (build-test-dfa `((complement "1"))))
  
  ;; Some tests from re.rkt
  (test-block ((c (make-cache))
               (a (char->integer #\a))
               (b (char->integer #\b))
               (r1 (->re #\a c))
               (r2 (->re `(repetition 0 +inf.0 #\a) c))
               (r3 (->re `(repetition 0 +inf.0 ,r2) c))
               (r4 (->re `(concatenation #\a ,r2) c))
               (r5 (->re `(repetition 0 +inf.0 ,r4) c))
               (r6 (->re `(union ,r5 #\a) c))
               (r7 (->re `(concatenation ,r2 ,r2) c))
               (r8 (->re `(complement ,r4) c))
               (r9 (->re `(intersection ,r2 ,r4) c)))
              ((deriveR e a c) z)
              ((deriveR z a c) z)
              ((deriveR r1 b c) z)
              ((deriveR r1 a c) e)
              ((deriveR r2 a c) r2)
              ((deriveR r2 b c) z)
              ((deriveR r3 a c) r2)
              ((deriveR r3 b c) z)
              ((deriveR r4 a c) r2)
              ((deriveR r4 b c) z)
              ((deriveR r5 a c) (->re `(concatenation ,r2 ,r5) c))
              ((deriveR r5 b c) z)
              ((deriveR r6 a c) (->re `(union (concatenation ,r2 ,r5) "") c))
              ((deriveR r6 b c) z)
              ((deriveR r7 a c) (->re `(union (concatenation ,r2 ,r2) ,r2) c))
              ((deriveR r7 b c) z)
              ((deriveR r8 a c) (->re `(complement, r2) c))
              ((deriveR r8 b c) (->re `(complement ,z) c))
                ((deriveR r9 a c) r2)
                ((deriveR r9 b c) z)
                ((deriveR (->re `(repetition 1 2 "ab") c) a c)
                 (->re `(concatenation "b" (repetition 0 1 "ab")) c))))