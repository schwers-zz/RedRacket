 ;; Matching Regular Expressions with Derivatives

(module matcher racket
  (require parser-tools/private-lex/re
           parser-tools/private-lex/deriv
           parser-tools/private-lex/util
           mzlib/integer-set)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; From Deriv-Racket
  ;; (make-dfa num-states start-state final-states/actions transitions)
  ;;  where num-states, start-states are int
  ;;  final-states/actions is (list-of (cons-int syntax-object))
  ;;  transitions is (list-of (cons int (list-of (cons char-set int))))

  ;; : (list-of transitions) -> (list-of transitions)
  ;; the vanillla list from dfa-transitions is minimal
  ;; => need to insert states that have no outgoing edges
  ;;    otherwise we will try to invoke an undefined function
  ;;    when we try to run the 'dfa' family of functions
  (define (clean-trans trans num)
    (let loop ([lot trans][n 0] [acc '()])
      (cond [(and (empty? lot) (eq? n num)) (reverse acc)]
            [(and (not (empty? lot)) (eq? n (caar lot)))
             (loop (cdr lot) (add1 n) (cons (car lot) acc))]
            [(empty? lot) (loop lot (add1 n) (cons (cons n empty) acc))]
            [else (loop (cdr lot) (add1 n) (cons (cons n empty) acc))])))

  ;;  : dfa -> syntax-object
  (define (dfa-expand in)
    (if (not (dfa? in)) (error 'dfa-expand "improper input")
        (let* ([id (lambda (x) x)]
               [num (dfa-num-states in)]
               [init (dfa-start-state in)]
               [state-ids (generate-temporaries (build-list num id))]
               [id-of (lambda (x) (list-ref state-ids x))]
               [finals (dfa-final-states/actions in)]
               ;; : int -> bool ; true if state labeled by x is a final state
               [final? (lambda (x)
                         (ormap (lambda (y) (eq? x (car y))) finals))]

               [transitions (clean-trans (dfa-transitions in) num)]
               ;; : transition -> (list-of int)
               [destinations (lambda (x)
                               (map (lambda (y) (id-of (cdr y))) (rest x)))]
               ;; : transition -> (list-of char-set)
               [edges (lambda (x) (map car (rest x)))]
               ;; : transition -> syntax-object
               [trans-expand
                (lambda (tlist)
                  (with-syntax ([src (id-of (car tlist))]
                                [empty-case (final? (car tlist))]
                                [(set ...) (edges tlist)]
                                [(dst ...) (destinations tlist)])
                   ;; Heart of the matcher
                   #'[src
                      (lambda (stream)
                        (if (empty? stream) empty-case
                            (let ([hd (char->integer (first stream))]
                                  [tl (rest stream)])
                              (cond [(member? hd set) (dst tl)]
                                    ...
                                    [else false]))))]))])
          (with-syntax ([(node ...) (map trans-expand transitions)]
                        [start (id-of init)])
            #'(letrec (node ...)
                start)))))

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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tests
  ;; This should eval to true
  ;; (eval (cons (syntax->datum (dfa-expand t4)) (cons (quote (cons #\a (cons #\a (cons #\a (cons #\a empty))))) empty)))

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