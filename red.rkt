 ;; Matching Regular Expressions with Derivatives
(module red racket
  (require parser-tools/private-lex/re
           parser-tools/private-lex/deriv
           parser-tools/private-lex/util
           (prefix-in is: mzlib/integer-set)
           racket/unsafe/ops
           (for-syntax racket
                       racket/unsafe/ops
                       (prefix-in is: mzlib/integer-set))
           (for-template racket
                         racket/unsafe/ops))
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
               [strlen* (car (generate-temporaries '(0)))]
               [string* (car (generate-temporaries '(0)))]
               [id-of (lambda (x) (list-ref state-ids x))]
               [finals (dfa-final-states/actions in)]
               ;; : int -> bool ; true if state labeled by x is a final state
               [final? (lambda (x)
                         (ormap (lambda (y) (eq? x (car y))) finals))]

               [transitions (clean-trans (dfa-transitions in) num)]
               ;; : (Pair <integer-set> int) -> Syntax-object
               [build-edge
                (lambda (pair)
                  (with-syntax ([(chars ...) (is:foldr cons null (car pair))]
                                [dest (id-of (cdr pair))]
                                [string string*])
                    #'[(or (= next chars) ...)
                       (dest (unsafe-fx+ i 1)
                             (char->integer
                              (unsafe-string-ref string
                                                 (unsafe-fx+ i 1))))]))]
               ;; : (Listof (Pair <integer-set> int) -> (Listof Syntax-object)
               [build-edges (lambda (pairs) (map build-edge pairs))]
               ;; : transition -> syntax-object
               [trans-expand
                (lambda (tlist)
                  (with-syntax ([src (id-of (car tlist))]
                                [empty-case (final? (car tlist))]
                                [(edges ...) (build-edges (cdr tlist))]
                                [n strlen*] [string string*])
                     #'[src
                        (lambda (i next)
                          (if (unsafe-fx= i n) empty-case
                              (cond edges ...
                                    [else #f])))]))])
          (with-syntax ([(nodes ...) (map trans-expand transitions)]
                        [start (id-of init)]
                        [n strlen*] [string string*])
            #'(lambda (string)
                (letrec ([n (unsafe-string-length string)]
                         nodes ...)
                  (start 0 (char->integer
                            (unsafe-string-ref string 0))))))))))