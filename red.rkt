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
    (let loop ([lot trans][n 0] [acc null])
      (cond [(and (null? lot) (eq? n num)) (reverse acc)]
            [(and (not (null? lot)) (eq? n (caar lot)))
             (loop (cdr lot) (add1 n) (cons (car lot) acc))]
            [(empty? lot) (loop lot (add1 n) (cons (cons n null) acc))]
            [else (loop (cdr lot) (add1 n) (cons (cons n null) acc))])))


  ;; Functions for "Roll-our-own Binary Search"...

  ;; A Range is a (Pair a b) where a <= b
  ;; A RangeMapping is a (Pair Range Dest)
  ;; A Dest is the sytanx for the destination state of the DFA

  ;; : (Listof Naturals) Dest -> (Listof RangeMapping)
  (define (to-ranges lon dest)
    (if (null? lon) (error 'to-ranges "Given an empty series for mapping")
        (let loop ([lon lon] [acc null])
          (if (null? lon) (reverse acc)
              (let ([r (get-first-range lon)])
                (loop (cdr r) (cons (cons (car r) dest) acc)))))))

  ;; : (Listof Naturals) (Pair Range (Listof Naturals))
  (define (get-first-range lon)
    (if (null? lon) (error 'get-first-range "Given an empty series")
        (let loop ([lon (cdr lon)] [acc (cons (car lon) null)])
          (if (null? lon) (cons (sequence->range (reverse acc)) null)
               (let ([a (car lon)])
                 (if (unsafe-fx= (unsafe-fx+ (car acc) 1) a)
                     (loop (cdr lon) (cons a acc))
                     (cons (sequence->range (reverse acc)) lon)))))))

  ;; (Listof Naturals) -> (Pair Natural Natural)
  (define (sequence->range lon)
    (if (null? lon) (error 'sequence-to-range "Given an empy sequnece")
        (let ([a (car lon)])
          (if (null? (cdr lon)) (cons a a)
              (let loop ([lon (cdr lon)])
                (if (null? (cdr lon)) (cons a (car lon))
                    (loop (cdr lon))))))))

  ;; (Listof RangeMapping) -> Natural
  ;; Returns the start of the first RangeMapping in X
  (define (start-of-ranges X) (caaar X))

  ;; : (Listof RangeMapping) (Listof RangeMapping) -> (Listof RangeMapping)
  ;; Assumes that (dest-of A) is different than (dest-of B)
  ;; and that the ranges are sorted
  (define (merge-range-dests A B)
    (cond [(null? A) B]
          [(null? B) A]
          [(<= (start-of-ranges A) (start-of-ranges B))
           (cons (car A) (merge-range-dests (cdr A) B))]
          [#t (cons (car B) (merge-range-dests A (cdr B)))]))

  ;; : (Listof RangeMapping) Natural
  ;;     -> (Pair (Listof RangeMapping) (Listof RangeMapping))
  ;; Returns a parition of the list where the RangeMapping whose start
  ;; is partitionn, is excluded, and RangeMappings in the first part of the pair
  ;; are less than partition, and those in the rest are greater than partition
  (define (list-rmaps->partition rmaps part)
    (let loop ([rmps rmaps] [acc1 null] [acc2 null])
      (if (empty? rmps) (cons (reverse acc1) (reverse acc2))
          (let ([start (start-of-ranges rmps)])
            (cond [(< start part) (loop (cdr rmps) (cons (car rmps) acc1) acc2)]
                  [(> start part) (loop (cdr rmps) acc1 (cons (car rmps) acc2))]
                  [#t (loop (cdr rmps) acc1 acc2)])))))


  (define CHAR_MIN 0)
  (define CHAR_MAX 1114111)

  ;; Dest Dest -> Boolean
  (define (same-id? a b)
    (bound-identifier=? a b))

  ;; (Listof (Pair Range Dest) Dest -> Boolean : returns true if all out going edges
  ;; come back to this state
  (define (staying-here? rmaps id)
     (andmap (lambda (pair) (same-id? (cdr pair) id)) rmaps))

  ;; (Listof (Pair Range Dest) Dest -> Boolean : returns true if the state
  ;; represnets .*X, where x is some symbol
  (define (loop-until-x? rmaps id)
    (and (= 3 (length rmaps))
         (let ([a (car rmaps)] [b (cadr rmaps)] [c (caddr rmaps)])
           (let ([a1 (caar a)] [a2 (cdar a)]
                 [b1 (caar b)] [b2 (cdar b)]
                 [c1 (caar c)] [c2 (cdar c)])
             (and (= b1 b2)
                  (= CHAR_MIN a1)
                  (= CHAR_MAX c2)
                  (= (+ a2 1) b1 (- c1 1))
                  (same-id? (cdr a) id)
                  (same-id? (cdr c) id))))))

  ;; (Listof (Pair Range Dest) Dest -> Syntax-Object : Simplified syntax-object
  ;; for the a state which passes loop-until-x?
  (define (build-.*-loop rmaps id)
    (let ([outmap (cadr rmaps)])
      (with-syntax ([dest (cdr outmap)]
                    [num  (caar outmap)]
                    [this id]
                    [next #'(unsafe-fx+ i 1)])
         #'(if (unsafe-fx= n num)
               (dest next)
               (this next)))))

  ;; (Listof (Pair Range Dest)-> Boolean
  ;; returns true if rmaps of the form ((0 Xi) (Xi+1 Xi+1) (Xi+2 ...) (Xj MAX))
  (define (series? rmaps)
    (define (series* rmaps x id)
      (define (first-two-same?)
        (and (= (+ 1 x) (caaar rmaps) (cdaar rmaps) (- (caaadr rmaps) 1))
             (same-id? id (cdadr rmaps))))
      (define (two-in-a-row?)
        (= (+ 1 x) (caaar rmaps) (cdaar rmaps)
           (- (caaadr rmaps) 1) (- (cdaadr rmaps) 1)))
      (if (not (null? (cdr rmaps)))
          (if (first-two-same?)
              (if (null? (cddr rmaps))
                  (= (cdaadr rmaps) CHAR_MAX)
                  (series* (cddr rmaps) (cdaadr rmaps) id))
              (if (two-in-a-row?)
                  (series* (cdr rmaps) (caaar rmaps) id)
                  #f))
          #f))
    (if (not (null? rmaps))
        (let ([s (caaar rmaps)] [x (cdaar rmaps)]  [id (cdar rmaps)])
          (and (>= (length rmaps) 3)
               (not (= s x))
               (= s CHAR_MIN)
               (series* (cdr rmaps) x id)))
        #f))

  ;; (Listof (Pair Range Dest) -> Syntax-Object
  ;; returns the syntax object for a state wich passes the predicate series?
  (define (build-series rmaps)
    (define (build-branches rmaps acc)
      (define (two-in-a-row?) (= (cdaadr rmaps) (caaadr rmaps)))
      (let ([next (with-syntax ([val (caaar rmaps)] [dest (cdar rmaps)]
                                [base acc])
                   #'(if (unsafe-fx= i val) (dest (unsafe-fx+ i 1)) base))])
        (if (null? (cddr rmaps))
            next
            (if (two-in-a-row?)
                (build-branches (cdr rmaps) next)
                (build-branches (cddr rmaps) next)))))
    (build-branches (cdr rmaps)
                    (with-syntax ([dest (cdar rmaps)]) #'(dest (unsafe-fx+ i 1)))))

  ;; (Listof (Pair Range Dest)) Boolean Dest -> Syntax-Object
  (define (build-bst rmaps)
    (let* ([part-ref (unsafe-fxquotient (length rmaps) 2)]
           [part (list-ref rmaps part-ref)]
           [low (caar part)]
           [hi (cdar part)]
           [going (cdr part)]
           [pair-parts (list-rmaps->partition rmaps low)]
           [less (car pair-parts)]
           [more (cdr pair-parts)])
      (with-syntax ([dest going] [l low] [h hi]
                    [next #'(unsafe-fx+ i 1)])
        (with-syntax ([this-range
                       (if (= hi low)
                           #'(if (unsafe-fx= n l) (dest next))
                           #'(if (unsafe-fxand (unsafe-fx<= n h)
                                               (unsafe-fx>= n l))
                                 (dest next)
                                 #f))])
          ;; Syntax-Transformation with slight optimizations
          (cond [(and (null? less) (null? more)) #'this-range]
                [(null? less)
                 (with-syntax ([upper-range (build-bst more)])
                   #'(if (unsafe-fx> n h)
                         upper-range
                         this-range))]
                [(null? more)
                 (with-syntax ([lower-range (build-bst less)])
                   #'(if (unsafe-fx< n l)
                         lower-range
                         this-range))]
                [#t
                 (with-syntax ([lower-range (build-bst less)]
                               [upper-range (build-bst more)])
                   #'(if (unsafe-fx> n h)
                         upper-range
                         (if (unsafe-fx< n l)
                             lower-range
                             ;; Lack of this-range (low < x < hi => x in range)
                             (dest next))))])))))


  ;; : (Listof (Pair Range Dest)) Boolean Dest-> Syntax-Object
  (define (rmaps->bst rmaps final? id)
    (if (null? rmaps) final?
        (if final? #t ;; These optimizations will be useful for returning a match ...
            (if (and final? (staying-here? rmaps id)) #t
                (if (loop-until-x? rmaps id)
                    (build-.*-loop rmaps id)
                    (if (series? rmaps)
                        (build-series rmaps)
                        (build-bst rmaps)))))))

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
                         (ormap (lambda (y) (equal? x (car y))) finals))]

               [transitions (clean-trans (dfa-transitions in) num)]
               ;; : (Pair <integer-set> Natural) -> (Listof RangeMapping)
               [build-range-map
                (lambda (pair)
                  (let ([outgoing-chars (is:foldr cons null (car pair))]
                        [dest (id-of (cdr pair))])
                    (to-ranges outgoing-chars dest)))]
               ;; : (Listof (Pair <integer-set> Natural)) -> (Listof RangeMapping)
               [sets->rmaps
                (lambda (pairs)
                  (foldl merge-range-dests null (map build-range-map pairs)))]
               ;; : (List Natural (Listof (Pair <integer-set> int)) -> Syntax-object
               [trans-expand
                (lambda (tlist)
                  (let ([accepts? (final? (car tlist))])
                    (with-syntax ([src (id-of (car tlist))])
                       (if accepts? #'[src (lambda (i) #t)]
                           (with-syntax ([empty-case (final? (car tlist))]
                                         [bst (rmaps->bst (sets->rmaps (cdr tlist)) accepts? (id-of (car tlist)))]
                                         [len strlen*] [string string*])
                             #'[src
                                (lambda (i)
                                  (if (unsafe-fx= i len) empty-case
                                      (let ([n (char->integer (unsafe-string-ref string i))])
                                        bst)))])))))])

          (with-syntax ([(nodes ...) (map trans-expand transitions)]
                        [start (id-of init)]
                        [n strlen*] [string string*])
             #'(lambda (string)
                 (let ([n (unsafe-string-length string)])
                   (if (unsafe-fx= n 0) #f
                       (letrec  (nodes ...)
                         (start 0)))))))))
)