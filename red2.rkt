;; RED2 -- Attempt two at regular expression derivatives
(module red2 racket
  (require parser-tools/private-lex/re
           parser-tools/private-lex/deriv
           parser-tools/private-lex/util
           (prefix-in mz: mzlib/integer-set)
           racket/unsafe/ops
           (for-syntax racket
                       racket/unsafe/ops
                       (prefix-in mz: mzlib/integer-set))
           (for-template racket
                         racket/unsafe/ops))

  (provide (combine-out dfa-expand build-test-dfa
                        (all-from-out parser-tools/private-lex/re)
                        (all-from-out parser-tools/private-lex/deriv)))

  ;; (define-type SYNTAX (Syntaxof Any))

  (define (id x) x)

  (define (make-temps n) (list->vector (generate-temporaries (build-list n id))))
  (define (one-temp) (vector-ref (make-temps 1) 0))

  (define fast-stop? #t)
  (define char-min 0)
  (define char-max 1114111)
  (define series-max 12)
  (define (dispatched x) (integer->char x))

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
  ;; (: clean-trans ((Listof transitions) -> (Listof transitions)))
  (define (clean-trans trans num)
    (let loop ([lot trans] [n 0] [acc null])
      (cond [(and (null? lot) (eq? n num)) (reverse acc)]
            [(and (not (null? lot)) (eq? n (caar lot)))
             (loop (cdr lot) (add1 n) (cons (car lot) acc))]
            [(empty? lot) (loop lot (add1 n) (cons (cons n null) acc))]
            [else (loop (cdr lot) (add1 n) (cons (cons n null)) acc)])))


  ;; (: quick-accpet (temporary -> SYNTAX))
  (define (quick-accept x) (with-syntax ([src x]) #'[src (lambda (i) #t)]))


  ;; (: trans-expand (Temporaryx3 Boolean
  ;;      (List Natural (listof (Pair integer-set Nubmer))) (Vector temporaries)) -> SYNTAX))
  (define (trans-expand* state string length final? tlist ids)
    (if (and final? fast-stop?)
        (quick-accept state)
        (with-syntax ([base-case final?] [src state]
                      [dispatch (dispatch-on state final? (make-rmps (cdr tlist) ids))]
                      [len length] [str string])
                     #'[src (lambda (i)
                              (if (unsafe-fx= i len) base-case
                                  (let ([n (unsafe-string-ref str i)])
                                    dispatch)))])))

  ;; (: trans-helper (Temprorary Temporary ->
  ;;    (Temporary Boolean (List (Natural (Listof (Pair integer-set Number)))) -> SYNTAX)))
  (define (trans-helper string length ids)
    (lambda (state final? tlist) (trans-expand* state string length final? tlist ids)))


  ;; (: make-rmps ((Listof (Pair integer-set Number)) (Vector temporaries) -> SYNTAX))
  (define (make-rmps edges-list ids)
    (define (build-range-map pair)
      (let ([outgoing-chars (mz:integer-set-contents (car pair))]
            [dest (vector-ref ids (cdr pair))])
        (to-ranges outgoing-chars dest)))
    (foldl merge-range-dests null (map build-range-map edges-list)))

  ;; A Range is either a singleton or Number or range of Number Number
  ;; A RangeMapping is a (RangeMapping range temporary)
  (define-struct singleton (val))
  (define-struct range (lo hi))

  ;; (: range-min (Range -> Number))
  (define (range-min A)
    (cond [(singleton? A) (singleton-val A)]
          [(range? A) (range-lo A)]
          [else (error 'range-min "expected a range, given ~s" A)]))

  ;; (: range-max (Range -> Number))
  (define (range-max A)
    (cond [(singleton? A) (singleton-val A)]
          [(range? A) (range-hi A)]
          [else (error 'range-max "expected a range, given ~s" A)]))

  ;; (: range<= (Range Range -> Boolean))
  (define (range<= A B) (<= (range-min A) (range-min B)))

  ;; (: build-range ((Pair Number Number) -> Range
  (define (build-range pair)
    (let ([a (car pair)] [b (cdr pair)])
      (cond [(= a b) (make-singleton a)]
            [(< a b) (make-range a b)]
            [else (error 'build-range "expected ~s < ~s" a b)])))

  ;; (: to-ranges ((Listof (Pair Number Number)) temporary) -> (Listof RangeMapping))
  (define (to-ranges pairs dest)
    (if (null? pairs) (error 'to-ranges "Given an empty series to turn to ranges")
        (let loop ([lon pairs] [acc null])
          (if (null? lon) (reverse acc)
              (loop (cdr lon) (cons (cons (build-range (car lon)) dest) acc))))))

  ;; (: merge-ranges-dests ((Listof RangeMapping) (Listof RangeMapping) -> (Listof RangeMapping)))
  (define (merge-range-dests A B)
    (let loop ([A A] [B B] [acc null])
      (cond [(null? A) (append (reverse acc) B)]
            [(null? B) (append (reverse acc) A)]
            [(range<= (caar A) (caar B))
             (loop (cdr A) B (cons (car A) acc))]
            [else (loop A (cdr B) (cons (car B) acc))])))

  ;; (: dispatch-on (temporary boolean (Listof RangeMapping) -> SYNTAX))
  (define (dispatch-on state final? rmaps) (dispatch-one state final? rmaps))

  ;; (: dispatch-one (temporary boolean (Listof RangeMapping) -> SYNTAX))
  (define (dispatch-one state final? rmaps)
    (if (null? rmaps) final?
        (if (and final? (or fast-stop? (staying-here? rmaps state)))
            #f
            (if (loop-until-x? rmaps state)
                (build-.*-loop rmaps state)
                (if (series? rmaps)
                    (build-series rmaps)
                    (build-bst rmaps))))))

  (define (same-id? a b) (bound-identifier=? a b))

  ;; (: staying-here? ((Listof RangeMapping) Temporary -> Boolena))
  (define (staying-here? rmaps id) (andmap (lambda (pair) (same-id? (cdr pair id))) rmaps))

  ;; (: loop-until-x? ((Listof RangeMapping) Temporary -> Boolean))
  (define (loop-until-x? rmaps id)
    (and (= 3 (length rmaps))
         (let ([a (car rmaps)] [b (cadr rmaps)] [c (caddr rmaps)])
           (let ([ra (car a)] [rb (car b)] [rc (car c)])
             (and (singleton? rb)
                  (= char-min (range-min ra))
                  (= char-max (range-max rb))
                  (= (add1 (range-max ra))
                        (singleton-val rb)
                        (sub1 (range-min rc)))
                  (same-id? (cdr a) id)
                  (same-id? (cdr c) id))))))

  ;; (: build-.*-loop ((Listof RangeMapping) Temporary -> SYNTAX))
  (define (build-.*-loop rmaps id)
    (let ([outmap (cadr rmaps)])
      (unless (singleton? outmap) (error 'build-.*-loop "expected a singletoin"))
      (with-syntax ([dest (cdr rmaps)]
                    [num (dispatched (singleton-val outmap))]
                    [this id]
                    [next #'(unsafe-fx+ i 1)])
        #'(if (unsafe-fx= n num)
               (dest next)
               (this next)))))

  ;; (: series? ((Listof RangeMapping) -> Boolean))
  (define (series? rmaps)
    (define (series*? rmaps x id)
      (define (first-two-same?)
        (and (singleton? (caar rmaps))
             (= (add1 x) (singleton-val (caar rmaps)) (range-min (caadr rmaps)))
             (same-id? id (cdadr rmaps))))
      (define (two-in-a-row?)
        (and (singleton? (caar rmaps)) (singleton? (caadr rmaps))
             (= (add1 x) (singleton-val (caar rmaps))
                (sub1 (singleton-val (caadr rmaps))))))
      (and (not (null? (cdr rmaps)))
          (if (first-two-same?)
              (if (null? (cddr rmaps))
                  (= char-max (range-max (caadr rmaps)))
                  (series*? (cddr rmaps) (range-max (caadr rmaps)) id))
              (and (two-in-a-row?) (series*? (cdr rmaps) (range-min (caar rmaps)) id)))))
    (and (not (null? rmaps))
         (let ([r (caar rmaps)] [id (cdar rmaps)])
           (and (>= (length rmaps) 3)
                (not (singleton? r))
                (= (range-min r) char-min)
                (series*? (cdr rmaps) (range-max r) id)))))

  ;; (: build-series ((Listof RangeMapping) -SYNTAX))
  (define (build-series rmaps)
    (define (build-branches rmaps acc)
      (define (two-in-a-row?) (singleton? (caadr rmaps)))
      (let ([next (with-syntax ([val (dispatched (range-min (caar rmaps)))]
                                [dest (cdar rmaps)] [base acc])
                    #'(if (unsafe-fx= n val) (dest (unsafe-fx+ i 1)) base))])
        (if (null? (cddr rmaps)) next
            (if (two-in-a-row?)
                (build-branches (cdr rmaps) next)
                (build-branches (cddr rmaps) next)))))
    (build-branches (cdr rmaps)
                   (with-syntax ([dest (cdar rmaps)]) #'(dest (unsafe-fx+ i 1)))))


  ;; (: list-rmaps->partition ((Listof RangeMapping) Number)
  ;;       -> (Pair (Listof RangeMapping) (Listof RangeMapping)))
  (define (list-rmaps->partitions rmaps part)
    (let loop ([rmaps rmaps] [acc null])
      (if (null? rmaps) (cons (reverse acc) null)
          (let ([start (range-min (caar rmaps))])
            (cond [(< start part) (loop (cdr rmaps) (cons (car rmaps) acc))]
                  [(= start part) (loop (cdr rmaps) acc)]
                  [else (cons (reverse acc) rmaps)])))))

  ;; (: build-bst ((Listof RangeMapping) -> SYNTAX))
  (define (build-bst rmaps)
    (let* ([part-ref (unsafe-fxquotient (length rmaps) 2)]
           [part (list-ref rmaps part-ref)]
           [low (range-min (car part))]
           [hi (range-max (car part))]
           [going (cdr part)]
           [pair-parts (list-rmaps->partitions rmaps low)]
           [less (car pair-parts)] [more (cdr pair-parts)])
      (with-syntax ([dest going] [l (dispatched low)] [h (dispatched hi)]
                    [next #'(unsafe-fx+ i 1)])
        (with-syntax ([this-range
                       (if (singleton? (car part))
                           #'(unsafe-fxand (unsafe-fx= n l) (dest next))
                           #'(unsafe-fxand (unsafe-fx <= n h)
                                           (unsafe-fx >= n l)
                                           (dest next)))])
           (cond [(and (null? less) (null? more)) #'this-range]
                 [(null? less)
                  (with-syntax ([upper-range (build-bst more)])
                    #'(if (unsafe-fx> n h) upper-range this-range))]
                 [(null? more)
                  (with-syntax ([lower-range (build-bst less)])
                    #'(if (unsafe-fx< n l) lower-range this-range))]
                 [else
                  (with-syntax ([upper-range (build-bst more)]
                                [lower-range (build-bst less)])
                    #'(if (unsafe-fx> n h)
                          upper-range
                          (if (unsafe-fx< n l)
                              lower-range
                              (dest next))))])))))

  (define (make-finals finals num)
    (let loop ([finals finals] [nums (build-list num id)] [acc null])
      (cond [(null? nums) (list->vector (reverse acc))]
            [(null? finals) (loop null (cdr nums) (cons #f acc))]
            [(< (caar finals) (car nums)) (loop (cdr finals) nums acc)]
            [(= (caar finals) (car nums)) (loop (cdr finals) (cdr nums) (cons #t acc))]
            [else (loop finals (cdr nums) (cons #f acc))])))

  ;; (: dfa-expand (dfa -> SYNTAX))
  (define (dfa-expand in)
    (unless (dfa? in) (error 'dfa-epxand "expected a dfa given: ~s" in))
    (let* ([num (dfa-num-states in)]
           [init (dfa-start-state in)]
           [string* (one-temp)]
           [strlen* (one-temp)]
           [state-ids (make-temps num)]
           [transitions (clean-trans (dfa-transitions in) num)]
           [id-of (lambda (x) (vector-ref state-ids x))]
           [trans-expand (trans-helper string* strlen* state-ids)]
           [finals (make-finals (dfa-final-states/actions in) num)]
           [0tonums (build-list num id)]
           )
      (with-syntax ([(nodes ...)
                     (map trans-expand
                          (map id-of 0tonums)
                          (map (lambda (x) (vector-ref finals x)) 0tonums)
                          transitions)]
                    [n strlen*] [string string*]
                    [start (id-of init)])
        #'(lambda (string)
            (let ([n (unsafe-string-length string)])
              (if (unsafe-fx= n 0) #f
                  (letrec (nodes ...)
                    (start 0))))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; For output / testing

  (define (build-and-print x)
    (print-dfa (build-test-dfa x)))

  (define (build-test-dfa rs)
    (let ((c (make-cache)))
      (build-dfa (map (lambda (x) (cons (->re x c) 'action))
                      rs)
                 c)))


)