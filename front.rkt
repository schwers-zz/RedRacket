(module front racket
  (require racket/unsafe/ops)

  (provide (all-defined-out))


  ;; The finite-state-machine to turn a list required for build-ing
  ;; a dfa, lets call them a redstring
  (define (->redstring string)
    (let ([length (unsafe-string-length string)]
          [depth (box 0)]) ;; Keep track of nested syntax like (regexp)
      (letrec
          ([start
            ;; The main state, does most jof the work using convenience functions
            (lambda (acc index)
              (if (< index length)
                  (let ([char (unsafe-string-ref string index)])
                    (case char
                      [(#\\) (start (cons (get-escaped index) acc) (unsafe-fx+ index 2))]
                      [(#\*) (start (cons (make-star (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\+) (start (cons (make-plus (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\|) (let ([res (start '() (unsafe-fx+ index 1))])
                               (make-result res acc make-or))]
                      [(#\&) (let ([res (start '() (unsafe-fx+ index 1))])
                               (make-result res acc make-and))]
                      ;; When we encounter a parenthesized expression, increase
                      ;; the depth, and then parse the regexp.
                      ;; NOTE THE TRICKERY : when we find a close paren,
                      ;; the result is to return a (Pair redstring Natural)
                      ;; so the position of where to start parsing next is maintained
                      [(#\() (begin
                               (set-box! depth (unsafe-fx+ (unbox depth) 1))
                               (let ([res (start '() (unsafe-fx+ index 1))])
                                 (start (cons (car res) acc) (cdr res))))]
                      ;; AGAIN TRICKERY : the position of where to start parsing
                      ;; next is returned along with the result of the parenthesized
                      ;; regexp.
                      [(#\)) (if (> (unbox depth) 0)
                                 (begin
                                   (set-box! depth (unsafe-fx- (unbox depth) 1))
                                   (cons (make-concat acc) (unsafe-fx+ index 1)))
                                 (error 'regexp-make "too many close parens! in ~s" string))]
                      ;; Use a helper function, which will validate the {Num,Num} Syntax
                      ;; and build the appropriate repeition.
                      [(#\{) (let ([res (get-repetition (unsafe-fx+ index 1))])
                               (start (cons (make-repeat (car acc) (first res) (second res))
                                            (cdr acc))
                                      (unsafe-fx+ (third res) 1)))]
                      ;; User a helper function to deal with ranges enclosed in [_somerange_]
                      [(#\[) (let ([res (get-range '() (unsafe-fx+ index 1) #t)])
                               (start (cons (car res) acc) (cdr res)))]
                      [(#\^) (start (cons (make-complement (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\.) (start (cons anything acc) (unsafe-fx+ index 1))]
                      [else (start (cons char acc) (unsafe-fx+ index 1))]))
                  (if (= (unbox depth) 0)
                      (make-concat acc)
                      (error 'regexp-make "not enough closing parens! in ~s" string))))]

           ;; Mostly convenience helper functions for dealing with
           ;; operations resembling seperate states of the machine.
           [get-escaped
            (lambda (index)
              (let ([i (unsafe-fx+ index 1)])
                (if (< i length) (unsafe-string-ref string i)
                    (error 'regexp-make "unfinisehd escape character in ~s" string))))]

           [get-repetition
            (lambda (index)
              (let* ([r1 (get-num-until #\, index 0)]
                     [r2 (get-num-until #\} (cdr r1) 0)])
                (if (> (car r1) (car r2))
                    (error 'make-regexp "invalid range for repetition")
                    (list (car r1) (car r2) (cdr r2)))))]

           [get-num-until
            (lambda (sym index acc)
              (if (< index length)
                  (let ([char (unsafe-string-ref string index)])
                    (if (unsafe-fx= sym char) (cons acc (unsafe-fx+ index 1))
                        (get-num-until sym (unsafe-fx+ index 1) (add-num char acc))))
                  (error 'regexp-make "unfinished syntax! in ~s")))]

           [get-range
            (lambda (acc index first?)
              ;; after the first time, we're no longer on the first character
              ;; this is just to keep track of making the complmenet of ranges
              (let ([get-range (lambda (acc index) (get-range acc index #f))])
              (if (< index length)
                  (let ([char (unsafe-string-ref string index)])
                    (case char
                      [(#\\) (get-range (cons (get-escaped index) acc) (unsafe-fx+ index 2))]
                      [(#\]) (cons (make-union acc) (unsafe-fx+ index 1))]
                      [(#\^) (if first? (let ([res (get-range acc (unsafe-fx+ index 1))])
                                          (cons (make-complement (car res)) (cdr res)))
                                 (error 'regexp-make "can't use non escapse ^ in a range when its not the first char"))]
                      [(#\-) (let ([res (range* (car acc) (unsafe-fx+ index 1))])
                               (get-range (cons (car res) (cdr acc)) (cdr res)))]
                      [else  (get-range (cons char acc) (unsafe-fx+ index 1))]))
                  (error 'regexp-make "unfinished range-expression in regexp"))))]

           [range*
            (lambda (c index)
              (if (< index length)
                  (let ([char (unsafe-string-ref string index)])
                    (cons (make-range c char) (unsafe-fx+ index 1)))
                  (error 'regexp-make "unfinished range expression in regexp")))]
)

           ;; Helper functions to deal with the regexp starting with ^ and ending with $

           ;; starts-^? -- might change the acc to (make-start anything) and the init pos to 1
           (define (starts-^? initacc init)
             (if (unsafe-fx= (unsafe-string-ref string 0) #\^)
                 (begin (set-box! initacc '())
                        (set-box! init 1))
                 void))

           ;; ends-$? -- might change the the wrapup function to not add a .* to the end
           ;; also might decrease the checked length of the string to not include it
           (define (ends-$? wrapup)
             (if (and (unsafe-fx=
                       (unsafe-string-ref string (unsafe-fx- length 1))
                       #\$)
                      (and (unsafe-fx>= length 3)
                           (not (unsafe-fx=
                             (unsafe-string-ref string (unsafe-fx- length 2))
                             #\\))))
                 (begin (set! length (unsafe-fx- length 1))
                        (set-box! wrapup (lambda (x) (list x))))
                 void))

        ;; Start of the machine, if there's no ^ at the beginning of the machine
        ;; then bake-in a .* at the front. If the end of the machine has a $,
        ;;_don't_ bake in a .* at the end.
        (if (unsafe-fx= length 0)
            (error 'regexp-make "empty regular expression!")
            (let ([hd (unsafe-string-ref string 0)]
                  [tl (unsafe-string-ref string (unsafe-fx- length 1))]
                  [initacc (box (list (make-star anything)))] [init (box 0)]
                  [wrapup
                   (box (lambda (x) (append (list x) (list (make-star anything)))))])

              (starts-^? initacc init)
              (ends-$? wrapup)
              ((unbox wrapup) (start (unbox initacc) (unbox init))))))))




  ;; Helper functions that build the actual redstrings
  (define anything (list 'char-complement))
  (define space (char->integer (unsafe-string-ref "  " 1)))

  (define (make-star char)
    (list 'repetition 0 +inf.0 char))

  (define (make-plus char)
    (list 'repetition 1 +inf.0 char))

  (define (make-concat lst)
    (if (= (length lst) 1)
        (car lst)
        (cons 'concatenation (reverse lst))))

  (define (make-or l1 l2)
    (list 'union l1 l2))

  (define (make-and l1 l2)
    (list 'intersection l1 l2))

  (define (make-union lst)
    (if (unsafe-fx= 1 (length lst))
        (car lst)
        (cons 'union (reverse lst))))

  (define (make-range c1 c2)
    (and (validate-range c1 c2)
         (list 'char-range c1 c2)))

  (define (validate-range c1 c2)
    (or (and (unsafe-fx< c1 c2)
             (not (unsafe-fx= c1 space))
             (not (unsafe-fx= c1 space)))
        (error 'regepx-make "invalid range syntax")))

  ;; To deal with close parens...
  (define (make-result res acc function)
    (if (pair? res)
        (cons (function (make-concat acc) (car res)) (cdr res))
        (function (make-concat acc) res)))

  (define (make-complement t)
    (list 'char-complement t))

  (define (make-repeat t lo hi)
    (list 'repetition lo hi t))

  ;; Assumes numbers are parsed left->right in base 10...
  (define (add-num char num)
    (let ([num? (unsafe-fx- (char->integer char) 48)])
      (if (<= num? 9) (unsafe-fx+ (unsafe-fx* 10 num) num?)
          (error 'regexp-make "not a valid number in ~s" char))))



)
