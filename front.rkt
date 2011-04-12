(module front racket
  (require racket/unsafe/ops)
  (provide (all-defined-out))

  ;; use a finite-state-machine to turn a list required
  ;; for input to build-test-dfa
  (define (to->pre-dfa string)
    (let ([length (unsafe-string-length string)]
          [depth (box 0)])
      (letrec
          ([start
            (lambda (acc index)
              (if (< index length)
                  (let ([char (unsafe-string-ref string index)])
                    (case char
                      [(#\*) (start (cons (make-star (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\+) (start (cons (make-plus (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\|) (let ([reg2 (start '() (unsafe-fx+ index 1))])
                               (make-or (make-concat acc) reg2))]
                      [(#\&) (let ([reg2 (start '() (unsafe-fx+ index 1))])
                               (make-and (make-concat acc) reg2))]
                      [(#\() (begin
                               (set-box! depth (unsafe-fx+ (unbox depth) 1))
                               (let ([res (start '() (unsafe-fx+ index 1))])
                                 (start (cons (car res) acc) (cdr res))))]
                      [(#\)) (if (> (unbox depth) 0)
                                 (begin
                                   (set-box! depth (unsafe-fx- (unbox depth) 1))
                                   (cons (make-concat acc) (unsafe-fx+ index 1)))
                                 (error 'regexp-make "too many close parens! in ~s" string))]
                      [(#\{) (let ([res (get-repetition (unsafe-fx+ index 1))])
                               (start (cons (make-repeat (car acc) (first res) (second res))
                                            (cdr acc))
                                      (unsafe-fx+ (third res) 1)))]
                      [(#\^) (start (cons (make-complement (car acc)) (cdr acc))
                                    (unsafe-fx+ index 1))]
                      [(#\\) (start (cons (get-escaped index) acc) (unsafe-fx+ index 2))]
                      [(#\.) (start (cons anything acc) (unsafe-fx+ index 1))]
                      [else (start (cons char acc) (unsafe-fx+ index 1))]))
                  (if (= (unbox depth) 0)
                      (make-concat acc)
                      (error 'regexp-make "not enough closing parens! in ~s" string))))]

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

)

        (if (unsafe-fx= length 0)
            (error 'regexp-make "empty regular expression!")
            (let ([hd (unsafe-string-ref string 0)])
              (if (unsafe-fx= hd #\^)
                  (list (start (make-star anything) 1))
                  (list (start '() 0))))))))


  (define anything (list 'char-complement))

  (define (make-star char)
    (list 'repetition 0 +inf.0 char))

  (define (make-plus char)
    (list 'repetition 1 +inf.0 char))

  (define (make-concat lst)
    (if (= (length lst) 1) (car lst)
        (cons 'concatenation (reverse lst))))

  (define (make-or l1 l2)
    (list 'union l1 l2))

  (define (make-and l1 l2)
    (list 'intersection l1 l2))

  (define (make-complement t)
    (list 'char-complement t))

  (define (make-repeat t lo hi)
    (list 'repetition lo hi t))

  (define (add-num char num)
    (let ([num? (unsafe-fx- (char->integer char) 48)])
      (if (<= num? 9) (unsafe-fx+ (unsafe-fx* 10 num) num?)
          (error 'regexp-make "not a valid number in ~s" char))))






)
