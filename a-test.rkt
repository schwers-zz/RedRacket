(module a-test racket
    (require racket/unsafe/ops)

    (define a*-only
      (lambda (input)
        (letrec ((length (unsafe-string-length input))
                 (loop
                  (lambda (i n)
                    (if (unsafe-fx= i length) #t
                        (let* ((a (unsafe-fx+ i 1))
                               (b (char->integer (unsafe-string-ref input a))))
                          (if (unsafe-fx= n 97) (loop a b) #f))))))
          (loop 0 (char->integer (unsafe-string-ref input 0))))))

    (define a*-only-2
      (lambda (input)
        (letrec ((length (unsafe-string-length input))
                 (loop
                  (lambda (i)
                    (if (unsafe-fx= i length) #t
                        (let ((n (char->integer (unsafe-string-ref input i))))
                          (if (unsafe-fx= n 97) (loop (unsafe-fx+ i 1)) #f))))))
          (loop 0))))


    (define a*-only-3
      (lambda (input)
        (let ((length (unsafe-string-length input)))
          (if (unsafe-fx= length 0)
              #f
              (letrec ((accept (lambda (i) #t))) (accept 0))))))

    (define a+paren-edited
      (lambda (input)
        (let ((length (unsafe-string-length input)))
          (if (unsafe-fx= length 0)
              #f
              (letrec ((state1
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (if (unsafe-fx= n 40) (state2 (unsafe-fx+ i 1)) #f)))))
                       (state2
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (if (unsafe-fx= n 97) (state3 (unsafe-fx+ i 1)) #f)))))
                       (state3
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (if (unsafe-fx= n 41)
                                    (state4 (unsafe-fx+ i 1))
                                    (and (unsafe-fx= n 97) (state3 (unsafe-fx+ i 1))))))))
                       (state4 (lambda (i) #t)))
                (state1 0))))))

    (define a+paren-compiled
      (lambda (input)
        (let ((length (unsafe-string-length input)))
          (if (unsafe-fx= length 0)
              #f
              (letrec ((state1
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (and (unsafe-fx= n 40) (state2 (unsafe-fx+ i 1)))))))
                       (state2
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (and (unsafe-fx= n 97) (state3 (unsafe-fx+ i 1)))))))
                       (state3
                        (lambda (i)
                          (if (unsafe-fx= i length)
                              #f
                              (let ((n (char->integer (unsafe-string-ref input i))))
                                (if (unsafe-fx< n 97)
                                    (and (unsafe-fx= n 41) (state4 (unsafe-fx+ i 1)))
                                    (and (unsafe-fx= n 97) (state3 (unsafe-fx+ i 1))))))))
                       (state4 (lambda (i) #t)))
                (state1 0))))))

    (define a+paren-cased
      (lambda (input)
        (let ((length (unsafe-string-length input)))
          (if (unsafe-fx= length 0) #f
              (let loop ([state 1][pos 0])
                (case state
                  [(1) (if (unsafe-fx= pos length) #f
                           (let ((n (char->integer (unsafe-string-ref input pos))))
                             (case n
                               [(40) (loop 2 (unsafe-fx+ pos 1))]
                               [else #f])))]
                  [(2) (if (unsafe-fx= pos length) #f
                           (let ((n (char->integer (unsafe-string-ref input pos))))
                             (case n
                               [(97) (loop 3 (unsafe-fx+ pos 1))]
                               [else #f])))]
                  [(3) (if (unsafe-fx= pos length) #f
                           (let ((n (char->integer (unsafe-string-ref input pos))))
                             (case n
                               [(97) (loop 3 (unsafe-fx+ pos 1))]
                               [(41) (loop 4 (unsafe-fx+ pos 1))]
                               [else #f])))]
                  [(4) #t]))))))


)







