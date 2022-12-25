(import
  (chicken io))

(define (import-input)
  (read-lines))

(define (snafu->int str)
  (foldl
    (lambda (acc digit)
      (+ (case digit
           ((#\2)  2)
           ((#\1)  1)
           ((#\0)  0)
           ((#\-) -1)
           ((#\=) -2))
         (* acc 5)))
    0 (string->list str)))

(define (int->snafu n)
  (let loop ((n n) (acc '()))
    (if (= n 0)
      (list->string acc)
      (case (modulo n 5)
        ((2) (loop (quotient (- n 2) 5) (cons #\2 acc)))
        ((1) (loop (quotient (- n 1) 5) (cons #\1 acc)))
        ((0) (loop (quotient (+ n 0) 5) (cons #\0 acc)))
        ((4) (loop (quotient (+ n 1) 5) (cons #\- acc)))
        ((3) (loop (quotient (+ n 2) 5) (cons #\= acc)))))))

(define (solve input)
  (int->snafu (apply + (map snafu->int input))))

(let ((input (import-input)))
  (print (solve input)))
