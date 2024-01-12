(import
  (chicken io)
  (chicken string)
  (euler))

(define (parse i)
  (string->number (list-ref (string-split i " ") 2)))

(define (import-input)
  (let*
    ((l (read-lines))
     (a (parse (list-ref l 22)))
     (b (parse (list-ref l 24)))
     (m (+ 836 (* 22 a) b))
     (n (+ 10550400 m)))
    (values m n)))

(define (solve n)
  (apply + (divisors n)))

(let-values (((input/1 input/2) (import-input)))
  (let ((part/1 (solve input/1)))
    (print part/1) (assert (= part/1 2520)))
  (let ((part/2 (solve input/2)))
    (print part/2) (assert (= part/2 27941760))))
