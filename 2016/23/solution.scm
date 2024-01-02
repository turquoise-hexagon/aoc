(import
  (chicken fixnum)
  (chicken io)
  (chicken string))

(define (address n)
  (case (string->symbol n)
    ((a) '(0))
    ((b) '(1))
    ((c) '(2))
    ((d) '(3))))

(define (convert n)
  (let ((r (string->number n)))
    (if r r (address n))))

(define (import-input)
  (map
    (lambda (i)
      (apply
        (lambda (op . l)
          (cons (string->symbol op) (map convert l)))
        (string-split i " ")))
    (read-lines)))

(define-inline (get n)
  (if (number? n) n (vector-ref acc (car n))))

(define-inline (set n v)
  (vector-set! acc (car n) v))

(define (solve input init)
  (let ((acc (make-vector 4 0)) (limit (vector-length input)))
    (set (address "a") init)
    (let loop ((i 0))
      (if (fx>= i limit)
        (get (address "a"))
        (apply
          (case-lambda
            ((op a)
             (case op
               ((inc) (set a (fx+ (get a) 1)) (loop (fx+ i 1)))
               ((dec) (set a (fx- (get a) 1)) (loop (fx+ i 1)))
               ((tgl)
                (let ((_ (fx+ i (get a))))
                  (unless (fx>= _ limit)
                    (apply
                      (case-lambda
                        ((op a)
                         (case op
                           ((inc) (vector-set! input _ (list 'dec a)))
                           ((dec) (vector-set! input _ (list 'inc a)))
                           ((tgl) (vector-set! input _ (list 'inc a)))))
                        ((op a b)
                         (case op
                           ((cpy) (vector-set! input _ (list 'jnz a b)))
                           ((jnz) (vector-set! input _ (list 'cpy a b))))))
                      (vector-ref input _))))
                (loop (fx+ i 1)))))
            ((op a b)
             (case op
               ((cpy)
                (set b (get a))
                (loop (fx+ i 1)))
               ((jnz)
                (if (fx= (get a) 0)
                  (loop (fx+ i 1))
                  (loop (fx+ i (get b))))))))
          (vector-ref input i))))))

(let ((input (import-input)))
  (let ((part/1 (solve (list->vector input) 7)))
    (print part/1) (assert (= part/1 10880)))
  (let ((part/2 (solve (list->vector input) 12)))
    (print part/2) (assert (= part/2 479007440))))
