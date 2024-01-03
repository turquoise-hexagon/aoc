(import
  (chicken fixnum)
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

(define (parse lst)
  (map
    (lambda (i)
      (apply
        (lambda (op . l)
          (cons (string->symbol op) (map convert l)))
        (string-split i " ")))
    lst))

(define-inline (get n)
  (if (number? n) n (vector-ref mem (car n))))

(define-inline (set n v)
  (vector-set! mem (car n) v))

(define (run vec a b v)
  (let ((mem (make-vector 4 0)) (limit (vector-length vec)))
    (set (address a) v)
    (let loop ((i 0) (out '()))
      (if (or (fx>= i limit) (fx> (length out) 20))
        (list (get (address b)) out)
        (apply
          (case-lambda
            ((op a)
             (case op
               ((inc) (set a (fx+ (get a) 1)) (loop (fx+ i 1) out))
               ((dec) (set a (fx- (get a) 1)) (loop (fx+ i 1) out))
               ((tgl)
                (let ((_ (fx+ i (get a))))
                  (unless (fx>= _ limit)
                    (apply
                      (case-lambda
                        ((op a)
                         (case op
                           ((inc) (vector-set! vec _ (list 'dec a)))
                           ((dec) (vector-set! vec _ (list 'inc a)))
                           ((tgl) (vector-set! vec _ (list 'inc a)))))
                        ((op a b)
                         (case op
                           ((cpy) (vector-set! vec _ (list 'jnz a b)))
                           ((jnz) (vector-set! vec _ (list 'cpy a b))))))
                      (vector-ref vec _))))
                (loop (fx+ i 1) out))
               ((out a)
                (loop (fx+ i 1) (cons (get a) out)))))
            ((op a b)
             (case op
               ((cpy)
                (set b (get a))
                (loop (fx+ i 1) out))
               ((jnz)
                (if (fx= (get a) 0)
                  (loop (fx+ i 1) out)
                  (loop (fx+ i (get b)) out))))))
          (vector-ref vec i))))))
