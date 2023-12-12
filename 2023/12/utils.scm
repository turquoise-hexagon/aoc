(define (_drop l n)
  (cond
    ((null? l) l)
    ((zero? n) l)
    (else (_drop (cdr l) (- n 1)))))

(define (_take l n)
  (cond
    ((null? l) '())
    ((zero? n) '())
    (else (cons (##sys#slot l 0) (_take (cdr l) (- n 1))))))

(define (_list-ref l n)
  (cond
    ((null? l) #f)
    ((zero? n) (##sys#slot l 0))
    (else (_list-ref (cdr l) (- n 1)))))
