(import
  (chicken fixnum))

(define (_id a b)
  (let*
    ((a (length a))
     (b (length b))
     (_ (fx+ a b)))
    (fx+ (fx/ (fx* _ (fx+ _ 1)) 2) b)))

(define (_drop l n)
  (cond
    ((null? l) l)
    ((fx= n 0) l)
    (else (_drop (##sys#slot l 1) (fx- n 1)))))

(define (_take l n)
  (cond
    ((null? l) '())
    ((fx= n 0) '())
    (else (cons (##sys#slot l 0) (_take (##sys#slot l 1) (fx- n 1))))))

(define (_list-ref l n #!optional (i #f))
  (cond
    ((null? l) i)
    ((fx= n 0) (##sys#slot l 0))
    (else (_list-ref (##sys#slot l 1) (fx- n 1)))))
