(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (foldr
    (lambda (i acc)
      (if (null? i)
        (cons '() acc)
        (cons (cons i (car acc)) (cdr acc))))
    '(()) (map string->list (read-lines))))

(define (compare a b)
  (count (lambda (a b) (not (char=? a b))) a b))

(define (reflection lst n)
  (find
    (lambda (i)
      (let-values (((a b) (split-at lst i)))
        (= (apply + (map compare (reverse a) b)) n)))
    (iota (sub1 (length lst)) 1)))

(define (score lst n)
  (cond
    ((reflection lst n) => (cut * <> 100))
    (else (reflection (apply zip lst) n))))

(define (solve input n)
  (apply + (map (lambda (i) (score i n)) input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= part/1 31956)))
  (let ((part/2 (solve input 1)))
    (print part/2) (assert (= part/2 37617))))
