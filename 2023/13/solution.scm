(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (foldr
    (lambda (i acc)
      (if (string=? i "")
        (cons '() acc)
        (cons (cons (string->list i) (car acc)) (cdr acc))))
    '(()) (read-lines)))

(define (reflection lst n)
  (find
    (lambda (i)
      (let-values (((a b) (split-at lst i)))
        (= (apply +
             (map
               (lambda (a b)
                 (count (lambda (a b) (not (char=? a b))) a b))
               (reverse a) b))
           n)))
    (iota (sub1 (length lst)) 1)))

(define (score lst n)
  (let*
    ((rot (apply zip lst))
     (a (let ((_ (reflection lst n))) (if _ _ 0)))
     (b (let ((_ (reflection rot n))) (if _ _ 0))))
    (+ (* 100 a) b)))

(define (solve input n)
  (apply + (map (lambda (i) (score i n)) input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= 31956)))
  (let ((part/2 (solve input 1)))
    (print part/2) (assert (= 37617))))
