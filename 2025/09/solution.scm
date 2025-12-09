(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ",")))
    (read-lines)))

(define (process l)
  (map
    (lambda (i)
      (bind (a b c d) (join i)
        (list (min a c) (min b d)
              (max a c) (max b d))))
    l))

(define (solve l)
  (let ((gs (process (zip l (foldr cons (list (car l)) (cdr l))))))
    (let loop ((rs (process (combinations l 2))) (p1 0) (p2 0))
      (if (null? rs)
        (list p1 p2)
        (bind (a b c d) (car rs)
          (let ((_ (* (- c a -1)
                      (- d b -1))))
            (loop (cdr rs)
              (max p1 _)
              (if (any
                    (lambda (i)
                      (bind (p q r s) i
                        (and
                          (< p c) (< q d)
                          (> r a) (> s b))))
                    gs)
                p2 (max p2 _)))))))))

(let* ((input (import-input)) (_ (solve input)))
  (for-each print _) (assert (equal? _ '(4741848414 1508918480))))
