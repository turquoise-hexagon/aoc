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

(define (process lst)
  (map
    (lambda (i)
      (bind (a b c d) (join i)
        (list (min a c) (min b d)
              (max a c) (max b d))))
    lst))

(define (solve input)
  (let ((temp (process (zip input (foldr cons (list (car input)) (cdr input))))))
    (let loop ((input (process (combinations input 2))) (p1 0) (p2 0))
      (if (null? input)
        (list p1 p2)
        (bind (a b c d) (car input)
          (let ((area (* (- c a -1)
                         (- d b -1))))
            (loop (cdr input)
              (max p1 area)
              (if (any
                    (lambda (i)
                      (bind (p q r s) i
                        (and
                          (< p c) (< q d)
                          (> r a) (> s b))))
                    temp)
                p2 (max p2 area)))))))))

(let* ((input (import-input)) (_ (solve input)))
  (for-each print _) (assert (equal? _ '(4741848414 1508918480))))
