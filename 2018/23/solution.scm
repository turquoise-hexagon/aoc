(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (apply
        (lambda (_ x y z _ r)
          (list r x y z))
        (map string->number (string-split i "=<,>"))))
    (read-lines)))

(define (distance a b)
  (apply + (map abs (map - a b))))

(define (solve input)
  (let ((center (extremum input car >)))
    (count
      (lambda (i)
        (> (car center) (distance (cdr center) (cdr i))))
      input)))

(let ((part (solve (import-input))))
  (print part) (assert (= part 595)))
