(import
  (chicken io)
  (euler)
  (srfi 1))

(define (import-input)
  (let ((lst (map string->number (read-lines))))
    (filter
      (lambda (i)
        (= (apply + i) 150))
      (join
        (map
          (lambda (i)
            (combinations lst i))
          (range 1 (length lst)))))))

(define (solve/1 input)
  (length input))

(define (solve/2 input)
  (let ((minimum (apply min (map length input))))
    (count
      (lambda (i)
        (= (length i) minimum))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 4372)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 4))))
