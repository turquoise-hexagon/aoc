(import
  (chicken io)
  (euler))

(define (import-input)
  (number->list (string->number (read-line))))

(define (solve input iterations)
  (length
    (foldl
      (lambda (acc i)
        (join
          (map
            (lambda (i)
              (list (car i) (cdr i)))
            (run-length acc))))
      input (range 1 iterations))))

(let ((input (import-input)))
  (let ((part/1 (solve input 40)))
    (print part/1) (assert (= part/1 252594)))
  (let ((part/2 (solve input 50)))
    (print part/2) (assert (= part/2 3579328))))
