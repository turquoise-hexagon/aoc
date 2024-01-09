(import
  (srfi 1))

(define (import-input)
  (read))

(define (solve input iterations target)
  (let ((acc (circular-list 0)))
    (do ((i 1 (+ i 1)))
        ((> i iterations) (cadr (member target acc)))
      (let* ((tail (list-tail acc input)) (next (cons i (cdr tail))))
        (set-cdr! tail next)
        (set!      acc next)))))

(let ((input (import-input)))
  (let ((part/1 (solve input 2017 2017)))
    (print part/1) (assert (= part/1 1487)))
  (let ((part/2 (solve input #e5e7 0)))
    (print part/2) (assert (= part/2 25674054))))
