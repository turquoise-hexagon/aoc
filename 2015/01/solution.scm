(import
  (chicken io))

(define (import-input)
  (map
    (lambda (i)
      (case i
        ((#\() +1)
        ((#\)) -1)))
    (string->list (read-line))))

(define (solve/1 input)
  (apply + input))

(define (solve/2 input)
  (let loop ((lst input) (sum 0) (acc 0))
    (if (= sum -1)
      acc
      (loop (cdr lst) (+ sum (car lst)) (+ acc 1)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 138)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1771))))
