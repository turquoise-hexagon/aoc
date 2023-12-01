(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define (import-input)
  (sort
    (foldl
      (lambda (acc i)
        (if i (cons (+ (car acc) i) (cdr acc))
          (cons 0 acc)))
      '(0) (map string->number (read-lines)))
    >))

(define (solve input n)
  (apply + (take input n)))

(let* ((input (import-input))
       (part/1 (solve input 1))
       (part/2 (solve input 3)))
  (print part/1) (assert (= part/1 67027))
  (print part/2) (assert (= part/2 197291)))
