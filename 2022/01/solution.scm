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

(let ((input (import-input)))
  (print (solve input 1))
  (print (solve input 3)))
