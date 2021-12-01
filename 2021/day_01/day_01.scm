(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input)
  (count < input (cdr input)))

(let ((input (import-input)))
  (print (solve input))
  (let ((input (map + input (cdr input) (cddr input))))
    (print (solve input))))
