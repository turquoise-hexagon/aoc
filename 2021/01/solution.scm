(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input n)
  (count < input (drop input n)))

(let ((input (import-input)))
  (print (solve input 1))
  (print (solve input 3)))
