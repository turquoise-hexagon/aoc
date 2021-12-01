(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input proc)
  (count < input (proc input)))

(let ((input (import-input)))
  (print (solve input cdr))
  (print (solve input cdddr))) 
