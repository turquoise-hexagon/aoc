(import
  (chicken io)
  (euler)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input n)
  (apply * (find
             (lambda (lst)
               (= (apply + lst) 2020))
             (combinations input n))))

(let ((input (import-input)))
  (print (solve input 2))
  (print (solve input 3)))
