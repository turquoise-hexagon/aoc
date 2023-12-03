(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input n)
  (count < input (drop input n)))

(let ((input (import-input)))
  (let ((part/1 (solve input 1)))
    (print part/1) (assert (= part/1 1387)))
  (let ((part/2 (solve input 3)))
    (print part/2) (assert (= part/2 1362))))
