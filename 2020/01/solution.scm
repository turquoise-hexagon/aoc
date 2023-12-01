(import
  (chicken io)
  (euler)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input n)
  (apply *
    (find
      (lambda (lst)
        (= (apply + lst) 2020))
      (combinations input n))))

(let* ((input (import-input))
       (part/1 (solve input 2))
       (part/2 (solve input 3)))
  (print part/1) (assert (= part/1 224436))
  (print part/2) (assert (= part/2 303394260)))
