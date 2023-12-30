(import
  (chicken io)
  (euler)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input n)
  (let ((t (quotient (apply + input) n)))
    (let loop ((i 1))
      (let ((_ (find
                 (lambda (i)
                   (= (apply + i) t))
                 (combinations input i))))
        (if _
          (apply * _)
          (loop (+ i 1)))))))

(let ((input (import-input)))
  (let ((part/1 (solve input 3)))
    (print part/1) (assert (= part/1 10439961859)))
  (let ((part/2 (solve input 4)))
    (print part/2) (assert (= part/2 72050269))))
