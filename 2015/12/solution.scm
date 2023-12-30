(import
  (chicken io)
  (srfi 1)
  (srfi 158)
  (srfi 180))

(define (import-input)
  (generator->list (json-lines-read)))

(define (solve input #!optional (flag #f))
  (let loop ((i input))
    (cond
      ((vector? i)
       (apply + (map loop (vector->list i))))
      ((list? i)
       (if (not (and flag (any (lambda (i) (and (string? (cdr i)) (string=? (cdr i) "red"))) i)))
         (apply + (map loop i))
         0))
      ((pair? i)
       (+ (loop (car i))
          (loop (cdr i))))
      ((number? i) i)
      (else 0))))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 191164)))
  (let ((part/2 (solve input #t)))
    (print part/2) (assert (= part/2 87842))))
