(import
  (chicken io)
  (srfi 1))

(include "assembunny.scm")

(define (import-input)
  (parse (read-lines)))

(define (solve input)
  (let loop ((i 0))
    (let ((_ (cadr (run (list->vector input) "a" "a" i))))
      (if (every
            (lambda (a b)
              (not (= a b)))
            _ (cdr _))
        i
        (loop (+ i 1))))))

(let ((part (solve (import-input))))
  (print part) (assert (= part 175)))
