(import
  (chicken io))

(include-relative "assembunny.scm")

(define (import-input)
  (parse (read-lines)))

(define (solve input n)
  (car (run (list->vector input) "c" "a" n)))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= part/1 318077)))
  (let ((part/2 (solve input 1)))
    (print part/2) (assert (= part/2 9227731))))
