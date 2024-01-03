(import
  (chicken io))

(include-relative "../assembunny.scm")

(define (import-input)
  (parse (read-lines)))

(define (solve input n)
  (car (run (list->vector input) "a" "a" n)))

(let ((input (import-input)))
  (let ((part/1 (solve input 7)))
    (print part/1) (assert (= part/1 10880)))
  (let ((part/2 (solve input 12)))
    (print part/2) (assert (= part/2 479007440))))
