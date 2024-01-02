(import
  (chicken io))

(include-relative "assembunny.scm")

(let ((input (parse (read-lines))))
  (let ((part/1 (solve (list->vector input) "c" "a" 0)))
    (print part/1) (assert (= part/1 318077)))
  (let ((part/2 (solve (list->vector input) "c" "a" 1)))
    (print part/2) (assert (= part/2 9227731))))
