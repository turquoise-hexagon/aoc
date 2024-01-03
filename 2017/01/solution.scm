(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map string->number (string-chop (read-line) 1)))

(define (solve input n)
  (fold
    (lambda (a b acc)
      (if (= a b) (+ acc a) acc))
    0 input (list-tail (apply circular-list input) n)))

(let ((input (import-input)))
  (let ((part/1 (solve input 1)))
    (print part/1) (assert (= part/1 1029)))
  (let ((part/2 (solve input (quotient (length input) 2))))
    (print part/2) (assert (= part/2 1220))))
