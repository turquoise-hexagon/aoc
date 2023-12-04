(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (_)
      (map string->number (string-split _ "-,")))
    (read-lines)))

(define (valid?/1 a b c d)
  (or (<= a c d b)
      (<= c a b d)))

(define (valid?/2 a b c d)
  (and (<= a d)
       (<= c b)))

(define (solve input proc)
  (count (lambda (_) (apply proc _)) input))

(let ((input (import-input)))
  (let ((part/1 (solve input valid?/1)))
    (print part/1) (assert (= part/1 536)))
  (let ((part/2 (solve input valid?/2)))
    (print part/2) (assert (= part/2 845))))
