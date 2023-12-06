(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->list (irregex-split "\n" i)))
    (irregex-split "\n\n" (read-string))))

(define (solve input proc)
  (apply +
    (map
      (lambda (i)
        (length (apply proc char=? i)))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input lset-union)))
    (print part/1) (assert (= part/1 6686)))
  (let ((part/2 (solve input lset-intersection)))
    (print part/2) (assert (= part/2 3476))))
