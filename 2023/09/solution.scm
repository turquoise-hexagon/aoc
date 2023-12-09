(import
  (chicken io)
  (chicken string)
  (euler))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i " ")))
    (read-lines)))

(define (extrapolate lst)
  (let ((len (length lst)))
    (apply +
      (map
        (lambda (value index)
          (* (expt -1 index) (binomial len index) value))
        lst (range (sub1 len))))))

(define (solve input)
  (apply + (map extrapolate input)))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 2105961943)))
  (let ((part/2 (solve (map reverse input))))
    (print part/2) (assert (= part/2 1019))))
