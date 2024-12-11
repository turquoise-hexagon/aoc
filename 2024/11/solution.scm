(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax))

(define (import-input)
  (map string->number (string-split (read-line))))

(define-memoized (run n count)
  (if (zero? count)
    1
    (if (zero? n)
      (run 1 (- count 1))
      (let ((len (+ (integer-log n 10) 1)))
        (if (even? len)
          (let ((tmp (expt 10 (quotient len 2))))
            (+ (run (quotient n tmp) (- count 1))
               (run (modulo   n tmp) (- count 1))))
          (run (* n 2024) (- count 1)))))))

(define (solve input count)
  (apply +
    (map
      (lambda (i)
        (run i count))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 25)))
    (print part/1) (assert (= part/1 199946)))
  (let ((part/2 (solve input 75)))
    (print part/2) (assert (= part/2 237994815702032))))
