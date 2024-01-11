(import
  (chicken io))

(define (import-input)
  (map string->number (read-lines)))

(define (proc/1 n)
  (- (quotient n 3) 2))

(define (proc/2 n)
  (let loop ((i n) (acc 0))
    (let ((_ (proc/1 i)))
      (if (< _ 0)
        acc
        (loop _ (+ acc _))))))

(define (solve input proc)
  (apply + (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 3126794)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 4687331))))
