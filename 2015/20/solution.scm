(import
  (chicken fixnum))

(define (import-input)
  (read))

(define (generate limit multiplier repetitions)
  (let ((acc (make-vector (fx+ limit 1) 0)))
    (do ((i 1 (fx+ i 1))) ((fx> i limit))
      (do ((j i (fx+ j i))
           (c 0 (fx+ c 1)))
        ((or (fx> j limit) (fx= c repetitions)))
        (vector-set! acc j (fx+ (vector-ref acc j) (fx* i multiplier)))))
    acc))

(define (solve input multiplier #!optional (repetitions most-positive-fixnum))
  (let ((mem (generate #e1e6 multiplier repetitions)))
    (do ((i 0 (fx+ i 1)))
      ((fx> (vector-ref mem i) input)
       i))))

(let ((input (import-input)))
  (let ((part/1 (solve input 10)))
    (print part/1) (assert (= part/1 776160)))
  (let ((part/2 (solve input 11 50)))
    (print part/2) (assert (= part/2 786240))))
