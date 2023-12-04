(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (process str)
  (apply
    (lambda (_ a b)
      (length
        (lset-intersection string=?
          (string-split a " ")
          (string-split b " "))))
    (string-split str ":|")))

(define (import-input)
  (map process (read-lines)))

(define (solve/1 input)
  (apply +
    (map
      (lambda (value)
        (if (= value 0) 0 (expt 2 (- value 1))))
      input)))

(define (solve/2 input)
  (let ((acc (make-vector (length input) 1)))
    (for-each
      (lambda (value index)
        (let ((count (vector-ref acc index)))
          (for-each
            (lambda (i)
              (vector-set! acc i (+ (vector-ref acc i) count)))
            (iota value (+ index 1)))))
      input (iota (length input)))
    (apply + (vector->list acc))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 23028)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 9236992))))
