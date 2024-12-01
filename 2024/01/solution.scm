(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (apply zip
    (map
      (lambda (i)
        (map string->number (string-split i)))
      (read-lines))))

(define (solve/1 input)
  (apply
    (lambda (a b)
      (apply +
        (map
          (lambda (i j)
            (abs (- i j)))
          (sort a <) (sort b <))))
    input))

(define (solve/2 input)
  (apply
    (lambda (a b)
      (let ((mem (make-hash-table)))
        (for-each
          (lambda (i)
            (hash-table-update!/default mem i add1 0))
          b)
        (apply +
          (map
            (lambda (i)
              (* i (hash-table-ref/default mem i 0)))
            a))))
    input))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1151792)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 21790168))))
