(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (bind (ranges ids)
    (foldr
      (lambda (i acc)
        (if (string=? i "") (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))
    (list
      (sort
        (map
          (lambda (i)
            (map string->number (string-split i "-")))
          ranges)
        (lambda (m n)
          (every < m n)))
      (map string->number ids))))

(define (solve/1 ranges ids)
  (count
    (lambda (id)
      (any
        (lambda (range)
          (bind (m n) range (<= m id n)))
        ranges))
    ids))

(define (solve/2 ranges)
  (let loop ((lst ranges) (i 0) (acc 0))
    (if (null? lst)
      acc
      (bind (m n) (car lst)
        (let ((m (if (<= m i) (+ i 1) m)))
          (if (<= m n)
            (loop (cdr lst) (max i n) (+ acc (+ (- n m) 1)))
            (loop (cdr lst) i acc)))))))

(bind (ranges ids) (import-input)
  (let ((part/1 (solve/1 ranges ids)))
    (print part/1) (assert (= part/1 617)))
  (let ((part/2 (solve/2 ranges)))
    (print part/2) (assert (= part/2 313461316777284))))
