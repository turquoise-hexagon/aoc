(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant h 400)
(define-constant w 400)

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ", ")))
    (read-lines)))

(define (distance a b)
  (apply + (map abs (map - a b))))

(define (solve/1 input)
  (let ((mem (make-hash-table #:initial '())))
    (for-each
      (lambda (a)
        (let ((m (apply min (map (cut distance a <>) input))))
          (for-each
            (lambda (b)
              (when (= (distance a b) m)
                (hash-table-update! mem a (cut cons b <>))))
            input)))
      (product
        (range h)
        (range w)))
    (let ((acc (make-hash-table #:initial '())))
      (for-each
        (lambda (i)
          (let ((v (hash-table-ref mem i)))
            (when (= (length v) 1)
              (hash-table-update! acc v (cut cons i <>)))))
        (hash-table-keys mem))
      (apply max
        (map length
          (remove
            (lambda (i)
              (any (lambda (i) (apply (lambda (a b) (or (= a 0) (= a h) (= b 0) (= b w))) i)) i))
            (hash-table-values acc)))))))

(define (solve/2 input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (when (< (apply + (map (cut distance i <>) input)) 10000)
          (hash-table-set! acc i #t)))
      (product
        (range h)
        (range w)))
    (hash-table-size acc)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 3890)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 40284))))
