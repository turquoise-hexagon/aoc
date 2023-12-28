(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 69))

(define (connect! graph a b dist)
  (hash-table-update!/default graph a
    (lambda (value)
      (hash-table-set! value b dist)
      value)
    (make-hash-table)))

(define (process graph)
  (map
    (lambda (i)
      (apply +
        (map
          (lambda (a b)
            (hash-table-ref (hash-table-ref graph a) b))
          i (cdr i))))
    (permutations (hash-table-keys graph))))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (a _ b _ dist)
            (let ((dist (string->number dist)))
              (connect! acc a b dist)
              (connect! acc b a dist)))
          (string-split i " ")))
      (read-lines))
    (process acc)))

(define (solve input proc)
  (foldl proc (car input) (cdr input)))

(let ((input (import-input)))
  (let ((part/1 (solve input min)))
    (print part/1) (assert (= part/1 117)))
  (let ((part/2 (solve input max)))
    (print part/2) (assert (= part/2 909))))
