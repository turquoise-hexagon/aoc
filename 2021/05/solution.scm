(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse str)
  (chop (map string->number (string-split str ", ->")) 2))

(define (import-input)
  (partition
    (lambda (i)
      (apply any = i))
    (map parse (read-lines))))

(define (segment->points lst)
  (apply
    (lambda (a b)
      (let ((offset (map signum (map - b a))))
        (let loop ((i a) (acc '()))
          (let ((acc (cons i acc)))
            (if (equal? i b)
              acc
              (loop (map + i offset) acc))))))
    lst))

(define (solve mem input)
  (for-each
    (lambda (i)
      (for-each
        (lambda (i)
          (hash-table-update!/default mem i add1 0))
        (segment->points i)))
    input)
  (count
    (lambda (i)
      (> i 1))
    (hash-table-values mem)))

(let-values (((lines diags) (import-input)))
  (let ((mem (make-hash-table)))
    (let ((part/1 (solve mem lines)))
      (print part/1) (assert (= part/1 8622)))
    (let ((part/2 (solve mem diags)))
      (print part/2) (assert (= part/2 22037)))))
