(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (increment! table key increment)
  (hash-table-update!/default table key (lambda (_) (+ increment _)) 0))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (increment! acc i 1))
      (map string->number (string-split (read-line) ",")))
    acc))

(define (iterate table)
  (let ((acc (make-hash-table)))
    (hash-table-for-each table
      (lambda (i n)
        (for-each
          (lambda (i)
            (increment! acc i n))
          (if (= i 0) '(6 8)
            (list (- i 1))))))
    acc))

(define (accumulate table n)
  (let loop ((i 0) (table table))
    (if (> i n)
      '()
      (cons table (loop (+ i 1) (iterate table))))))

(define (solve lst n)
  (apply + (hash-table-values (list-ref lst n))))

(let ((input (import-input)))
  (let ((lst (accumulate input 256)))
    (let ((part/1 (solve lst 80)))
      (print part/1) (assert (= part/1 375482)))
    (let ((part/2 (solve lst 256)))
      (print part/2) (assert (= part/2 1689540415957)))))
