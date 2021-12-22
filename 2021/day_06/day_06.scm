(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (increment! table key increment)
  (hash-table-update!/default table key (cut + <> increment) 0))

(define (import-input)
  (let ((mem (make-hash-table)))
    (for-each (cut increment! mem <> 1)
      (map string->number (string-split (read-line) ",")))
    mem))

(define (iterate table)
  (let ((next (make-hash-table)))
    (hash-table-for-each table
      (lambda (i n)
        (for-each (cut increment! next <> n)
          (if (= i 0) `(6 8) `(,(- i 1))))))
    next))

(define (accumulate table n)
  (reverse (foldl
             (lambda (acc _)
               (cons (iterate (car acc)) acc))
             `(,table) (iota n))))

(define (solve lst n)
  (apply + (hash-table-values (list-ref lst n))))

(let ((input (import-input)))
  (let ((res (accumulate input 256)))
    (print (solve res 80))
    (print (solve res 256))))
