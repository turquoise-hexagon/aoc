(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (increment! mem key value)
  (hash-table-set! mem key
    (+ (hash-table-ref/default mem key 0) value)))

(define (import-input)
  (let ((mem (make-hash-table)))
    (for-each (cut increment! mem <> 1)
      (map string->number (string-split (read-line) ",")))
    mem))

(define (iterate mem)
  (let ((next (make-hash-table)))
    (hash-table-for-each mem
      (lambda (i n)
        (for-each (cut increment! next <> n)
          (if (= i 0) `(6 8) `(,(- i 1))))))
    next))

(define (solve input n)
  (let ((res (foldl
               (lambda (acc _)
                 (iterate acc))
               input (iota n))))
    (apply + (hash-table-values res))))

(let ((input (import-input)))
  (print (solve input 80))
  (print (solve input 256)))
