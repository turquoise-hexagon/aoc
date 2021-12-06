(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map string->number (string-split (read-line) ",")))

(define (increment mem key value)
  (hash-table-set! mem key
    (+ (hash-table-ref/default mem key 0) value)))

(define (iterate mem)
  (let ((new (make-hash-table)))
    (hash-table-for-each mem
      (lambda (i n)
        (for-each (cut increment new <> n)
          (if (= i 0) '(6 8) (list (- i 1))))))
    new))

(define (solve x n)
  (let ((mem (make-hash-table)))
    (for-each (cut increment mem <> 1) x)
    (apply + (hash-table-values
               (foldl
                 (lambda (acc _)
                   (iterate acc))
                 mem (iota n))))))

(let ((input (import-input)))
  (print (solve input 80))
  (print (solve input 256)))
