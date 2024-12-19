(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (apply
    (lambda (ps _ . ds)
      (values (string-split ps ", ") ds))
    (read-lines)))

(define-inline (__solve d)
  (apply +
    (map
      (lambda (p)
        (let ((pl (string-length p))
              (dl (string-length d)))
          (if (and (>= dl pl) (substring=? d p))
            (_solve (substring d pl))
            0)))
      ps)))

(define (solve ps ds)
  (let ((m (make-hash-table)))
    (hash-table-set! m "" 1)

    (define (_solve d)
      (if (hash-table-exists? m d)
        (hash-table-ref m d)
        (let ((r (__solve d)))
          (hash-table-set! m d r)
          r)))

    (let ((l (map _solve ds)))
      (list (count positive? l) (apply + l)))))

(let-values (((ps ds) (import-input)))
  (let ((parts (solve ps ds)))
    (for-each print parts) (assert (equal? parts '(365 730121486795169)))))
