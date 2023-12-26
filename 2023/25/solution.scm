(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply
          (lambda (a . lst)
            (for-each
              (lambda (b)
                (hash-table-update! acc a (cut cons b <>))
                (hash-table-update! acc b (cut cons a <>)))
              lst))
          (string-split i ": ,")))
      (read-lines))
    acc))

(define (solve graph)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (i)
        (hash-table-set! mem i #t))
      (hash-table-keys graph))

    (define (value i)
      (count
        (lambda (i)
          (not (hash-table-exists? mem i)))
        (hash-table-ref graph i)))

    (let loop ()
      (if (= (apply + (map value (hash-table-keys mem))) 3)
        (* (hash-table-size mem)
           (count
             (lambda (i)
               (not (hash-table-exists? mem i)))
             (hash-table-keys graph)))
        (begin
          (hash-table-delete! mem (extremum (hash-table-keys mem) value >))
          (loop))))))

(let ((part (solve (import-input))))
  ; warning : probabilistic
  (print part) (assert (= part 582692)))
