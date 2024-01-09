(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ", ")))
    (read-lines)))

(define (distance a b)
  (apply + (map abs (map - a b))))

(define (process lst)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply
          (lambda (a b)
            (unless (> (distance a b) 3)
              (hash-table-update! acc a
                (lambda (lst) (cons b lst)))))
          i))
      (power lst 2))
    acc))

(define (group graph node)
  (let ((acc (make-hash-table)))
    (let loop ((i node))
      (unless (hash-table-exists? acc i)
        (hash-table-set! acc i #t)
        (for-each loop (hash-table-ref graph i))))
    acc))

(define (solve input)
  (let ((graph (process input)))
    (do ((lst
           (hash-table-keys graph)
           (let ((group (group graph (car lst))))
             (remove
               (lambda (i)
                 (hash-table-exists? group i))
               lst)))
         (acc 0 (+ acc 1)))
      ((null? lst) acc))))

(let ((part (solve (import-input))))
  (print part) (assert (= part 352)))
