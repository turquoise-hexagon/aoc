(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (connect! graph a b)
  (hash-table-update! graph a (cut cons b <>)))

(define (import-input)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply
          (lambda (a . l)
            (for-each
              (lambda (b)
                (connect! acc a b)
                (connect! acc b a))
              l))
          (map string->number (string-split i " <->,"))))
      (read-lines))
    acc))

(define (group graph node)
  (let ((acc (make-hash-table)))
    (let loop ((i node))
      (unless (hash-table-exists? acc i)
        (hash-table-set! acc i #t)
        (for-each loop (hash-table-ref graph i))))
    acc))

(define (solve/1 input)
  (hash-table-size (group input 0)))

(define (solve/2 input)
  (let loop ((lst (hash-table-keys input)) (acc 0))
    (if (null? lst)
      acc
      (loop
        (let ((_ (group input (car lst))))
          (remove
            (lambda (i)
              (hash-table-exists? _ i))
            lst))
        (+ acc 1)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 134)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 193))))
