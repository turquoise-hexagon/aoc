(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (let ((vals (make-hash-table)) (lsts (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (name val . lst)
            (hash-table-set! vals name (string->number val))
            (hash-table-set! lsts name lst))
          (string-split i " ()->,")))
      (read-lines))
    (values vals lsts)))

(define (solve/1 vals lsts)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (i)
        (for-each
          (lambda (i)
            (hash-table-set! mem i #t))
          (hash-table-ref lsts i)))
      (hash-table-keys lsts))
    (find
      (lambda (i)
        (not (hash-table-exists? mem i)))
      (hash-table-keys vals))))

(define (solve/2 vals lsts node)
  (define (total node)
    (foldl
      (lambda (acc i)
        (+ acc (total i)))
      (hash-table-ref vals node)
      (hash-table-ref lsts node)))

  (let loop ((node node) (acc 0))
    (let ((mem (make-hash-table #:initial 0)))
      (for-each
        (lambda (i)
          (hash-table-update! mem (total i) add1))
        (hash-table-ref lsts node))
      (if (= (hash-table-size mem) 1)
        (+ acc (hash-table-ref vals node))
        (apply
          (lambda (a b)
            (loop
              (find
                (lambda (i)
                  (= (total i) b))
                (hash-table-ref lsts node))
              (- a b)))
          (sort (hash-table-keys mem)
            (lambda (a b)
              (> (hash-table-ref mem a)
                 (hash-table-ref mem b)))))))))

(let-values (((vals lsts) (import-input)))
  (let ((part/1 (solve/1 vals lsts)))
    (print part/1) (assert (string=? part/1 "uownj"))
    (let ((part/2 (solve/2 vals lsts part/1)))
      (print part/2) (assert (= part/2 596)))))
