(import
  (chicken io)
  (chicken sort)
  (srfi 1)
  (srfi 69))

(define (process lst)
  (let ((mem (make-hash-table #:initial 0)))
    (for-each
      (lambda (i)
        (hash-table-update! mem i add1))
      lst)
    (sort (hash-table-keys mem)
      (lambda (a b)
        (> (hash-table-ref mem a)
           (hash-table-ref mem b))))))

(define (import-input)
  (map process (apply zip (map string->list (read-lines)))))

(define (solve input proc)
  (list->string (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input first)))
    (print part/1) (assert (string=? part/1 "zcreqgiv")))
  (let ((part/2 (solve input last)))
    (print part/2) (assert (string=? part/2 "pljvorrk"))))
