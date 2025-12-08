(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (distance pair)
  (apply
    (lambda (m n)
      (let* ((a (- (vector-ref m 0) (vector-ref n 0)))
             (b (- (vector-ref m 1) (vector-ref n 1)))
             (c (- (vector-ref m 2) (vector-ref n 2))))
        (+ (* a a) (* b b) (* c c))))
    pair))

(define (_import-input)
  (map
    (lambda (i)
      (list->vector (map string->number (string-split i ","))))
    (read-lines)))

(define (import-input)
  (let ((lst (_import-input)))
    (values
      (sort
        (combinations lst 2)
        (lambda (m n)
          (< (distance m)
             (distance n))))
      (length lst))))

(define (_solve i acc)
  (let-values (((m n) (partition (lambda (mem) (any (lambda (i) (hash-table-exists? mem i)) i)) acc)))
    (if (null? m)
      (let ((mem (make-hash-table)))
        (for-each
          (lambda (i)
            (hash-table-set! mem i #t))
          i)
        (cons mem acc))
      (let ((mem (foldl hash-table-merge (car m) (cdr m))))
        (for-each
          (lambda (i)
            (hash-table-set! mem i #t))
          i)
        (cons mem n)))))

(define (solve lst arg len)
  (let loop ((lst lst) (cnt 0) (acc '()) (p1 #f) (p2 #f))
    (if (and p1 p2)
      (list p1 p2)
      (if (and (not p1) (= cnt arg))
        (loop lst cnt acc (apply * (take (sort (map hash-table-size acc) >) 3)) p2)
        (let* ((i (car lst)) (acc (_solve i acc)))
          (if (= (hash-table-size (car acc)) len)
            (loop (cdr lst) (+ cnt 1) acc p1 (apply * (map (lambda (i) (vector-ref i 0)) i)))
            (loop (cdr lst) (+ cnt 1) acc p1 p2)))))))

(let-values (((lst len) (import-input)))
  (let ((parts (solve lst 1000 len)))
    (for-each print parts) (assert (equal? parts '(26400 8199963486)))))
