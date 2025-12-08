(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

(define (distance pair)
  (bind (m n) pair
    (let* ((a (- (vector-ref m 0) (vector-ref n 0)))
           (b (- (vector-ref m 1) (vector-ref n 1)))
           (c (- (vector-ref m 2) (vector-ref n 2))))
      (+ (* a a) (* b b) (* c c)))))

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

(define (_solve m n acc)
  (let-values (((a b) (partition (lambda (i) (or (hash-table-exists? i m) (hash-table-exists? i n))) acc)))
    (if (null? a)
      (let ((mem (make-hash-table)))
        (hash-table-set! mem m #t)
        (hash-table-set! mem n #t)
        (cons mem acc))
      (let ((mem (foldl hash-table-merge (car a) (cdr a))))
        (hash-table-set! mem m #t)
        (hash-table-set! mem n #t)
        (cons mem b)))))

(define (solve/1 lst arg)
  (let loop ((lst lst) (cnt 0) (acc '()))
    (if (= cnt arg)
      (apply * (take (sort (map hash-table-size acc) >) 3))
      (bind (m n) (car lst)
        (loop (cdr lst) (+ cnt 1) (_solve m n acc))))))

(define (solve/2 lst arg)
  (let loop ((lst lst) (acc '()))
    (bind (m n) (car lst)
      (let ((acc (_solve m n acc)))
        (if (= (hash-table-size (car acc)) arg)
          (* (vector-ref m 0) (vector-ref n 0))
          (loop (cdr lst) acc))))))

(let-values (((lst len) (import-input)))
  (let ((part/1 (solve/1 lst 1000)))
    (print part/1) (assert (= part/1 26400)))
  (let ((part/2 (solve/2 lst len)))
    (print part/2) (assert (= part/2 8199963486))))
