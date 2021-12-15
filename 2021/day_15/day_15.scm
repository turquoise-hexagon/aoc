(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(include-relative "bucket")

(define (pack-input lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (hash-table-set! acc coord
            (list-ref (list-ref lst x) y))))
      (product (iota (length lst)) (iota (length (car lst)))))
    acc))

(define (import-input)
  (pack-input
    (map (cut map string->number <>)
      (map (cut string-chop <> 1)
        (read-lines)))))

(define (extend mem n)
  (receive (x y) (unzip2 (hash-table-keys mem))
    (let ((h (+ (apply max x) 1))
          (w (+ (apply max y) 1))
          (multipliers (delete '(0 0) (combinations (iota n) 2))))
      (hash-table-for-each mem
        (lambda (coord value)
          (for-each
            (lambda (multiplier)
              (hash-table-set! mem (map + coord (map * multiplier (list h w)))
                (+ (modulo (+ value (apply + multiplier) -1) 9) 1)))
            multipliers))))))

(define (neighbors mem coord)
  (filter (cut hash-table-exists? mem <>)
    (map (cut map + <> coord) '((1 0) (0 1) (-1 0) (0 -1)))))

(define (solve mem from to)
  (let ((acc (alist->hash-table `((,from . 0)))) (bucket (make-bucket 10000)))
    (bucket-add! bucket 0 from)
    (let loop ()
      (let ((result (bucket-pop! bucket)))
        (if result
          (receive (distance coord) (apply values result)
            (for-each
              (lambda (neigh)
                (let ((value (+ (hash-table-ref mem neigh) distance)))
                  (if (hash-table-exists? acc neigh)
                    (when (> (hash-table-ref acc neigh) value)
                      (hash-table-set! acc neigh value))
                    (begin
                      (bucket-add! bucket value neigh)
                      (hash-table-set! acc neigh value)))))
              (neighbors mem coord))
            (loop))
          (hash-table-ref acc to))))))

(let ((input (import-input)))
  (print (solve input '(0 0) '(99 99)))
  (extend input 5)
  (print (solve input '(0 0) '(499 499))))
