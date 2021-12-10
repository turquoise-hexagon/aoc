(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define (pack-input lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (hash-table-set! mem coord (list-ref (list-ref lst x) y))))
      (product (iota (length lst)) (iota (length (car lst)))))
    mem))

(define (import-input)
  (pack-input
    (map (cut map string->number <>)
      (map (cut string-chop <> 1) (read-lines)))))

(define (neighbors mem coord)
  (filter (cut hash-table-exists? mem <>)
    (map (cut map + coord <>) '((1 0) (0 1) (-1 0) (0 -1)))))

(define (low-point? mem coord)
  (> (apply min (map (cut hash-table-ref mem <>) (neighbors mem coord)))
     (hash-table-ref mem coord)))

(define (bassin mem coord)
  (let ((acc (make-hash-table)))
    (let loop ((coord coord))
      (unless (or (hash-table-exists? acc coord)
                  (= (hash-table-ref mem coord) 9))
        (hash-table-set! acc coord #t)
        (for-each loop (neighbors mem coord))))
    acc))

(define (solve/1 input low-points)
  (apply + (map (cut + <> 1) (map (cut hash-table-ref input <>) low-points))))

(define (solve/2 input low-points)
  (let ((lens (map hash-table-size (map (cut bassin input <>) low-points))))
    (apply * (take (sort lens >) 3))))

(let ((input (import-input)))
  (let ((low-points (filter (cut low-point? input <>) (hash-table-keys input))))
    (print (solve/1 input low-points))
    (print (solve/2 input low-points))))
