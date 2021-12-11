(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets (delete '(0 0) (combinations '(1 0 -1) 2)))

(define (pack-input lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (hash-table-set! mem coord
            (list-ref (list-ref lst x) y))))
      (product (iota (length lst)) (iota (length (car lst)))))
    mem))

(define (import-input)
  (pack-input
    (map (cut map string->number <>)
      (map (cut string-chop <> 1) (read-lines)))))

(define (neighbors mem coord)
  (filter (cut hash-table-exists? mem <>) (map (cut map + <> coord) offsets)))

(define (iterate mem)
  (let ((acc (make-hash-table)))
    (define (loop coord)
      (unless (hash-table-exists? acc coord)
        (hash-table-set! mem coord (+ (hash-table-ref mem coord) 1))
        (when (> (hash-table-ref mem coord) 9)
          (hash-table-set! mem coord  0)
          (hash-table-set! acc coord #t)
          (for-each loop (neighbors mem coord)))))
    (for-each loop (hash-table-keys mem))
    (hash-table-size acc)))

(define (solve/1 input)
  (foldl
    (lambda (acc _)
      (+ acc (iterate input)))
    0 (iota 100)))

(define (solve/2 input)
  (let loop ((acc 1))
    (if (= (iterate input) (hash-table-size input))
      acc
      (loop (+ acc 1)))))

(let ((input (import-input)))
  (print (solve/1 (hash-table-copy input)))
  (print (solve/2 (hash-table-copy input))))
