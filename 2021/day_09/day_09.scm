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

(define (extend-bassin mem mem/bassin mem/added)
  (let ((mem/acc (make-hash-table)))
    (for-each
      (lambda (coord)
        (let ((current (hash-table-ref mem coord)))
          (for-each
            (lambda (coord)
              (let ((tmp (hash-table-ref mem coord)))
                ;; do not take into account invalid values & coordinates already in the bassin
                (when (and (> 9 tmp current) (not (hash-table-exists? mem/bassin coord)))
                  (hash-table-set! mem/acc coord #t))))
            (neighbors mem coord))))
      (hash-table-keys mem/added))
    mem/acc))

(define (bassin mem coord)
  (let ((mem/acc (alist->hash-table `((,coord . #t)))))
    (let loop ((mem/added mem/acc))
      ;; considere only the coordinates that were just added
      (let ((mem/tmp (extend-bassin mem mem/acc mem/added)))
        (if (= (hash-table-size mem/tmp) 0)
          mem/acc
          (begin
            (for-each (cut hash-table-set! mem/acc <> #t) (hash-table-keys mem/tmp))
            (loop mem/tmp)))))))

(define (solve/1 input low-points)
  (apply + (map (cut + <> 1) (map (cut hash-table-ref input <>) low-points))))

(define (solve/2 input low-points)
  (let ((lens (map hash-table-size (map (cut bassin input <>) low-points))))
    (apply * (take (sort lens >) 3))))

(let ((input (import-input)))
  (let ((low-points (filter (cut low-point? input <>) (hash-table-keys input))))
    (print (solve/1 input low-points))
    (print (solve/2 input low-points))))
