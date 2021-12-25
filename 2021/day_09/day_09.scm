(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (list->array
    (map (cut map string->number <>)
      (map (cut string-chop <> 1)
        (read-lines)))))

(define (neighbors array coord)
  (filter (cut array-exists? array <>)
    (map (cut map + <> coord)
      '((1 0) (0 1) (-1 0) (0 -1)))))

(define (low-point? array coord)
  (> (apply min
       (map (cut array-ref array <>)
         (neighbors array coord)))
     (array-ref array coord)))

(define (low-points array)
  (receive (h w) (apply values (array-dimensions array))
    (filter (cut low-point? array <>)
      (product
        (range 0 (- h 1))
        (range 0 (- w 1))))))

(define (bassin array coord)
  (let ((acc (make-hash-table)))
    (let loop ((coord coord))
      (unless (hash-table-exists? acc coord)
        (unless (= (array-ref array coord) 9)
          (hash-table-set! acc coord #t)
          (for-each loop (neighbors array coord)))))
    acc))

(define (solve/1 input low-points)
  (apply +
    (map (cut + <> 1)
      (map (cut array-ref input <>)
        low-points))))

(define (solve/2 input low-points)
  (let ((lens (map hash-table-size (map (cut bassin input <>) low-points))))
    (apply * (take (sort lens >) 3))))

(let ((input (import-input)))
  (let ((low-points (low-points input)))
    (print (solve/1 input low-points))
    (print (solve/2 input low-points))))
