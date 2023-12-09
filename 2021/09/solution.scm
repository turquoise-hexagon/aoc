(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (list->array
    (map
      (lambda (i)
        (map string->number (string-chop i 1)))
      (read-lines))))

(define (neighbors array coord)
  (filter
    (lambda (i)
      (array-exists? array i))
    (map
      (lambda (i)
        (map + coord i))
      '((1 0) (0 1) (-1 0) (0 -1)))))

(define (low-point? array coord)
  (> (apply min
       (map
         (lambda (i)
           (array-ref array i))
         (neighbors array coord)))
     (array-ref array coord)))

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
    (map
      (lambda (i)
        (+ (array-ref input i) 1))
      low-points)))

(define (solve/2 input low-points)
  (let ((lens (map (lambda (i) (hash-table-size (bassin input i))) low-points)))
    (apply * (take (sort lens >) 3))))

(let ((input (import-input)))
  (let ((low-points (filter (lambda (i) (low-point? input i)) (array-indexes input))))
    (let ((part/1 (solve/1 input low-points)))
      (print part/1) (assert (= part/1 444)))
    (let ((part/2 (solve/2 input low-points)))
      (print part/2) (assert (= part/2 1168440)))))
