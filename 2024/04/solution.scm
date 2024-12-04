(import
  (chicken io)
  (euler)
  (srfi 1))

(define offsets/1 (delete-first (power '(-1 0 1) 2) '(0 0) equal?))
(define offsets/2 (delete-first (power '(-1   1) 2) '(0 0) equal?))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (_search array pattern coord offset)
  (let loop ((coord coord) (pattern pattern) (acc '()))
    (if (null? pattern)
      acc
      (if (and (array-exists? array coord) (char=? (array-ref array coord) (car pattern)))
        (loop (map + coord offset) (cdr pattern) (cons coord acc))
        #f))))

(define (search array pattern offsets)
  (let ((pattern (string->list pattern)))
    (append-map
      (lambda (offset)
        (filter-map
          (lambda (coord)
            (_search array pattern coord offset))
          (array-indexes array)))
      offsets)))

(define (solve/1 input)
  (length (search input "XMAS" offsets/1)))

(define (solve/2 input)
  (let loop/1 ((lst/1 (search input "MAS" offsets/2)) (acc/1 0))
    (if (null? lst/1)
      acc/1
      (let loop/2 ((lst/2 (cdr lst/1)) (acc/2 0))
        (if (null? lst/2)
          (loop/1 (cdr lst/1) (+ acc/1 acc/2))
          (loop/2 (cdr lst/2)
            (if (equal? (cadar lst/1) (cadar lst/2))
              (+ acc/2 1)
              acc/2)))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2370)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1908))))
