(import
  (chicken io)
  (euler)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (search array pattern offsets)
  (filter
    (lambda (coords)
      (equal?
        (map
          (lambda (coord)
            (if (array-exists? array coord)
              (array-ref array coord)
              ""))
          coords)
        pattern))
    (append-map
      (lambda (offset)
        (map
          (lambda (coord)
            (let loop ((coord coord) (pattern pattern))
              (if (null? pattern)
                '()
                (cons coord (loop (map + coord offset) (cdr pattern))))))
          (array-indexes array)))
      offsets)))

(define (solve/1 input)
 (length (search input '(#\X #\M #\A #\S) '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))))

(define (solve/2 input)
  (count
    (lambda (lst)
      (bind (a b) lst
        (equal?
          (cadr a)
          (cadr b))))
    (combinations (search input '(#\M #\A #\S) '((-1 -1) (-1 1) (1 -1) (1 1))) 2)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2370)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1908))))
