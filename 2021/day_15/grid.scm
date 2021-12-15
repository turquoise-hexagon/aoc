(define-record grid
  content
  h
  w)

(define (list->grid lst)
  (make-grid
    (list->vector (map list->vector lst))
    (length lst)
    (length (car lst))))

(define (grid->list grid)
  (map vector->list (vector->list (grid-content grid))))

(define (grid-end grid)
  (map (cut - <> 1)
    (list (grid-h grid)
          (grid-w grid))))

(define (grid-ref grid coord)
  (let ((content (grid-content grid)))
    (receive (x y) (apply values coord)
      (vector-ref (vector-ref content x) y))))

(define (grid-exists? grid coord)
  (let ((h (grid-h grid))
        (w (grid-w grid)))
    (receive (x y) (apply values coord)
      (and (< -1 x h)
           (< -1 y w)))))
