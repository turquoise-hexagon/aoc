(define-record grid
  content
  h
  w)

(define (list->grid lst)
  (let ((h (length lst)) (w (length (car lst))))
    (make-grid
      (list->vector (map list->vector lst))
      h
      w)))

(define (grid->list grid)
  (map vector->list (vector->list (grid-content grid))))

(define (grid-end grid)
  (list (- (grid-h grid) 1)
        (- (grid-w grid) 1)))

(define (grid-ref grid coord)
  (let ((content (grid-content grid)))
    (receive (x y) (apply values coord)
      (vector-ref (vector-ref content x) y))))

(define (grid-exists? grid coord)
  (receive (x y) (apply values coord)
    (and (< -1 x (grid-h grid))
         (< -1 y (grid-w grid)))))
