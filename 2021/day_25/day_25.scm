(import
  (chicken io)
  (euler))

(include-relative "utils/grid.scm")

(define (import-input) 
  (list->grid (map string->list (read-lines))))

(define (neighbor grid coord type)
  (let ((next
          (map
            (lambda (i dim)
              (modulo (+ i dim) dim))
            (map + coord
              (case type
                ((#\>) '(0 1))
                ((#\v) '(1 0))))
            (list (grid-h grid) (grid-w grid)))))
    (if (char=? (grid-ref grid next) #\.) next #f)))

(define (iterate grid type)
  (let ((acc (list->grid (grid->list grid))))
    (for-each
      (lambda (coord)
        (if (char=? (grid-ref grid coord) type)
          (let ((next (neighbor grid coord type)))
            (when next
              (grid-set! acc next type)
              (grid-set! acc coord #\.)))))
      (product (range 0 (- (grid-h grid) 1))
               (range 0 (- (grid-w grid) 1))))
    acc))

(define (solve input)
  (let loop ((i 1) (acc input))
    (let ((next (iterate (iterate acc #\>) #\v)))
      (if (equal? next acc)
        i
        (loop (+ i 1) next)))))

(let ((input (import-input)))
  (print (solve input)))
