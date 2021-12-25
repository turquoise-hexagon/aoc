(import
  (chicken io)
  (euler))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (neighbor array coord type)
  (let ((next (map
                (lambda (i dimension)
                  (modulo (+ i dimension) dimension))
                (map + coord
                  (case type
                    ((#\>) '(0 1))
                    ((#\v) '(1 0))))
                (array-dimensions array))))
    (if (char=? (array-ref array next) #\.) next #f)))

(define (iterate array type)
  (receive (h w) (apply values (array-dimensions array))
    (let ((acc (array-copy array)))
      (for-each
        (lambda (coord)
          (when (char=? (array-ref array coord) type)
            (let ((next (neighbor array coord type)))
              (when next
                (array-set! acc coord #\.)
                (array-set! acc next type)))))
        (array-indexes array))
      acc)))

(define (solve input)
  (let loop ((i 1) (acc input))
    (let ((next (iterate (iterate acc #\>) #\v)))
      (if (equal? next acc)
        i
        (loop (+ i 1) next)))))

(let ((input (import-input)))
  (print (solve input)))
