(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  '((-1 -1) (-1  0) (-1  1)
    ( 0 -1) ( 0  0) ( 0  1)
    ( 1 -1) ( 1  0) ( 1  1)))

(define (convert str)
  (map
    (lambda (char)
      (case char
        ((#\.) 0)
        ((#\#) 1)))
    (string->list str)))

(define (pack-image lst)
  (let ((h (length lst)) (w (length (car lst))))
    (let ((acc (make-hash-table)))
      (for-each
        (lambda (coord)
          (receive (x y) (apply values coord)
            (hash-table-set! acc coord
              (list-ref (list-ref lst x) y))))
        (product (range 0 (- h 1)) (range 0 (- w 1))))
      acc)))

(define (import-input)
  (receive (data _ . image) (apply values (map convert (read-lines)))
    (values data (pack-image image))))

(define (minimum/maximum lst proc)
  ;; applying proc would be inefficient
  (foldl proc (car lst) (cdr lst)))

(define (minimum lst) (minimum/maximum lst min))
(define (maximum lst) (minimum/maximum lst max))

(define (default data n)
  ;; take into account blinking edges
  (if (odd? n) (car data)
    0))

(define (neighbors data image coord default)
  (list-ref data
    (list->number
      (map (cut hash-table-ref/default image <> default)
        (map (cut map + coord <>) offsets)) 2)))

(define (bounds image)
  (receive (x y) (unzip2 (hash-table-keys image))
    (values (minimum x) (maximum x)
            (minimum y) (maximum y))))

(define (iterate data image n)
  (receive (x-min x-max y-min y-max) (bounds image)
    (let ((acc (make-hash-table)) (default (default data n)))
      (for-each
        (lambda (coord)
          (hash-table-set! acc coord
            (neighbors data image coord default)))
        ;; extend
        (product (range (- x-min 1) (+ x-max 1))
                 (range (- y-min 1) (+ y-max 1))))
      acc)))

(define (solve data image n)
  (foldl + 0
    (hash-table-values
      (foldl
        (lambda (acc n)
          (iterate data acc n))
        image (iota n)))))

(receive (data image) (import-input)
  (print (solve data image 2))
  (print (solve data image 50)))
