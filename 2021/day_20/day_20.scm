(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  '((-1 -1) (-1  0) (-1  1)
    ( 0 -1) ( 0  0) ( 0  1)
    ( 1 -1) ( 1  0) ( 1  1)))

(define (pack-image lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (hash-table-set! acc coord
            (list-ref (list-ref lst x) y))))
      (product (range 0 (- (length lst) 1))
               (range 0 (- (length (car lst)) 1))))
    acc))

(define (import-input)
  (receive (data _ . image) (apply values (read-lines))
    (values data (pack-image (map string->list image)))))

(define (min/max lst proc)
  ;; applying would be inefficient
  (foldl proc (car lst) (cdr lst)))

(define (minimum lst) (min/max lst min))
(define (maximum lst) (min/max lst max))

(define (outside data n)
  ;; take into account blinking borders
  (if (odd? n)
    (string-ref data 0)
    #\.))

(define (neighbors data image coord outside)
  (string-ref data
    (list->number
      (map
        (lambda (char)
          (case char
            ((#\.) 0)
            ((#\#) 1)))
        (map (cut hash-table-ref/default image <> outside)
          (map (cut map + coord <>) offsets)))
      2)))

(define (bounds image)
  (receive (x y) (unzip2 (hash-table-keys image))
    (values (minimum x) (maximum x)
            (minimum y) (maximum y))))

(define (iterate data image n)
  (receive (x-min x-max y-min y-max) (bounds image)
    (let ((acc (make-hash-table)) (outside (outside data n)))
      (for-each
        (lambda (coord)
          (hash-table-set! acc coord
            (neighbors data image coord outside)))
        (product (range (- x-min 1) (+ x-max 1))
                 (range (- y-min 1) (+ y-max 1))))
      acc)))

(define (solve data image n)
  (count (cut char=? <> #\#)
    (hash-table-values
      (foldl
        (lambda (acc n)
          (iterate data acc n))
        image (iota n)))))

(receive (data image) (import-input)
  (print (solve data image 2))
  (print (solve data image 50)))
