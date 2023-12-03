(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets (delete-first (power '(-1 0 1) 2) '(0 0) equal?))

(define (number-start array coord)
  (let loop ((coord coord))
    (let ((next (map + coord '(0 -1))))
      (if (array-exists? array next)
        (if (char-numeric? (array-ref array next))
          (loop next)
          coord)
        coord))))

(define (number array coord)
  (let loop ((coord coord) (acc (list coord)))
    (let ((next (map + coord '(0 1))))
      (if (array-exists? array next)
        (if (char-numeric? (array-ref array next))
          (loop next (cons next acc))
          (reverse acc))
        (reverse acc)))))

(define (numbers array)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (coord)
        (when (char-numeric? (array-ref array coord))
          (hash-table-set! mem (number-start array coord) #t)))
      (array-indexes array))
    (map
      (lambda (coord)
        (number array coord))
      (hash-table-keys mem))))

(define (neighbors array coord)
  (filter
    (lambda (coord)
      (array-exists? array coord))
    (map
      (lambda (offset)
        (map + coord offset))
      offsets)))

(define (part-numbers array)
  (remove
    (lambda (coords)
      (every
        (lambda (coord)
          (every
            (lambda (coord)
              (let ((value (array-ref array coord)))
                (or (char-numeric? value) (char=? value #\.))))
            (neighbors array coord)))
        coords))
    (numbers array)))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values array (part-numbers array))))

(define (convert array coords)
  (string->number
    (list->string
      (map
        (lambda (coord)
          (array-ref array coord))
        coords))))

(define (maybe-gears array)
  (filter
    (lambda (coord)
      (char=? (array-ref array coord) #\*))
    (array-indexes array)))

(define (solve/1 input part-numbers)
  (foldl
    (lambda (acc coords)
      (+ acc (convert input coords)))
    0 part-numbers))

(define (adjascent-part-numbers array table coord)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (coord)
        (when (hash-table-exists? table coord)
          (hash-table-set! mem (number-start array coord) #t)))
      (neighbors array coord))
    (map
      (lambda (coord)
        (convert array (number array coord)))
      (hash-table-keys mem))))

(define (solve/2 input part-numbers)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (coords)
        (for-each
          (lambda (coord)
            (hash-table-set! mem coord #t))
          coords))
      part-numbers)
    (foldl
      (lambda (acc coord)
        (let ((result (adjascent-part-numbers input mem coord)))
          (if (= (length result) 2)
            (+ acc (apply * result))
            acc)))
      0 (maybe-gears input))))

(let-values (((array part-numbers) (import-input)))
  (let ((part/1 (solve/1 array part-numbers)))
    (print part/1) (assert (= part/1 530495)))
  (let ((part/2 (solve/2 array part-numbers)))
    (print part/2) (assert (= part/2 80253814))))
