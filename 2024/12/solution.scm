(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant offsets
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (region array coord)
  (let ((acc (make-hash-table)))
    (let loop ((coord coord))
      (hash-table-set! acc coord #t)
      (for-each
        (lambda (index)
          (let ((next (map + coord (vector-ref offsets index))))
            (if (and (array-exists? array next)
                     (char=? (array-ref array coord)
                             (array-ref array next))
                     (not (hash-table-exists? acc next)))
              (loop next))))
        '(0 1 2 3)))
    acc))

(define (regions array)
  (foldl
    (lambda (acc coord)
      (if (any
            (lambda (table)
              (hash-table-exists? table coord))
            acc)
        acc
        (cons (region array coord) acc)))
    '() (array-indexes array)))

(define (import-input)
  (regions (list->array (map string->list (read-lines)))))

(define (perimeter table)
  (apply +
    (map
      (lambda (coord)
        (count
          (lambda (index)
            (not (hash-table-exists? table (map + coord (vector-ref offsets index)))))
          '(0 1 2 3)))
      (hash-table-keys table))))

(define (sides table)
  (apply +
    (map
      (lambda (coord)
        (count
          (lambda (index)
            (let* ((offset/a (vector-ref offsets (modulo (+ index 0) 4)))
                   (offset/b (vector-ref offsets (modulo (+ index 1) 4)))
                   (a (hash-table-exists? table (map + coord offset/a)))
                   (b (hash-table-exists? table (map + coord offset/b)))
                   (c (hash-table-exists? table (map + coord offset/a offset/b))))
              (or (not (or a b)) (and a b (not c)))))
          '(0 1 2 3)))
      (hash-table-keys table))))

(define (solve input proc)
  (apply +
    (map
      (lambda (table)
        (* (proc table) (hash-table-size table)))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input perimeter)))
    (print part/1) (assert (= part/1 1396562)))
  (let ((part/2 (solve input sides)))
    (print part/2) (assert (= part/2 844132))))
