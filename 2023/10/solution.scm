(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define (moves array coord)
  (case (array-ref array coord)
    ((#\|) '((-1  0) ( 1  0)))
    ((#\-) '(( 0 -1) ( 0  1)))
    ((#\L) '((-1  0) ( 0  1)))
    ((#\J) '((-1  0) ( 0 -1)))
    ((#\7) '(( 1  0) ( 0 -1)))
    ((#\F) '(( 1  0) ( 0  1)))
    ((#\S) '((-1  0) ( 0  1) ( 1  0) ( 0 -1)))
    ((#\.) '())))

(define (nexts array coord)
  (filter-map
    (lambda (offset)
      (let ((next (map + coord offset)))
        (if (and (array-exists? array next))
          (if (member offset (map (cut map - <>) (moves array next)))
            next
            #f)
          #f)))
    (moves array coord)))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\S))
    (array-indexes array)))

(define (run array)
  (call/cc
    (lambda (return)
      (let loop ((coord (start array)) (acc '()))
        (let ((nexts (remove
                       (lambda (next)
                         (member next acc))
                       (nexts array coord))))
          (if (null? nexts)
            (return acc)
            (for-each
              (lambda (next)
                (loop next (cons next acc)))
              nexts)))))))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values (run array) array)))

(define (solve/1 path)
  (quotient (length path) 2))

(define (valid? array table coord)
  (let loop ((coord coord) (acc #f))
    (let ((next (map + coord '(0 -1))))
      (if (array-exists? array next)
        (loop next
          (if (hash-table-exists? table next)
            (case (array-ref array next)
              ((#\|) (not acc))
              ((#\J) (not acc))
              ((#\L) (not acc))
              (else acc))
            acc))
        acc))))

(define (solve/2 path array)
  (let ((table (make-hash-table)))
    (for-each
      (lambda (coord)
        (hash-table-set! table coord #t))
      path)
    (count
      (lambda (coord)
        (valid? array table coord))
      (remove
        (lambda (coord)
          (hash-table-exists? table coord))
        (array-indexes array)))))

(let-values (((path array) (import-input)))
  (let ((part/1 (solve/1 path)))
    (print part/1) (assert (= part/1 7107)))
  (let ((part/2 (solve/2 path array)))
    (print part/2) (assert (= part/2 281))))
