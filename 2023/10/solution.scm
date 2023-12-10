(import
  (chicken io)
  (euler)
  (srfi 1))

(define (moves array coord)
  (case (array-ref array coord)
    ((#\S) '((-1 +0) (+0 +1) (+1 +0) (+0 -1)))
    ((#\|) '((-1 +0) (+1 +0)))
    ((#\-) '((+0 -1) (+0 +1)))
    ((#\L) '((-1 +0) (+0 +1)))
    ((#\J) '((-1 +0) (+0 -1)))
    ((#\7) '((+1 +0) (+0 -1)))
    ((#\F) '((+1 +0) (+0 +1)))
    ((#\.) '())))

(define (nexts array coord)
  (filter-map
    (lambda (offset)
      (let ((next (map + coord offset)))
        (if (and (array-exists? array next) (member (map - offset) (moves array next)))
          next
          #f)))
    (moves array coord)))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\S))
    (array-indexes array)))

(define (run array)
  (let ((acc (make-array (array-dimensions array) #f)))
    (let loop ((coord (start array)))
      (array-set! acc coord #t)
      (for-each
        (lambda (next)
          (unless (array-ref acc next)
            (loop next)))
        (nexts array coord)))
    acc))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values (run array) array)))

(define (value array)
  (count
    (lambda (coord)
      (array-ref array coord))
    (array-indexes array)))

(define (solve/1 path)
  (quotient (value path) 2))

(define (iterate! path array inside i)
  (let loop ((coord (list i 0)) (acc #f))
    (when (array-exists? array coord)
      (loop (map + coord '(0 1))
        (if (array-ref path coord)
          (case (array-ref array coord)
            ((#\| #\J #\L) (not acc))
            (else acc))
          (begin
            (array-set! inside coord acc)
            acc))))))

(define (solve/2 path array)
  (let ((acc (make-array (array-dimensions array) #f)))
    (for-each
      (lambda (i)
        (iterate! path array acc i))
      (range (sub1 (car (array-dimensions array)))))
    (value acc)))

(let-values (((path array) (import-input)))
  (let ((part/1 (solve/1 path)))
    (print part/1) (assert (= part/1 7107)))
  (let ((part/2 (solve/2 path array)))
    (print part/2) (assert (= part/2 281))))
