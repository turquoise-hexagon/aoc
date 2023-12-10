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
        (if (and (array-exists? array next) (member offset (map (cut map - <>) (moves array next))))
          next
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
      (let ((acc (make-array (array-dimensions array) #f)))
        (let loop ((coord (start array)))
          (array-set! acc coord #t)
          (let ((nexts (remove
                         (lambda (next)
                           (array-ref acc next))
                         (nexts array coord))))
            (if (null? nexts)
              (return acc)
              (for-each loop nexts))))))))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values (run array) array)))

(define (solve/1 path)
  (quotient (count identity (join (array->list path))) 2))

(define (valid? array path coord)
  (let loop ((coord coord) (acc #f))
    (let ((next (map + coord '(0 -1))))
      (if (array-exists? array next)
        (loop next
          (if (array-ref path next)
            (case (array-ref array next)
              ((#\| #\J #\L) (not acc))
              (else acc))
            acc))
        acc))))

(define (solve/2 path array)
  (count
    (lambda (coord)
      (and (not (array-ref path coord)) (valid? array path coord)))
    (array-indexes array)))

(let-values (((path array) (import-input)))
  (let ((part/1 (solve/1 path)))
    (print part/1) (assert (= part/1 7107)))
  (let ((part/2 (solve/2 path array)))
    (print part/2) (assert (= part/2 281))))
