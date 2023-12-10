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

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\S))
    (array-indexes array)))

(define (run array)
  (let ((acc (make-array (array-dimensions array) #f)))
    (let loop ((i (start array)))
      (array-set! acc i #t)
      (for-each
        (lambda (o)
          (let ((i (map + i o)))
            (when (and (array-exists? array i) (not (array-ref acc i)) (member (map - o) (moves array i)))
              (loop i))))
        (moves array i)))
    acc))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values (run array) array)))

(define (value array)
  (count
    (lambda (i)
      (array-ref array i))
    (array-indexes array)))

(define (solve/1 path)
  (quotient (value path) 2))

(define (iterate! path array inside coord)
  (let loop ((i coord) (acc #f))
    (when (array-exists? array i)
      (loop (map + i '(0 1))
        (if (array-ref path i)
          (case (array-ref array i)
            ((#\| #\J #\L) (not acc))
            (else acc))
          (begin
            (array-set! inside i acc)
            acc))))))

(define (solve/2 path array)
  (let ((acc (make-array (array-dimensions array) #f)))
    (for-each
      (lambda (i)
        (iterate! path array acc (list i 0)))
      (range (sub1 (car (array-dimensions array)))))
    (value acc)))

(let-values (((path array) (import-input)))
  (let ((part/1 (solve/1 path)))
    (print part/1) (assert (= part/1 7107)))
  (let ((part/2 (solve/2 path array)))
    (print part/2) (assert (= part/2 281))))
