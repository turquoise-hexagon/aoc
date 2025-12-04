(import
  (chicken io)
  (euler)
  (srfi 1))

(define offsets
  (delete-first (power '(-1 0 1) 2) '(0 0) equal?))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (neighbors array coord)
  (filter
    (lambda (coord)
      (array-exists? array coord))
    (map
      (lambda (offset)
        (map + coord offset))
      offsets)))

(define (neighbors-rolls-count array coord)
  (count
    (lambda (coord)
      (char=? (array-ref array coord) #\@))
    (neighbors array coord)))

(define (accessibles array)
  (filter
    (lambda (coord)
      (and (char=? (array-ref array coord) #\@) (< (neighbors-rolls-count array coord) 4)))
    (array-indexes array)))

(define (solve/1 input)
  (length (accessibles input)))

(define (solve/2 input)
  (let loop ((acc 0))
    (let ((lst (accessibles input)))
      (if (null? lst)
        acc
        (begin
          (for-each
            (lambda (coord)
              (array-set! input coord #\.))
            lst)
          (loop (+ acc (length lst))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1393)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 8643))))
