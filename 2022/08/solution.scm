(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array
    (map
      (lambda (str)
        (map string->number (string-chop str 1)))
      (read-lines))))

(define (_neighbors array coord offset)
  (let loop ((coord coord) (acc '()))
    (let ((_ (map + coord offset)))
      (if (array-exists? array _)
        (loop _ (cons _ acc))
        (reverse acc)))))

(define (neighbors array coord)
  (map
    (lambda (offset)
      (_neighbors array coord offset))
    offsets))

(define (visible? array coord)
  (let ((value (array-ref array coord)))
    (any
      (lambda (coords)
        (every
          (lambda (coord)
            (> value (array-ref array coord)))
          coords))
      (neighbors array coord))))

(define (_scenic-score array value coords)
  (let loop ((lst coords) (acc 0))
    (if (null? lst)
      acc
      (let ((_ (+ acc 1)))
        (if (> value (array-ref array (car lst)))
          (loop (cdr lst) _)
          _)))))

(define (scenic-score array coord)
  (let ((value (array-ref array coord)))
    (apply *
      (map
        (lambda (coords)
          (_scenic-score array value coords))
        (neighbors array coord)))))

(define (solve/1 input)
  (count
    (lambda (coord)
      (visible? input coord))
    (array-indexes input)))

(define (solve/2 input)
  (apply max
    (map
      (lambda (coord)
        (scenic-score input coord))
      (array-indexes input))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
