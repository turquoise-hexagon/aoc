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

(define (neighbors array coord)
  (map
    (lambda (offset)
      (let loop ((coord coord) (acc '()))
        (let ((_ (map + coord offset)))
          (if (array-exists? array _)
            (loop _ (cons _ acc))
            (reverse acc)))))
    offsets))

(define (import-input)
  (list->array
    (map
      (lambda (_)
        (map string->number (string-chop _ 1)))
      (read-lines))))

(define (visible? array coord)
  (let ((value (array-ref array coord)))
    (any
      (lambda (coords)
        (every
          (lambda (_)
            (> value (array-ref array _)))
          coords))
      (neighbors array coord))))

(define (scenic-score array coord)
  (let ((value (array-ref array coord)))
    (apply *
      (map
        (lambda (coords)
          (let loop ((lst coords) (acc 0))
            (if (null? lst)
              acc
              (let ((_ (+ acc 1)))
                (if (> value (array-ref array (car lst)))
                  (loop (cdr lst) _)
                  _)))))
        (neighbors array coord)))))

(define (solve/1 input)
  (count
    (lambda (_)
      (visible? input _))
    (array-indexes input)))

(define (solve/2 input)
  (apply max
    (map
      (lambda (_)
        (scenic-score input _))
      (array-indexes input))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
