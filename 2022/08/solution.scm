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

(define (_visible? array coord value offset)
  (let loop ((coord coord))
    (let ((_ (map + coord offset)))
      (if (array-exists? array _)
        (if (> value (array-ref array _))
          (loop _)
          #f)
        #t))))

(define (visible? array coord)
  (let ((value (array-ref array coord)))
    (any
      (lambda (offset)
        (_visible? array coord value offset))
      offsets)))

(define (_scenic-score array coord value offset)
  (let loop ((coord coord) (acc 0))
    (let ((_ (map + coord offset)))
      (if (array-exists? array _)
        (if (> value (array-ref array _))
          (loop _ (+ acc 1))
          (+ acc 1))
        acc))))

(define (scenic-score array coord)
  (let ((value (array-ref array coord)))
    (apply *
      (map
        (lambda (offset)
          (_scenic-score array coord value offset))
        offsets))))

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
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1798)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 259308))))
