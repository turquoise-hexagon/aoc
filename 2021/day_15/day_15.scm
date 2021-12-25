(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

; (include-relative "utils/array.scm")
(include-relative "utils/queue.scm")

(define (import-input)
  (list->array
    (map (cut map string->number <>)
      (map (cut string-chop <> 1)
        (read-lines)))))

(define (transform/h lst i)
  (map
    (lambda (n)
      (+ (modulo (+ n i -1) 9) 1))
    lst))

(define (transform array n)
  (let ((lst (array->list array)))
    (let* ((lst (join (map (lambda (i) (map (cut transform/h <> i) lst)) (iota n))))
           (lst (map (lambda (i) (join (map (cut transform/h i <>) (iota n)))) lst)))
      (list->array lst))))

(define (neighbors array coord)
  (filter (cut array-exists? array <>)
    (map (cut map + <> coord) '((1 0) (0 1) (-1 0) (0 -1)))))

(define (solve/h array distances queue current)
  (receive (distance coord) (apply values current)
    (for-each
      (lambda (neighbor)
        (let ((distance (+ (array-ref array neighbor) distance)))
          (if (hash-table-exists? distances neighbor)
            (when (> (hash-table-ref distances neighbor) distance)
              (hash-table-set! distances neighbor distance))
            (begin
              (hash-table-set! distances neighbor distance)
              (queue-push! queue distance neighbor)))))
      (neighbors array coord))))

(define (solve array from to)
  (let ((acc (make-hash-table)) (queue (queue)))
    (let loop ((current `(0 ,from)))
      (if (null? current)
        (hash-table-ref acc to)
        (begin
          (solve/h array acc queue current)
          (loop (queue-pop! queue)))))))

(let* ((input/1 (import-input)) (input/2 (transform input/1 5)))
  (print (solve input/1 '(0 0) (map (cut - <> 1) (array-dimensions input/1))))
  (print (solve input/2 '(0 0) (map (cut - <> 1) (array-dimensions input/2)))))
