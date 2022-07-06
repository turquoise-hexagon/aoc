(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

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

(define (comp? a b)
  (< (car a)
     (car b)))

(define (helper! array table queue cost coord)
  (foldl
    (lambda (queue next)
      (let ((cost (+ (array-ref array next) cost)))
        (if (> (hash-table-ref/default table next +inf.0) cost)
          (begin
            (hash-table-set! table next cost)
            (priority-queue-insert comp? `(,cost ,next) queue))
          queue)))
    (priority-queue-rest comp? queue) (neighbors array coord)))

(define (solve array from to)
  (let ((acc (make-hash-table))) 
    (let loop ((queue (priority-queue-insert comp? `(0 ,from) priority-queue-empty)))
      (if (priority-queue-empty? queue)
        (hash-table-ref acc to)
        (apply
          (lambda (cost coord)
            (loop (helper! array acc queue cost coord)))
          (priority-queue-first queue))))))

(let* ((input/1 (import-input)) (input/2 (transform input/1 5)))
  (print (solve input/1 '(0 0) (map (cut - <> 1) (array-dimensions input/1))))
  (print (solve input/2 '(0 0) (map (cut - <> 1) (array-dimensions input/2)))))
