(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (find-coordinates array value)
  (filter
    (lambda (coord)
      (char=? (array-ref array coord) value))
    (array-indexes array)))

(define (correct-value array coord)
  (let ((_ (array-ref array coord)))
    (case _
      ((#\E) (char->integer #\z))
      ((#\S) (char->integer #\a))
      (else  (char->integer   _)))))

(define (neighbors array coord)
  (let ((_ (correct-value array coord)))
    (filter
      (lambda (coord)
        (>= 1 (- (correct-value array coord) _)))
      (filter
        (lambda (coord)
          (array-exists? array coord))
        (map
          (lambda (offset)
            (map + coord offset))
          offsets)))))

(define (comp? a b)
  (< (car a)
     (car b)))

(define (helper! array acc queue cost coord)
  (foldl
    (lambda (queue coord)
      (let ((cost (+ cost 1)))
        (if (> (hash-table-ref/default acc coord #e1e32) cost)
          (begin
            (hash-table-set! acc coord cost)
            (priority-queue-insert comp? (list cost coord) queue))
          queue)))
    (priority-queue-rest comp? queue) (neighbors array coord)))

(define (find-path array from to)
  (let ((acc (make-hash-table)))
    (let loop ((queue (list->priority-queue comp? (list (list 0 from)))))
      (if (priority-queue-empty? queue)
        (hash-table-ref/default acc to #e1e32)
        (apply
          (lambda (cost coord)
            (loop (helper! array acc queue cost coord)))
          (priority-queue-first queue))))))

(define (solve/1 input)
  (let ((a (first (find-coordinates input #\S)))
        (b (first (find-coordinates input #\E))))
    (find-path input a b)))

(define (solve/2 input)
  (let ((b (first (find-coordinates input #\E))))
    (apply min
      (map
        (lambda (a)
          (find-path input a b))
        (find-coordinates input #\a)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
