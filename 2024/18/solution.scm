(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ",")))
    (read-lines)))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (neighbors array coord)
  (filter-map
    (lambda (offset)
      (let ((next (map + coord offset)))
        (if (and (array-exists? array next) (array-ref array next))
          next
          #f)))
    offsets))

(define (path array start goal)
  (let ((cache (make-array (array-dimensions array) #e1e8)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 start))))
      (if (priority-queue-empty? queue)
        cache
        (bind (cost coord) (priority-queue-first queue)
          (if (equal? coord goal)
            cost
            (loop
              (foldl
                (lambda (queue coord)
                  (let ((cost (+ cost 1)))
                    (if (> (array-ref cache coord) cost)
                      (begin
                        (array-set! cache coord cost)
                        (priority-queue-insert queue (list cost coord)))
                      queue)))
                (priority-queue-rest queue) (neighbors array coord)))))))))

(define (solve/1 input)
  (let* ((goal (apply map max input)) (array (make-array (map add1 goal) #t)))
    (for-each
      (lambda (coord)
        (array-set! array coord #f))
      (take input 1024))
    (path array '(0 0) goal)))

(define (solve/2 input)
  (let* ((goal (apply map max input)) (array (make-array (map add1 goal) #t)))
    (let loop ((lst input))
      (bind (coord . tail) lst
        (array-set! array coord #f)
        (if (array? (path array '(0 0) goal))
          (string-intersperse (map number->string coord) ",")
          (loop tail))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 372)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "25,6"))))
