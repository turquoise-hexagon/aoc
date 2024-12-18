(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

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

(define (id coord)
  (string-intersperse (map number->string coord) ","))

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
  (let ((cache (make-hash-table)) (goal/id (id goal)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 start))))
      (if (priority-queue-empty? queue)
        cache
        (bind (cost coord) (priority-queue-first queue)
          (if (string=? (id coord) goal/id)
            cost
            (loop
              (foldl
                (lambda (queue next/coord)
                  (let ((next/cost (+ cost 1)) (next/id (id next/coord)))
                    (if (< next/cost (hash-table-ref/default cache next/id #e1e6))
                      (begin
                        (hash-table-set! cache next/id next/cost)
                        (priority-queue-insert queue (list next/cost next/coord)))
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
    (let loop ((input input))
      (bind (coord . tail) input
        (array-set! array coord #f)
        (if (number? (path array '(0 0) goal))
          (loop tail)
          (id coord))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 372)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "25,6"))))
