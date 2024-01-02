(import
  (chicken io)
  (euler)
  (euler-syntax)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (next array cost coord)
  (map
    (lambda (i)
      (list (+ cost 1) i))
    (remove
      (lambda (i)
        (char=? (array-ref array i) #\#))
      (map
        (lambda (i)
          (map + coord i))
        offsets))))

(define (compare? a b)
  (< (car a)
     (car b)))

(define-memoized (path array coord)
  (let ((acc (make-array (array-dimensions array) #f)))
    (do
      ((queue
         (priority-queue-insert (priority-queue compare?) (list 0 coord))
         (foldl
           (lambda (queue i)
             (apply
               (lambda (cost coord)
                 (if (or (not (array-ref acc coord)) (< cost (array-ref acc coord)))
                   (begin
                     (array-set! acc coord cost)
                     (priority-queue-insert queue i))
                   queue))
               i))
           (priority-queue-rest queue) (apply next array (priority-queue-first queue)))))
      ((priority-queue-empty? queue) acc))))

(define (marked array)
  (filter
    (lambda (i)
      (case (array-ref array i)
        ((#\#) #f)
        ((#\.) #f)
        ((#\0) #f)
        (else  #t)))
    (array-indexes array)))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\0))
    (array-indexes array)))

(define (proc/1 coord lst) (append (list coord) lst))
(define (proc/2 coord lst) (append (list coord) lst (list coord)))

(define (solve input proc)
  (apply min
    (map
      (lambda (i)
        (apply +
          (map
            (lambda (a b)
              (array-ref (path input a) b))
            i (cdr i))))
      (let ((coord (start input)))
        (map
          (lambda (i)
            (proc coord i))
          (permutations (marked input)))))))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 442)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 660))))
