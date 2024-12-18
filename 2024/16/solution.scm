(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1))

(define-constant OFFSETS
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define-constant DIRS '(0 1 2 3))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (next array cost dir coord)
  (let ((acc (list (list (+ cost 1000) (modulo (+ dir +1 4) 4) coord)
                   (list (+ cost 1000) (modulo (+ dir -1 4) 4) coord)))
        (coord (map + coord (vector-ref OFFSETS dir))))
    (if (not (char=? (array-ref array coord) #\#))
      (cons (list (+ cost 1) dir coord) acc)
      acc)))

(define (compute array lst)
  (let ((acc (make-array (cons 4 (array-dimensions array)) #e1e8)))
    (do ((queue
           (list->priority-queue
             (map
               (lambda (item)
                 (bind (dir coord) item
                   (array-set! acc (cons dir coord) 0)
                   (list 0 dir coord)))
               lst)
             compare?)
           (bind (cost dir coord) (priority-queue-first queue)
             (foldl
               (lambda (queue item)
                 (bind (cost dir coord) item
                   (if (> (array-ref acc (cons dir coord)) cost)
                     (begin
                       (array-set! acc (cons dir coord) cost)
                       (priority-queue-insert queue item))
                     queue)))
               (priority-queue-rest queue) (next array cost dir coord)))))
      ((priority-queue-empty? queue) acc))))

(define (locate array value)
  (find
    (lambda (coord)
      (char=? (array-ref array coord) value))
    (array-indexes array)))

(define (solve input)
  (let* ((S (locate input #\S))
         (E (locate input #\E))
         (acc/S (compute input (list (list 1 S))))
         (acc/E (compute input
                  (map
                    (lambda (dir)
                      (list dir E))
                    DIRS)))
         (part/1 (apply min
                   (map
                     (lambda (dir)
                       (array-ref acc/S (cons dir E)))
                     DIRS)))
         (part/2 (count
                   (lambda (coord)
                     (any
                       (lambda (dir)
                         (= (+ (array-ref acc/S (cons (modulo (+ dir 0 4) 4) coord))
                               (array-ref acc/E (cons (modulo (+ dir 2 4) 4) coord)))
                            part/1))
                       DIRS))
                   (array-indexes input))))
    (list part/1 part/2)))

(let ((parts (solve (import-input))))
  (for-each print parts) (assert (equal? parts '(105508 548))))
