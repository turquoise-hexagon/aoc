(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant offsets
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array
    (map
      (lambda (i)
        (map string->number (string-chop i 1)))
      (read-lines))))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (generate array min-moves max-moves cost coord dir)
  (let ((offset (vector-ref offsets dir)))
    (let loop ((i 1) (cost cost) (coord coord))
      (if (> i max-moves)
        '()
        (let ((coord (map + coord offset)))
          (if (array-exists? array coord)
            (let ((cost (+ cost (array-ref array coord))))
              (if (< i min-moves)
                (loop (+ i 1) cost coord)
                (cons (list cost coord dir) (loop (+ i 1) cost coord))))
            '()))))))

(define (neighbors array min-moves max-moves cost coord dir)
  (append-map
    (lambda (i)
      (generate array min-moves max-moves cost coord (modulo (+ dir i 4) 4)))
    '(-1 1)))

(define (cantor a b)
  (let ((_ (+ a b)))
    (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (id dir coord)
  (foldl cantor dir coord))

(define (path array min-moves max-moves coord) 
  (let ((acc (make-hash-table)))
    (do ((queue
           (list->priority-queue (map (lambda (i) (list 0 coord i)) '(0 1 2 3)) compare?)
           (foldl
             (lambda (queue next)
               (apply
                 (lambda (cost coord dir)
                   (let ((id (id dir coord)))
                     (if (< cost (hash-table-ref/default acc id #e1e8))
                       (begin
                         (hash-table-set! acc id cost)
                         (priority-queue-insert queue next))
                       queue)))
                 next))
             (priority-queue-rest queue)
             (apply neighbors array min-moves max-moves (priority-queue-first queue)))))
      ((priority-queue-empty? queue) acc))))

(define (solve input min-moves max-moves)
  (let ((acc (path input min-moves max-moves '(0 0))) (coord (map sub1 (array-dimensions input))))
    (apply min
      (map
        (lambda (i)
          (hash-table-ref/default acc (id i coord) #e1e8))
        '(0 1 2 3)))))

(let ((input (import-input)))
  (print (solve input 1 3))
  (print (solve input 4 10)))
