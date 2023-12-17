(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant dirs
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array
    (map
      (lambda (i)
        (map string->number (string-chop i 1)))
      (read-lines))))

(define (? a b)
  (< (car a)
     (car b)))

(define-inline (generate dir)
  (let loop ((i 1) (cost cost) (coord coord))
    (if (> i M) '()
      (let ((coord (map + coord dir)))
        (if (not (array-exists? array coord)) '()
          (let ((cost (+ cost (array-ref array coord))))
            (if (< i m)
              (loop (+ i 1) cost coord)
              (cons (list cost coord dir) (loop (+ i 1) cost coord)))))))))

(define (neighbors array m M cost coord dir)
  (append
    (generate (map + (reverse dir)))
    (generate (map - (reverse dir)))))

(define (cantor a b)
  (let ((_ (+ a b)))
    (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (id a b)
  (cantor
    (apply cantor a)
    (apply cantor b)))

(define (path array m M coord)
  (let ((acc (make-hash-table)))
    (do ((queue (list->priority-queue (map (lambda (i) (list 0 coord i)) dirs) ?)
           (foldl
             (lambda (queue i)
               (bind (cost coord dir) i
                 (let ((id (id coord dir)))
                   (if (< cost (hash-table-ref/default acc id #e1e6))
                     (begin
                       (hash-table-set! acc id cost)
                       (priority-queue-insert queue i))
                     queue))))
             (priority-queue-rest queue) (apply neighbors array m M (priority-queue-first queue)))))
      ((priority-queue-empty? queue) acc))))

(define (solve input m M)
  (let ((acc (path input m M '(0 0))) (coord (map sub1 (array-dimensions input))))
    (apply min
      (map
        (lambda (i)
          (hash-table-ref/default acc (id coord i) #e1e6))
        dirs))))

(let ((input (import-input)))
  (let ((part/1 (solve input 1 3)))
    (print part/1) (assert (= part/1 859)))
  (let ((part/2 (solve input 4 10)))
    (print part/2) (assert (= part/2 1027))))
