(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (euler)
  (srfi 1))

(define-constant dirs
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

(define (? a b)
  (fx<
    (car a)
    (car b)))

(define-inline (generate dir)
  (let ((offset (vector-ref dirs dir)))
    (let loop ((i 1) (cost cost) (coord coord))
      (if (fx> i M) '()
        (let ((coord (map fx+ coord offset)))
          (if (not (array-exists? array coord)) '()
            (let ((cost (fx+ cost (array-ref array coord))))
              (if (fx< i m)
                (loop (fx+ i 1) cost coord)
                (cons (list cost coord dir) (loop (fx+ i 1) cost coord))))))))))

(define (neighbors array m M cost coord dir)
  (append
    (generate (fxmod (fx+ (fx+ dir 1) 4) 4))
    (generate (fxmod (fx+ (fx- dir 1) 4) 4))))

(define (path array m M coord)
  (let ((acc (make-array (cons 4 (array-dimensions array)) #e1e6)))
    (do ((queue (list->priority-queue (map (lambda (i) (list 0 coord i)) '(0 1 2 3)) ?)
           (foldl
             (lambda (queue i)
               (apply
                 (lambda (cost coord dir)
                   (let ((id (cons dir coord)))
                     (if (fx< cost (array-ref acc id))
                       (begin
                         (array-set! acc id cost)
                         (priority-queue-insert queue i))
                       queue)))
                 i))
             (priority-queue-rest queue) (apply neighbors array m M (priority-queue-first queue)))))
      ((priority-queue-empty? queue) acc))))

(define (solve input m M)
  (let ((acc (path input m M '(0 0))) (coord (map sub1 (array-dimensions input))))
    (apply min
      (map
        (lambda (i)
          (array-ref acc (cons i coord)))
        '(0 1 2 3)))))

(let ((input (import-input)))
  (let ((part/1 (solve input 1 3)))
    (print part/1) (assert (= part/1 859)))
  (let ((part/2 (solve input 4 10)))
    (print part/2) (assert (= part/2 1027))))
