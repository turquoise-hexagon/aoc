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

(define-inline (_iterate dir)
  (let ((_ (vector-ref dirs dir)))
    (let loop ((i 1) (queue queue) (cost cost) (coord coord))
      (if (fx> i M) queue
        (let ((coord (list (fx+ (car coord) (car _)) (fx+ (cadr coord) (cadr _)))))
          (if (not (array-exists? array coord)) queue
            (let ((cost (fx+ cost (array-ref array coord))))
              (if (fx< i m)
                (loop (fx+ i 1) queue cost coord)
                (let ((id (cons dir coord)))
                  (if (fx< cost (array-ref acc id))
                    (begin
                      (array-set! acc id cost)
                      (loop (fx+ i 1) (priority-queue-insert queue (list cost coord dir)) cost coord))
                    (loop (fx+ i 1) queue cost coord)))))))))))

(define (iterate array m M acc queue cost coord dir)
  (let*
    ((queue (_iterate (fxmod (fx+ dir 1) 4)))
     (queue (_iterate (fxmod (fx- dir 1) 4))))
    queue))

(define (path array m M coord)
  (let ((acc (make-array (cons 4 (array-dimensions array)) #e1e6)))
    (do ((queue
           (list->priority-queue (map (lambda (i) (list 0 coord i)) '(0 1 2 3)) ?)
           (apply iterate array m M acc
             (priority-queue-rest  queue)
             (priority-queue-first queue))))
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
