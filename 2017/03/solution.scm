(import
  (chicken fixnum)
  (euler)
  (srfi 1))

(define-constant directions
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define-constant offsets
  '((-1 -1) (-1  0) (-1  1)
    ( 0 -1)         ( 0  1)
    ( 1 -1) ( 1  0) ( 1  1)))

(define-inline (map2 fun a b)
  (list
    (fun (car  a) (car  b))
    (fun (cadr a) (cadr b))))

(define (import-input)
  (read))

(define (neighbors mem coord)
  (filter-map
    (lambda (i)
      (let ((i (map2 fx+ coord i)))
        (if (array-ref mem i) i #f)))
    offsets))

(define (move mem coord direction proc)
  (let* ((direction (let ((count (length (neighbors mem coord))))
                      (if (or (fx= count 1)
                              (fx= count 2))
                        (fxmod (fx+ direction 1) 4)
                        direction)))
         (next (map2 fx+ coord (vector-ref directions direction))))
    (list next direction (proc mem coord next))))

(define (run input proc)
  (let ((mem (make-array '(1000 1000) #f)))
    (let loop ((coord '(500 500)) (direction 0) (value 1))
      (array-set! mem coord value)
      (if (fx>= value input)
        (list coord value)
        (apply loop (move mem coord direction proc))))))

(define (proc/1 mem coord next)
  (fx+ (array-ref mem coord) 1))

(define (proc/2 mem coord next)
  (foldl
    (lambda (acc i)
      (fx+ acc (array-ref mem i)))
    0 (neighbors mem next)))

(define (solve/1 input)
  (apply
    (lambda (coord _)
      (apply + (map abs (map - '(500 500) coord))))
    (run input proc/1)))

(define (solve/2 input)
  (apply
    (lambda (_ value)
      value)
    (run input proc/2)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 430)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 312453))))
