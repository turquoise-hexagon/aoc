(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (matchable)
  (euler)
  (srfi 1)
  (srfi 69))

(define (distance a b)
  (apply + (map abs (map - a b))))

(define (parse-sensor str)
  (match
    (let ((_ (irregex-extract "-?[0-9]+" str)))
      (chop (map string->number _) 2))
    ((a b)
     (list a (distance a b)))))

(define (import-input)
  (map parse-sensor (read-lines)))

(define (_ranges lst n)
  (match lst
    (((x y) r)
     (let ((_ (- r (abs (- y n)))))
       (if (> _ 0)
         (list
           (- x _)
           (+ x _))
         #f)))))

(define (ranges lst n)
  (sort
    (filter-map
      (lambda (_)
        (_ranges _ n))
      lst)
    (lambda (a b)
      (< (car a)
         (car b)))))

(define (find-spaces lst n)
  (let ((_ (ranges lst n)))
    (foldl
      (lambda (acc next)
        (match-let*
          (((head . tail) acc)
           ((a b) head)
           ((c d) next))
          (if (>= b c)
            (cons (list a (max b d)) tail)
            (cons head acc))))
      (list (car _)) (cdr _))))

(define (solve/1 input n)
  (apply +
    (map
      (lambda (_)
        (apply - (reverse _)))
      (find-spaces input n))))

(define (in-bound? point bound)
  (every
    (lambda (_)
      (<= 0 _ bound))
    point))

(define (in-range? point table)
  (every
    (lambda (_)
      (> (distance _ point) (hash-table-ref table _)))
    (hash-table-keys table)))

(define (solve/2 input n)
  (let ((t (make-hash-table))
        (a (make-hash-table))
        (b (make-hash-table)))

    (for-each
      (lambda (_)
        (apply hash-table-set! t _))
      input)

    (for-each
      (match-lambda
        (((x y) r)
         (hash-table-set! a (+ (- y x) (+ r 1)) #t)
         (hash-table-set! a (- (- y x) (- r 1)) #t)
         (hash-table-set! b (+ (+ y x) (+ r 1)) #t)
         (hash-table-set! b (- (+ y x) (- r 1)) #t)))
      input)

    (let loop ((lst (product (hash-table-keys a) (hash-table-keys b))))
      (match lst
        (((a b) . tail)
         (let*
           ((x (quotient (- b a) 2))
            (y (quotient (+ b a) 2))
            (p (list x y)))
           (if (and (in-bound? p n)
                    (in-range? p t))
             (+ (* x n) y)
             (loop tail))))))))

(let ((input (import-input)))
  (print (solve/1 input #e2e6))
  (print (solve/2 input #e4e6)))
