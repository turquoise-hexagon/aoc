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
  (let ((_ (irregex-extract "-?[0-9]+" str)))
    (match (chop (map string->number _) 2)
      ((a b)
       (list a (distance a b))))))

(define (import-input)
  (map parse-sensor (read-lines)))

(define (solve/1 input n)
  (define process-sensor
    (match-lambda
      (((x y) r)
       (let ((_ (- r (abs (- y n)))))
         (if (> _ 0)
           (list
             (- x _)
             (+ x _))
           #f)))))

  (define (generate-ranges)
    (sort (filter-map process-sensor input)
      (lambda (a b)
        (< (car a)
           (car b)))))

  (define (find-spaces)
    (let ((_ (generate-ranges)))
      (foldl
        (lambda (acc next)
          (match-let*
            (((head . tail) acc)
             ((a b) head)
             ((c d) next))
            (let ((temp (list a (max b d))))
              (if (>= b c)
                (cons temp tail)
                (cons head acc)))))
        (list (car _)) (cdr _))))

  (apply +
    (map
      (lambda (_)
        (apply - (reverse _)))
      (find-spaces))))

(define (solve/2 input n)
  (let ((t (make-hash-table))
        (a (make-hash-table))
        (b (make-hash-table)))

    (define (in-bound? point)
      (every
        (lambda (_)
          (<= 0 _ n))
        point))

    (define (in-range? point)
      (every
        (lambda (_)
          (> (distance _ point) (hash-table-ref t _)))
        (hash-table-keys t)))

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
           (if (and (in-bound? p)
                    (in-range? p))
             (+ (* x n) y)
             (loop tail))))))))

(let ((input (import-input)))
  (print (solve/1 input #e2e6))
  (print (solve/2 input #e4e6)))
