(import
  (chicken io)
  (chicken fixnum)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\S))
    (array-indexes array)))

(define-inline (map2 fun a b)
  (list
    (fun (car  a) (car  b))
    (fun (cadr a) (cadr b))))

(define (neighbors array coord)
  (let ((dimensions (array-dimensions array)))
    (let loop ((l offsets) (acc '()))
      (if (null? l) acc
        (let* ((coord (map2 fx+ coord (car l))))
          (if (char=? (array-ref array (map2 fxmod coord dimensions)) #\#)
            (loop (cdr l) acc)
            (loop (cdr l) (cons coord acc))))))))

(define-inline (adjust n)
  (if (fx< n 0)
    (fx- -1 (fx+ n n))
    (fx+ n n)))

(define (id a b)
  (let*
    ((a (adjust a))
     (b (adjust b))
     (_ (fx+ a b)))
    (fx+ (fx/ (fx* _ (fx+ _ 1)) 2) b)))

(define (compute array n)
  (let ((acc (make-vector (+ n 1))))
    (let loop ((l (list (start array))) (i 0))
      (vector-set! acc i (length l))
      (if (= i n) acc
        (let ((mem (make-hash-table)))
          (for-each
            (lambda (i)
              (for-each
                (lambda (i)
                  (hash-table-set! mem (apply id i) i))
                (neighbors array i)))
            l)
          (loop (hash-table-values mem) (+ i 1)))))))

(define (interpolate n a b c)
  (let
    ((x (- a 0))
     (y (- b a))
     (z (- c b)))
    (+ x (* y n) (* (quotient (* n (- n 1)) 2) (- z y)))))

(define (solve input n)
  (let* ((h (first (array-dimensions input))) (i (modulo n h)) (acc (compute input (+ i h h))))
    (list (vector-ref acc 64)
      (interpolate (quotient n h)
        (vector-ref acc (+ i))
        (vector-ref acc (+ i h))
        (vector-ref acc (+ i h h))))))

(let ((input (import-input)))
  (let ((parts (solve input 26501365)))
    (for-each print parts)
    (assert (equal? parts '(3682 609012263058042)))))
