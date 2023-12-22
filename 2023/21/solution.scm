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
  (let ((acc (make-vector (+ n 1))) (d (array-dimensions array)))
    (do ((i 0 (+ i 1))
         (l (list (start array))
           (let ((mem (make-hash-table #:size #e1e5)))
             (for-each
               (lambda (i)
                 (let loop ((l offsets))
                   (unless (null? l)
                     (let ((i (map2 fx+ i (car l))))
                       (unless (char=? (array-ref array (map2 fxmod i d)) #\#)
                         (hash-table-set! mem (apply id i) i)))
                     (loop (cdr l))))) l)
             (hash-table-values mem))))
      ((> i n) acc)
      (vector-set! acc i (length l)))))

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

(let ((parts (solve (import-input) 26501365)))
  (for-each print parts)
  (assert (equal? parts '(3682 609012263058042))))
