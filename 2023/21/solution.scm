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
    (let loop ((lst offsets) (acc '()))
      (if (null? lst) acc
        (let* ((coord (map2 fx+ coord (car lst))))
          (if (char=? (array-ref array (map2 fxmod coord dimensions)) #\#)
            (loop (cdr lst) acc)
            (loop (cdr lst) (cons coord acc))))))))

(define-inline (adjust n)
  (if (fx< n 0)
    (fx- (fx* -2 n) 1)
    (fx* 2 n)))

(define (id a b)
  (let*
    ((a (adjust a))
     (b (adjust b))
     (_ (fx+ a b)))
    (fx+ (fx/ (fx* _ (fx+ _ 1)) 2) b)))

(define (compute array n)
  (let ((cache (make-vector (+ n 1) #f)))
    (let loop ((lst (list (start array))) (i 0))
      (vector-set! cache i (length lst))
      (if (= i n) cache
        (let ((table (make-hash-table)))
          (for-each
            (lambda (i)
              (for-each
                (lambda (i)
                  (hash-table-set! table (apply id i) i))
                (neighbors array i)))
            lst)
          (loop (hash-table-values table) (+ i 1)))))))

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
