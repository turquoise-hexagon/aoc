(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant dirs
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define-constant nb-dirs (vector-length dirs))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (cantor a b)
  (let ((_ (+ a b))) (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (split mem test? lst dir coord)
  (let ((id (foldl cantor dir coord)))
    (if (hash-table-exists? mem id) '()
      (begin
        (hash-table-set! mem id #t)
        (if (test? dir) lst `(,dir))))))

(define (bounce test? dir)
  `(,(modulo (+ dir nb-dirs (if (test? dir) +1 -1)) nb-dirs)))

(define (run array dir coord)
  (let ((acc (array-copy array)) (mem (make-hash-table)))
    (let loop ((dir dir) (coord coord))
      (when (array-exists? array coord)
        (array-set! acc  coord #\#)
        (for-each
          (lambda (dir)
            (loop dir (map + coord (vector-ref dirs dir))))
          (case (array-ref array coord)
            ((#\.) `(,dir))
            ((#\/) (bounce even? dir))
            ((#\\) (bounce odd?  dir))
            ((#\|) (split mem odd?  '(0 2) dir coord))
            ((#\-) (split mem even? '(1 3) dir coord))))))
    (count
      (lambda (coord)
        (char=? (array-ref acc coord) #\#))
      (array-indexes acc))))

(define (solve/1 input)
  (run input 1 '(0 0)))

(define (solve/2 input)
  (apply
    (lambda (h w)
      (apply max
        (map
          (lambda (i)
            (apply run input i))
          (append
            (product '(0) (product `(,h) (range w)))
            (product '(1) (product (range h)  '(0)))
            (product '(2) (product  '(0) (range w)))
            (product '(3) (product (range h) `(,w)))))))
    (map sub1 (array-dimensions input))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 8249)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 8444))))
