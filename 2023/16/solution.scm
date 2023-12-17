(import
  (chicken io)
  (chicken fixnum)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant dirs
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (pi a b)
  (let ((_ (fx+ a b))) (fx+ (fx/ (fx* _ (fx+ _ 1)) 2) b)))

(define-inline (split test? a b)
  (let ((id (foldl pi dir coord)))
    (unless (hash-table-exists? mem id)
      (hash-table-set! mem id #t)
      (if (test? dir)
        (begin
          (next a)
          (next b))
        (next dir)))))

(define (run array dir coord)
  (let ((acc (array-copy array)) (mem (make-hash-table)))
    (let loop ((dir dir) (coord coord))
      (define (next dir)
        (let ((_ (vector-ref dirs dir)))
          (loop dir (list (fx+ (car coord) (car _)) (fx+ (cadr coord) (cadr _))))))
      (when (array-exists? array coord)
        (array-set! acc coord #\#)
        (case (array-ref array coord)
          ((#\/) (next (fxmod (fx+ dir (if (fxeven? dir) 1 -1)) 4)))
          ((#\\) (next (fxmod (fx+ dir (if (fxodd?  dir) 1 -1)) 4)))
          ((#\-) (split fxeven? 1 3))
          ((#\|) (split fxodd?  2 0))
          (else  (next dir)))))
    (count (lambda (i) (char=? (array-ref acc i) #\#)) (array-indexes acc))))

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
            (product '(0) (product (list  h) (range w)))
            (product '(1) (product (range h)      '(0)))
            (product '(2) (product      '(0) (range w)))
            (product '(3) (product (range h) (list  w)))))))
    (map sub1 (array-dimensions input))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 8249)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 8444))))
