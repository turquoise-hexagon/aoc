(import
  (chicken io)
  (chicken string)
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

(define (cantor2 a b)
  (let ((_ (+ a b))) (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (cantor3 a b c)
  (cantor2 (cantor2 a b) c))

(define (run array dir coord)
  (let ((acc (array-copy array)) (mem (make-hash-table)))
    (let loop ((dir dir) (coord coord))
      (when (array-exists? array coord)
        (let ((id (apply cantor3 (cons dir coord))))
          (unless (hash-table-exists? mem id)
            (hash-table-set! mem id #t)
            (array-set! acc  coord #\#)
            (for-each
              (lambda (dir)
                (loop dir (map + coord (vector-ref dirs dir))))
              (case (array-ref array coord)
                ((#\.) `(,dir))
                ((#\/) `(,(modulo (+ dir nb-dirs (if (even? dir) +1 -1)) nb-dirs)))
                ((#\\) `(,(modulo (+ dir nb-dirs (if (odd?  dir) +1 -1)) nb-dirs)))
                ((#\|) (if (odd?  dir) '(0 2) `(,dir)))
                ((#\-) (if (even? dir) '(1 3) `(,dir)))))))))
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
  (print (solve/1 input))
  (print (solve/2 input)))
