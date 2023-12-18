(import
  (chicken io)
  (srfi 1))

(define-constant MAGIC #e1e5)

(define (import-input)
  (string->list (read-line)))

(define (adjust n)
  (if (< n 0)
    (- (+ n n 1))
    (+ n n)))

(define (cantor a b)
  (let* ((a (adjust a))
         (b (adjust b))
         (_ (+ a b)))
    (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (run! vec lst)
  (foldl
    (lambda (coord i)
      (vector-set! vec (apply cantor coord) #t)
      (map + coord
        (case i
          ((#\^) '(-1  0))
          ((#\>) '( 0  1))
          ((#\v) '( 1  0))
          ((#\<) '( 0 -1)))))
    '(0 0) lst))

(define (value vec)
  (do ((i 0 (+ i 1))
       (acc 0 (if (vector-ref vec i)
                (+ acc 1)
                acc)))
    ((= i MAGIC) acc)))

(define (solve/1 input)
  (let ((acc (make-vector MAGIC #f)))
    (run! acc input)
    (value acc)))

(define (solve/2 input)
  (let ((acc (make-vector MAGIC #f)))
    (let ((_ (iota (length input))))
      (run! acc (compress (map even? _) input))
      (run! acc (compress (map odd?  _) input)))
    (value acc)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2081)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 2341))))
