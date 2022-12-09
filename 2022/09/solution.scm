(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define offsets
  '((U (-1  0))
    (R ( 0  1))
    (D ( 1  0))
    (L ( 0 -1))))

(define (import-input)
  (map
    (lambda (_)
      (apply
        (lambda (a b)
          (list
            (string->symbol a)
            (string->number b)))
        (string-split _ " ")))
    (read-lines)))

(define (adjust a b)
  (if (every
        (lambda (_)
          (>= 1 _ -1))
        (map - a b)) '(0 0)
    (map
      (lambda (a b)
        (cond ((> a b)  1)
              ((= a b)  0)
              ((< a b) -1)))
      a b)))

(define (iterate! table knots direction value)
  (let ((offset (cadr (assoc direction offsets))))
    (foldl
      (lambda (knots _)
        (let ((knots (reverse
                       (foldl
                         (lambda (knots knot)
                           (cons (map + knot (adjust (car knots) knot)) knots))
                         (list (map + (car knots) offset)) (cdr knots)))))
          (hash-table-set! table (last knots) #t)
          knots))
      knots (iota value))))

(define (solve input n)
  (let ((acc (make-hash-table)))
    (foldl
      (lambda (knots instruction)
        (apply iterate! acc knots instruction))
      (make-list n '(0 0)) input)
    (hash-table-size acc)))

(let ((input (import-input)))
  (print (solve input 2))
  (print (solve input 10)))
