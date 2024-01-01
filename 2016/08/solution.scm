(import
  (chicken io)
  (chicken irregex)
  (euler)
  (srfi 1))

(define-constant h 6)
(define-constant w 50)

(define (process input)
  (foldl
    (lambda (acc i)
      (let ((next (array-copy acc)))
        (apply
          (lambda (t a b)
            (case t
              ((rect)   (for-each (lambda (i) (array-set! next i "█")) (product (iota b) (iota a))))
              ((row)    (for-each (lambda (i) (array-set! next i (array-ref acc (map modulo (map - i (list 0 b)) (list h w))))) (product (list a) (iota w))))
              ((column) (for-each (lambda (i) (array-set! next i (array-ref acc (map modulo (map - i (list b 0)) (list h w))))) (product (iota h) (list a))))))
          i)
        next))
    (make-array (list h w) " ") input))

(define (import-input)
  (process
    (map
      (lambda (i)
        (apply
          (lambda (t a b)
            (list
              (string->symbol t)
              (string->number a)
              (string->number b)))
          (irregex-split "( |rotate|[xy]=|by|x)" i)))
      (read-lines))))

(define (solve input)
  (count
    (lambda (i)
      (string=? (array-ref input i) "█"))
    (array-indexes input)))

(let ((input (import-input)))
  (let ((part (solve input)))
    (print part) (assert (= part 110)))
  (for-each (lambda (i) (print (apply string-append i))) (array->list input)))
