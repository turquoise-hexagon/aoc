(import
  (euler))

(define-constant limit #e5e7)

(define (import-input)
  (read))

(define (generate)
  (let ((acc (make-vector (+ limit 100) 0)))
    (vector-set! acc 0 3)
    (vector-set! acc 1 7)
    (let loop ((a 0) (b 1) (i 2))
      (if (> i limit)
        acc
        (let
          ((m (vector-ref acc a))
           (n (vector-ref acc b)))
          (do ((l (number->list (+ m n)) (cdr l))
               (i i (+ i 1)))
            ((null? l)
             (loop
               (modulo (+ a m 1) i)
               (modulo (+ b n 1) i)
               i))
            (vector-set! acc i (car l))))))))

(define (solve/1 vec input)
  (let loop ((i 0) (acc '()))
    (if (= i 10)
      (apply string-append (map number->string (reverse acc)))
      (loop (+ i 1) (cons (vector-ref vec (+ input i)) acc)))))

(define (solve/2 vec input)
  (let ((input (number->list input)))
    (let loop ((i 0))
      (let subloop ((t 0) (input input))
        (if (null? input)
          i
          (if (= (vector-ref vec (+ i t)) (car input))
            (subloop (+ t 1) (cdr input))
            (loop (+ i 1))))))))

(let ((input (import-input)))
  (let ((vec (generate)))
    (let ((part/1 (solve/1 vec input)))
      (print part/1) (assert (string=? part/1 "6126491027")))
    (let ((part/2 (solve/2 vec input)))
      (print part/2) (assert (= part/2 20191616)))))
