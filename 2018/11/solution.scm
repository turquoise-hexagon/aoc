(import
  (chicken format)
  (euler)
  (euler-syntax)
  (srfi 1))

(define (function n a b)
  (- (modulo (quotient (* (+ (* (+ a 10) b) n) (+ a 10)) 100) 10) 5))

(define (import-input)
  (let ((acc (make-array '(300 300) 0)) (n (read)))
    (for-each
      (lambda (i)
        (array-set! acc i (apply function n i)))
      (array-indexes acc))
    acc))

(define-memoized (offsets n)
  (let ((_ (- n 1)))
    (append
      (product (list n) (range n))
      (product (range _) (list n)))))

(define (edges array coord n)
  (let ((lst (map
               (lambda (i)
                 (map + coord i))
               (offsets n))))
    (if (every
          (lambda (i)
            (array-exists? array i))
          lst)
      lst
      #f)))

(define (compute array coord n)
  (let ((val (array-ref array coord)))
    (let loop ((i 1) (s val) (acc/i 1) (acc/s val))
      (if (= i n)
        (list acc/i acc/s)
        (let ((lst (edges array coord i)))
          (if lst
            (let ((s (apply + s
                       (map
                         (lambda (i)
                           (array-ref array i))
                         lst))))
              (if (> s acc/s)
                (loop (+ i 1) s     i     s)
                (loop (+ i 1) s acc/i acc/s)))
            (list acc/i acc/s)))))))

(define (solve/1 input)
  (apply format "~a,~a"
    (extremum (array-indexes input)
      (lambda (i)
        (second (compute input i 3)))
      >)))

(define (solve/2 input)
  (let loop ((lst (array-indexes input)) (acc/s 0) (acc/l 0))
    (if (null? lst)
      (apply format "~a,~a,~a" acc/l)
      (apply
        (lambda (tmp/i tmp/s)
          (if (> tmp/s acc/s)
            (loop (cdr lst) tmp/s (append (car lst) (list (+ tmp/i 1))))
            (loop (cdr lst) acc/s acc/l)))
        (compute input (car lst) 20)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (string=? part/1 "20,50")))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "238,278,9"))))
