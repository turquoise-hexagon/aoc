(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define-inline (offset direction)
  (case direction
    ((#\N) '(-1  0))
    ((#\E) '( 0  1))
    ((#\S) '( 1  0))
    ((#\W) '( 0 -1))))

(define (process lst)
  (let ((acc (make-array '(500 500) 0)))
    (let loop ((lst lst) (coord '(250 250)) (stack '()))
      (if (null? lst)
        acc
        (case (car lst)
          ((#\()
           (loop (cdr lst) coord (cons coord stack)))
          ((#\))
           (loop (cdr lst) (car stack) (cdr stack)))
          ((#\|)
           (loop (cdr lst) (car stack) stack))
          (else
           (let*
             ((next (map + coord (offset (car lst))))
              (a (array-ref acc coord))
              (b (array-ref acc  next)))
             (if (zero? b)
               (array-set! acc next (+ a 1))
               (array-set! acc next (min (+ a 1) b)))
             (loop (cdr lst) next stack))))))))

(define (import-input)
  (process
    (filter
      (lambda (i)
        (case i
          ((#\$) #f)
          ((#\^) #f)
          (else  #t)))
      (string->list (read-line)))))

(define (solve/1 input)
  (foldl
    (lambda (acc i)
      (max acc (array-ref input i)))
    0 (array-indexes input)))

(define (solve/2 input)
  (count
    (lambda (i)
      (>= (array-ref input i) 1000))
    (array-indexes input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 3966)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 8173))))
