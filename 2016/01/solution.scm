(import
  (chicken io)
  (chicken string))

(define-constant magic #e1e5)

(define-constant offsets
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define-inline (map2 fun a b)
  (list
    (fun (car  a) (car  b))
    (fun (cadr a) (cadr b))))

(define (run lst)
  (let loop ((lst lst) (coord '(0 0)) (dir 0) (acc '()))
    (if (null? lst)
      (cons coord acc)
      (apply
        (lambda (op val)
          (let* ((dir (case op
                        ((L) (modulo (+ dir 1) 4))
                        ((R) (modulo (- dir 1) 4))))
                 (offset (vector-ref offsets dir)))
            (let subloop ((i 0) (coord coord) (acc acc))
              (if (= i val)
                (loop (cdr lst) coord dir acc)
                (subloop (+ i 1) (map2 + coord offset) (cons coord acc))))))
        (car lst)))))

(define (import-input)
  (run
    (map
      (lambda (i)
        (list
          (string->symbol (substring i 0 1))
          (string->number (substring i 1))))
      (string-split (read-line) ", "))))

(define (adjust n)
  (if (< n 0)
    (- -1 (+ n n))
    (+ n n)))

(define (id a b)
  (let*
    ((a (adjust a))
     (b (adjust b))
     (_ (+ a b)))
    (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (solve/1 input)
  (apply + (car input)))

(define (solve/2 input)
  (let ((acc (make-vector magic #f)))
    (let loop ((lst (reverse input)))
      (let ((id (apply id (car lst))))
        (if (vector-ref acc id)
          (apply + (car lst))
          (begin
            (vector-set! acc id #t)
            (loop (cdr lst))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 246)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 124))))
