(import
  (chicken io)
  (chicken string)
  (euler))

(define-constant magic 2000)

(define (generate table)
  (let loop ((lst '((0 0))) (acc '()))
    (let ((a (cadar lst)))
      (foldl
        (lambda (acc b)
          (if (not (or (member (list a b) lst)
                       (member (list b a) lst)))
            (let* ((lst (cons (list a b) lst)))
              (loop lst (cons lst acc)))
            acc))
        acc (vector-ref table a)))))

(define (import-input)
  (let ((acc (make-vector 100 '())))
    (map
      (lambda (i)
        (apply
          (lambda (a b)
            (vector-set! acc a (cons b (vector-ref acc a)))
            (vector-set! acc b (cons a (vector-ref acc b))))
          (map string->number (string-split i "/"))))
      (read-lines))
    (map
      (lambda (i)
        (cons (length i) (apply + (join i))))
      (generate acc))))

(define (solve/1 input)
  (cdr (extremum input cdr >)))

(define (solve/2 input)
  (cdr (extremum input (lambda (i) (+ (* (car i) magic) (cdr i))) >)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1940)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1928))))
