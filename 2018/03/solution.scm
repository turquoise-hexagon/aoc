(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (process lst)
  (let ((acc (make-array '(1000 1000) '())))
    (for-each
      (lambda (i)
        (apply
          (lambda (n x y h w)
            (do ((i 0 (+ i 1))) ((= i h))
              (do ((j 0 (+ j 1))) ((= j w))
                (let ((_ (list (+ x i) (+ y j))))
                  (array-set! acc _ (cons n (array-ref acc _)))))))
          i))
      lst)
    acc))

(define (import-input)
  (process
    (map
      (lambda (i)
        (filter-map string->number (string-split i "# ,:x")))
      (read-lines))))

(define (solve/1 input)
  (count
    (lambda (i)
      (> (length (array-ref input i)) 1))
    (array-indexes input)))

(define (solve/2 input)
  (let ((acc (make-vector 2000 #t)))
    (for-each
      (lambda (i)
        (when (> (length (array-ref input i)) 1)
          (for-each
            (lambda (i)
              (vector-set! acc i #f))
            (array-ref input i))))
      (array-indexes input))
    (do ((i 1 (+ i 1)))
      ((vector-ref acc i) i))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 115304)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 275))))
