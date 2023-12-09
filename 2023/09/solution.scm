(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i " ")))
    (read-lines)))

(define (iterate lst proc)
  (if (every
        (lambda (i)
          (= i 0))
        lst)
    '()
    (cons lst (iterate (proc lst) proc))))

(define (proc/1 lst)
  (apply + (map last  (iterate lst (lambda (i) (map - (cdr i) i))))))

(define (proc/2 lst)
  (apply + (map first (iterate lst (lambda (i) (map - i (cdr i)))))))

(define (solve input proc)
  (apply + (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 2105961943)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 1019))))
