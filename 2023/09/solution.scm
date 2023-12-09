(import
  (chicken io)
  (chicken string))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i " ")))
    (read-lines)))

(define (iterate lst)
  (if ((list-of? zero?) lst)
    '()
    (cons lst (iterate (map - lst (cdr lst))))))

(define (proc/1 lst)
  (proc/2 (reverse lst)))

(define (proc/2 lst)
  (apply + (map car (iterate lst))))

(define (solve input proc)
  (apply + (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 2105961943)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 1019))))
