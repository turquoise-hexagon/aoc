(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i)))
    (read-lines)))

(define (safe?/1 lst)
  (and (or (apply < lst)
           (apply > lst))
       (every
         (lambda (a b)
           (<= 1 (abs (- a b)) 3))
         lst (cdr lst))))

(define (safe?/2 lst)
  (let loop ((a '()) (b lst))
    (if (null? b)
      #f
      (if (safe?/1 (append (reverse a) (cdr b)))
        #t
        (loop (cons (car b) a) (cdr b))))))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (let ((part/1 (solve input safe?/1)))
    (print part/1) (assert (= part/1 526)))
  (let ((part/2 (solve input safe?/2)))
    (print part/2) (assert (= part/2 566))))
