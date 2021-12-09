(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 113)
  (srfi 128))

(define (import-input)
  (map string->number (read-lines)))

(define (generate-preamble lst)
  (let ((acc (bag (make-default-comparator))))
    (for-each (cut bag-adjoin! acc <>)
      (map (cut apply + <>) (powerset lst 2)))
    acc))

(define (find-number lst n)
  (let ((acc (generate-preamble (take lst n))))
    (let loop ((lst lst))
      (receive (head tail) (split-at lst n)
        (if (bag-contains? acc (car tail))
          (begin
            (for-each
              (lambda (i)
                (bag-delete! acc (+ i (car head)))
                (bag-adjoin! acc (+ i (car tail))))
              (cdr head))
            (loop (cdr lst)))
          (car tail))))))

(define (pair-find pred lst)
  (if (or (null? lst) (pred lst))
    lst
    (pair-find pred (cdr lst))))

(define (solve input n)
  (let loop ((lst input))
    (let ((res (pair-find
                 (lambda (lst)
                   (= (apply + lst) n))
                 (reverse lst))))
      (if (null? res)
        (loop (cdr lst))
        (+ (apply max res)
           (apply min res))))))

(let ((input (import-input)))
  (let ((n (find-number input 25)))
    (print n)
    (print (solve input n))))
