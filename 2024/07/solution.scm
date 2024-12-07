(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (|| a b)
  (let loop ((i b) (a a))
    (if (= i 0)
      (+ a b)
      (loop (quotient i 10) (* a 10)))))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ": ")))
    (read-lines)))

(define (valid? lst operators)
  (let ((n (car lst)))
    (let loop ((i (cadr lst)) (lst (cddr lst)))
      (if (null? lst)
        (= i n)
        (any
          (lambda (op)
            (loop (op i (car lst)) (cdr lst)))
          operators)))))

(define (solve input operators)
  (apply +
    (map car
      (filter
        (lambda (lst)
          (valid? lst operators))
        input))))

(let ((input (import-input)))
  (let ((part/1 (solve input (list + *))))
    (print part/1) (assert (= part/1 66343330034722)))
  (let ((part/2 (solve input (list + * ||))))
    (print part/2) (assert (= part/2 637696070419031))))
