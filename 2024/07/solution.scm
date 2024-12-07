(import
  (chicken fixnum)
  (chicken io)
  (chicken string)
  (srfi 1))

(define (|| a b)
  (let loop ((i b) (a a))
    (if (fx= i 0)
      (fx+ a b)
      (loop (fx/ i 10) (fx* a 10)))))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ": ")))
    (read-lines)))

(define (valid? lst operators)
  (let ((n (car lst)))
    (let loop ((i (cadr lst)) (lst (cddr lst)))
      (if (fx> i n)
        #f
        (if (null? lst)
          (fx= i n)
          (let subloop ((operators operators))
            (if (null? operators)
              #f
              (if (loop ((car operators) i (car lst)) (cdr lst))
                #t
                (subloop (cdr operators))))))))))

(define (solve input operators)
  (apply +
    (map car
      (filter
        (lambda (lst)
          (valid? lst operators))
        input))))

(let ((input (import-input)))
  (let ((part/1 (solve input (list fx+ fx*))))
    (print part/1) (assert (= part/1 66343330034722)))
  (let ((part/2 (solve input (list fx+ fx* ||))))
    (print part/2) (assert (= part/2 637696070419031))))
