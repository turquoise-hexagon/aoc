(import
  (chicken io)
  (chicken string)
  (euler-syntax)
  (srfi 1))

(define (proc/1 input)
  (apply zip
    (map
      (lambda (i)
        (map
          (lambda (i)
            (cond
              ((string->number i) => (lambda (i) i))
              ((string->symbol i) => eval)))
          (string-split i " ")))
      (reverse input))))

(define (proc/2 input)
  (let loop ((input (apply zip (map reverse (map string->list input)))) (lst '()) (acc '()))
    (if (null? input)
      acc
      (bind (i . input) input
        (let ((i (remove char-whitespace? i)))
          (if (null? i)
            (loop input lst acc)
            (let-values (((m n) (partition char-numeric? i)))
              (let ((m (string->number (list->string m))))
                (if (null? n)
                  (loop input (cons m lst) acc)
                  (let ((n (eval (string->symbol (list->string n)))))
                    (loop input '() (cons (cons* n m lst) acc))))))))))))

(define (solve input proc)
  (apply +
    (map
      (lambda (i)
        (bind (op . lst) i
          (apply op lst)))
      (proc input))))

(let ((input (read-lines)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 4076006202939)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 7903168391557))))
