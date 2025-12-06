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
              ((string->number i) => identity)
              ((string->symbol i) => identity)))
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
              (let ((m (list->string m))
                    (n (list->string n)))
                (if (string=? n "")
                  (loop input (cons (string->number m) lst) acc)
                  (loop input '()
                    (cons (cons*
                            (string->symbol n)
                            (string->number m)
                            lst)
                      acc)))))))))))

(define (solve input proc)
  (apply +
    (map
      (lambda (i)
        (apply (eval (car i)) (cdr i)))
      (proc input))))

(let ((input (read-lines)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 4076006202939)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 7903168391557))))
