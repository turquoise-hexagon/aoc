(import
  (chicken io)
  (chicken string)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (read-lines))

(define (proc/1 input)
  (apply zip
    (reverse
      (map
        (lambda (i)
          (map
            (lambda (i)
              (let ((tmp (string->number i)))
                (if (not tmp)
                  (eval (string->symbol i))
                  tmp)))
            (string-split i " ")))
        input))))

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

(let ((input (import-input)))
  (print (solve input proc/1))
  (print (solve input proc/2)))
