(import (srfi 1)
        (chicken io)
        (chicken process-context))

(define (combinations l n)
  (cond ((zero? n)
         (list (list)))
        ((null? l)
         (list))
        (else
          (append (map
                    (lambda (x)
                      (cons (car l) x))
                    (combinations (cdr l) (sub1 n)))
                  (combinations (cdr l) n)))))

(define (import-input path)
  (call-with-input-file path
    (lambda (file)
      (map string->number (read-lines file)))))

(define (main l n)
  (display
    (apply * (car (filter
                    (lambda (l)
                      (= 2020 (apply + l)))
                    (combinations l n)))))
  (newline))

(let ((args (command-line-arguments)))
  (let ((l (import-input (car args))))
    (main l 2)
    (main l 3)))
