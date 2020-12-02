(import (srfi 1)
        (chicken io)
        (chicken process-context))

(define (combinations lst n)
  (cond ((zero? n)
         (list (list)))
        ((null? lst)
         (list))
        (else
          (append (map
                    (lambda (x)
                      (cons (car lst) x))
                    (combinations (cdr lst) (sub1 n)))
                  (combinations (cdr lst) n)))))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (solve input n)
  (display
    (apply * (car (filter
                    (lambda (lst)
                      (= 2020 (apply + lst)))
                    (combinations input n)))))
  (newline))

(let ((args (command-line-arguments)))
  (let ((lst (import-input (car args))))
    (solve lst 2)
    (solve lst 3)))
