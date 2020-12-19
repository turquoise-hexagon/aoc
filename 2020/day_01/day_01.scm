(import (chicken io)
        (chicken process-context)
        (srfi 1))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (combinations l n)
  (cond ((= n 0) '(()))
        ((null? l) '())
        (else (append (map
                        (lambda (a)
                          (cons (car l) a))
                        (combinations (cdr l) (- n 1)))
                      (combinations (cdr l) n)))))

(define (solve input n)
  (print (apply * (car (filter (lambda (l) (= 2020 (apply + l))) (combinations input n))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input 2)
    (solve input 3)))
