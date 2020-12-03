(import (chicken io)
        (chicken process-context))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (solve/1 input)
  (display
    (call/cc
      (lambda (return)
        (do ((i input (cdr i))) ((null? i))
          (do ((j (cdr i) (cdr j))) ((null? j))
            (let ((a (car i))
                  (b (car j)))
              (when (= 2020 (+ a b))
                (return (* a b)))))))))
  (newline))

(define (solve/2 input)
  (display
    (call/cc
      (lambda (return)
        (do ((i input (cdr i))) ((null? i))
          (do ((j (cdr i) (cdr j))) ((null? j))
            (do ((k (cdr j) (cdr k))) ((null? k))
              (let ((a (car i))
                    (b (car j))
                    (c (car k)))
                (when (= 2020 (+ a b c))
                  (return (* a b c))))))))))
  (newline))

(let ((args (command-line-arguments)))
  (let ((lst (import-input (car args))))
    (solve/1 lst)
    (solve/2 lst)))
