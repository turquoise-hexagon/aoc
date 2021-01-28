(import (chicken io)
        (chicken process-context)
        (chicken sort)
        (srfi 1))

(define (diffs input)
  (fold
    (lambda (a b acc)
      (cons (- b a) acc))
    (list) input (cdr input)))

(define (import-input path)
  (let ((lst (map string->number (read-lines (open-input-file path)))))
    (diffs (cons 0 (foldr cons (list (+ (apply max lst) 3)) (sort lst <))))))

(define (tribonacci n)
  (let tribonacci/h ((a 0) (b 0) (c 1) (n n))
    (if (= n 0)
        a
        (tribonacci/h b c (+ a b c) (- n 1)))))

(define (solve/1 input)
  (print (* (count (cut = <> 1) input)
            (count (cut = <> 3) input))))

(define (solve/2 input)
  (print (fold
           (lambda (a acc)
             (* (tribonacci (+ a 2)) acc))
           1 (fold
               (lambda (a acc)
                 (if (= a 1)
                     (cons (+ 1 (car acc)) (cdr acc))
                     (cons 0 acc)))
               (list 0) input))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
