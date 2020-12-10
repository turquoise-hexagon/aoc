(import (chicken io)
        (chicken process-context)
        (chicken sort)
        (srfi 1))

(define (import-input path)
  (let ((lst (map string->number (read-lines (open-input-file path)))))
    (cons 0 (foldr cons (list (+ (apply max lst) 3)) (sort lst <)))))

(define (tribonacci n)
  (let tribonacci/h ((a 0) (b 0) (c 1) (n n))
    (if (zero? n)
        a
        (tribonacci/h b c (+ a b c) (- n 1)))))

(define (diffs input)
  (fold
    (lambda (a b acc)
      (cons (- b a) acc))
    (list) input (cdr input)))

(define (solve/1 input)
  (let ((diffs (diffs input)))
    (print (* (length (filter (cut = <> 1) diffs))
              (length (filter (cut = <> 3) diffs))))))

(define (solve/2 input)
  (print (apply * (map
                    (lambda (n)
                      (tribonacci (+ n 2)))
                    (do ((lst (diffs input) (cdr lst))
                         (cnt 0 (if (= (car lst) 1)
                                    (+ cnt 1)
                                    0))
                         (acc (list) (if (= (car lst) 1)
                                         acc
                                         (cons cnt acc))))
                      ((null? lst) (cons cnt acc)))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
