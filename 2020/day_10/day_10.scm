(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define (process-input lst)
  (let ((lst (sort (append `(,(+ (apply max lst) 3) 0) lst) >)))
    (map - lst (cdr lst))))

(define (import-input)
  (process-input (map string->number (read-lines))))

(define (tribonacci n)
  (let tribonacci/h ((a 0) (b 0) (c 1) (n n))
    (if (= n 0)
      a
      (tribonacci/h b c (+ a b c) (- n 1)))))

(define (solve/1 input)
  (* (count (cut = 1 <>) input)
     (count (cut = 3 <>) input)))

(define (solve/2 input)
  (let ((lst (foldl
               (lambda (acc n)
                 (if (= n 1)
                   (cons (+ 1 (car acc)) (cdr acc))
                   (cons 0 acc)))
               '(0) input)))
    (apply * (map tribonacci (map (cut + <> 2) lst)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
