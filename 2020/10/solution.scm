(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define (process lst)
  (let ((lst (sort (cons* 0 (+ (apply max lst) 3) lst) >)))
    (map - lst (cdr lst))))

(define (import-input)
  (process (map string->number (read-lines))))

(define (tribonacci n)
  (let loop ((a 0) (b 0) (c 1) (n n))
    (if (= n 0)
      a
      (loop b c (+ a b c) (- n 1)))))

(define (solve/1 input)
  (* (count (lambda (i) (= i 1)) input)
     (count (lambda (i) (= i 3)) input)))

(define (solve/2 input)
  (let ((lst (foldl
               (lambda (acc i)
                 (if (= i 1)
                   (cons (+ (car acc) 1) (cdr acc))
                   (cons 0 acc)))
               '(0) input)))
    (apply * (map tribonacci (map (lambda (i) (+ i 2)) lst)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2450)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 32396521357312))))
