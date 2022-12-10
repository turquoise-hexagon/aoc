(import
  (chicken io)
  (chicken string)
  (matchable)
  (euler))

(define (import-input)
  (reverse
    (foldl
      (lambda (acc str)
        (let ((x (car acc)))
          (match (string-split str " ")
            (("addx" n)
             (cons (+ x (string->number n)) (cons x acc)))
            (("noop")
             (cons x acc)))))
      '(1) (read-lines))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (i)
        (* i (list-ref input (- i 1))))
      (range 20 220 40))))

(define (solve/2 input)
  (for-each
    (lambda (x i)
      (if (<= (abs (- x (modulo (- i 1) 40))) 1)
        (display "▊")
        (display " "))
      (if (= (modulo i 40) 0)
        (newline)))
    (butlast input) (range 1 (length input))))

(let ((input (import-input)))
  (print (solve/1 input))
  (solve/2 input))
