(import
  (chicken io)
  (chicken string)
  (euler))

(define (import-input)
  (reverse
    (foldl
      (lambda (acc str)
        (let* ((x (car acc)) (acc (cons x acc)))
          (let ((_ (string-split str " ")))
            (if (= (length _) 2)
              (cons (+ x (string->number (cadr _))) acc)
              acc))))
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
