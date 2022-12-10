(import
  (chicken io)
  (chicken string)
  (matchable)
  (euler))

(define (import-input)
  (reverse
    (foldl
      (lambda (acc str)
        (let ((_ (car acc)))
          (match (string-split str " ")
            (("addx" n)
             (cons (+ _ (string->number n)) (cons _ acc)))
            (("noop")
             (cons _ acc)))))
      '(1) (read-lines))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (_)
        (* _ (list-ref input (- _ 1))))
      (range 20 220 40))))

(define (solve/2 input)
  (for-each
    (lambda (x _)
      (if (<= (abs (- x (modulo (- _ 1) 40))) 1)
        (display "#")
        (display " "))
      (if (= (modulo _ 40) 0)
        (newline)))
    (butlast input) (range 1 (length input))))

(let ((input (import-input)))
  (print (solve/1 input))
  (solve/2 input))
