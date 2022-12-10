(import
  (chicken io)
  (chicken string)
  (matchable)
  (euler))

(define (parse-instruction str)
  (match (string-split str " ")
    (("addx" n)
     (list 0 (string->number n)))
    (("noop")
     (list 0))))

(define (process-instructions lst)
  (reverse
    (foldl
      (lambda (acc _)
        (cons (+ (car acc) _) acc))
      '(1) lst)))

(define (import-input)
  (process-instructions (join (map parse-instruction (read-lines)))))

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
