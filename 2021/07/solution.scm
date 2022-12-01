(import
  (chicken io)
  (chicken string)
  (chicken sort))

(define (import-input)
  (let ((lst (string-split (read-line) ",")))
    (sort (map string->number lst) <)))

(define (sum lst)
  (foldl + 0 lst))

(define (move/1 a b)
  (abs (- a b)))

(define (move/2 a b)
  (let ((n (move/1 a b)))
    (quotient (* n (+ n 1)) 2)))

(define (target/1 lst)
  (list-ref lst (quotient (length lst) 2)))

(define (target/2 lst)
  (let ((len (length lst)) (sum (sum lst)))
    (let ((n (quotient sum len)))
      (if (> (list-ref lst (modulo sum len)) n)
        (+ n 1)
        n))))

(define (solve input target-proc move-proc)
  (let ((n (target-proc input)))
    (sum (map (cut move-proc <> n) input))))

(let ((input (import-input)))
  (print (solve input target/1 move/1))
  (print (solve input target/2 move/2)))
