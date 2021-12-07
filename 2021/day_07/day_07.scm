(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map string->number (string-split (read-line) ",")))

(define (move/1 a b)
  (abs (- a b)))

(define (move/2 a b)
  (let ((n (move/1 a b)))
    (quotient (* n (+ n 1)) 2)))

(define (move lst n proc)
  (foldl + 0 (map (cut proc n <>) lst)))

(define (solve input proc)
  (let ((mini (foldl min 0 input))
        (maxi (foldl max 0 input)))
    (apply min (map (cut move input <> proc)
                 (iota (- maxi mini -1) mini)))))

(let ((input (import-input)))
  (print (solve input move/1))
  (print (solve input move/2)))
