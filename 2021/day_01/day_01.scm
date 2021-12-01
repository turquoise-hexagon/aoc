(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input)
  (count (cut apply < <>) (zip input (cdr input))))

(let ((input (import-input)))
  (print (solve input))
  (let ((input (map (cut apply + <>) (zip input (cdr input) (cddr input)))))
    (print (solve input))))
