(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (string->number (last (string-split (read-line) " "))))

(define (solve/1 input)
  (expt (- input 2) 2))

(define (solve/2 input)
  (let ((_ (+ (* input 100) #e1e5)))
    (count
      (lambda (i)
        (not (prime? (+ _ (* i 17)))))
      (range 1000))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
