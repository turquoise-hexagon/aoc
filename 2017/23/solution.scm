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
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 6241)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 909))))
