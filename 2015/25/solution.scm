(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (filter-map string->number (string-split (read-line) " ,.")))

(define (solve r c n b m)
  (modulo (* (modular-expt b (+ (quotient (* (+ r c -2) (+ r c -1)) 2) c -1) m) n) m))

(apply
  (lambda (r c)
    (let ((part (solve r c 20151125 252533 33554393)))
      (print part) (assert (= part 19980801))))
  (import-input))
