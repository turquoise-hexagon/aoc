(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define-constant e #e1e-10)

(define (parse str)
  (filter string->number (string-split str " ")))

(define (import-input)
  (list
    (parse (read-line))
    (parse (read-line))))

(define (transform/1 lst)
  (map string->number lst))

(define (transform/2 lst)
  (list (string->number (apply string-append lst))))

(define (function t d)
  (let ((_ (sqrt (- (* t t) (* 4 d)))))
    (inexact->exact
      (- (floor   (- (/ (+ t _) 2) e))
         (ceiling (+ (/ (- t _) 2) e))
         -1))))

(define (solve t d)
  (apply * (map function t d)))

(let ((input (import-input)))
  (let ((part/1 (apply solve (map transform/1 input))))
    (print part/1) (assert (= part/1 74698)))
  (let ((part/2 (apply solve (map transform/2 input))))
    (print part/2) (assert (= part/2 27563421))))
