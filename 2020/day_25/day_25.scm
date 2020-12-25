(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (transform-number-once previous subject)
  (remainder (* previous subject) 20201227))

(define (transform-number subject loop)
  (fold
    (lambda (a acc)
      (transform-number-once acc subject))
    1 (iota loop)))

(define (find-loop-size public)
  (let find-loop-size/h ((loop 1) (value (transform-number-once 1 7)))
    (if (= value public)
        loop
        (find-loop-size/h (+ loop 1) (transform-number-once value 7)))))

(define (solve/1 input)
  (match input ((a b) (print (transform-number a (find-loop-size b))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)))
