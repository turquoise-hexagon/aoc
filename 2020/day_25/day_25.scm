(import (chicken io)
        (chicken process-context)
        (matchable))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (expt-mod b e m)
  (let expt-mod/h ((b b) (e e) (r 1))
    (if (= e 0)
        r
        (expt-mod/h (modulo (* b b) m) (quotient e 2)
                    (if (odd? e)
                        (modulo (* b r) m)
                        r)))))

(define (find-loop-size public)
  (let find-loop-size/h ((loop 0))
    (if (= public (expt-mod 7 loop 20201227))
        loop
        (find-loop-size/h (+ loop 1)))))

(define (solve/1 input)
  (match input ((a b) (print (expt-mod b (find-loop-size a) 20201227)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)))
