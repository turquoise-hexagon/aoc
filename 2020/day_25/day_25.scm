(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1)
        (srfi 69))

(define (import-input path)
  (map string->number (read-lines (open-input-file path))))

(define (expt-mod b e m)
  (let expt-mod/h ([b b] [e e] [r 1])
    (if (= e 0)
        r
        (expt-mod/h (modulo (* b b) m) (quotient e 2)
                    (if (odd? e)
                        (modulo (* b r) m)
                        r)))))

(define (discrete-log a b m)
  (let ([n (inexact->exact (ceiling (sqrt m)))] [hash (make-hash-table)])
    (for-each
      (lambda (i)
        (hash-table-set! hash (expt-mod a (* i n) m) i))
      (iota n 1))
    (let discrete-log/h ([i 0])
      (if (= i n)
          -1
          (let ([cur (modulo (* (expt-mod a i m) b) m)])
            (if (hash-table-exists? hash cur)
                (let ([ans (- (* (hash-table-ref hash cur) n) i)])
                  (if (< ans m)
                      ans
                      (discrete-log/h (+ i 1))))
                (discrete-log/h (+ i 1))))))))

(define (solve/1 input)
  (let ([m 20201227])
    (match input
      ((a b) (print (expt-mod b (discrete-log 7 a m) m))))))

(let ([path (car (command-line-arguments))])
  (let ([input (import-input path)])
    (solve/1 input)))
