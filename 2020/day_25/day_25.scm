(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1)
        (srfi 69))

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

(define (discrete-log base res mod)
  (let ((n (inexact->exact (ceiling (sqrt mod)))))
    (let ((mem (make-hash-table)))
      (for-each
        (lambda (i)
          (hash-table-set! mem (expt-mod base i mod) i))
        (iota n))
      (let ((factor (expt-mod base (* n (- mod 2)) mod)))
        (call/cc
          (lambda (return)
            (for-each
              (lambda (i)
                (let ((tmp (modulo (* res (expt-mod factor i mod)) mod)))
                  (let ((res (hash-table-ref/default mem tmp #f)))
                    (when res (let ((ans (+ (* i n) res)))
                      (when (> ans 0) (return ans)))))))
              (iota n))
            (return -1)))))))

(define (solve/1 input)
  (let ((m 20201227))
    (match input
      ((a b) (print (expt-mod b (discrete-log 7 a m) m))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)))
