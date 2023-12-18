(import
  (chicken io)
  (srfi 1)
  (simple-md5))

(define (import-input)
  (read-line))

(define (generate input i n)
  (substring (string->md5sum (string-append input (number->string i))) 0 n))

(define (solve input n)
  (let ((str (apply string-append (make-list n "0"))))
    (do ((i 1 (+ i 1)))
      ((string=? str (generate input i n))
       i))))

(let ((input (import-input)))
  (let ((part/1 (solve input 5)))
    (print part/1) (assert (= part/1 346386)))
  (let ((part/2 (solve input 6)))
    (print part/2) (assert (= part/2 9958218))))
