(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (string->number (last (string-split i " "))))
    (read-lines)))

(define (generator init multiplier condition iterations)
  (let loop ((count 0) (value init))
    (if (fx= count iterations)
      '()
      (let ((next (fxmod (fx* value multiplier) 2147483647)))
        (if (fx= (fxmod next condition) 0)
          (cons next (loop (fx+ count 1) next))
          (loop count next))))))

(define (solve input conditions iterations)
  (apply count string=?
    (map
      (lambda (init multiplier condition)
        (map
          (lambda (i)
            (let* ((res (number->string i 2)) (len (string-length res)))
              (substring res (- len 16) len)))
          (generator init multiplier condition iterations)))
      input '(16807 48271) conditions)))


(let ((input (import-input)))
  (let ((part/1 (solve input '(1 1) #e4e7)))
    (print part/1) (assert (= part/1 592)))
  (let ((part/2 (solve input '(4 8) #e5e6)))
    (print part/2) (assert (= part/2 320))))
