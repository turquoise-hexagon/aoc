(import
  (chicken io)
  (chicken string)
  (euler))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i "\t")))
    (read-lines)))

(define (proc/1 lst)
  (- (apply max lst)
     (apply min lst)))

(define (proc/2 lst)
  (let loop ((lst (combinations lst 2)))
    (apply
      (lambda (a b)
        (cond
          ((zero? (modulo a b)) (quotient a b))
          ((zero? (modulo b a)) (quotient b a))
          (else (loop (cdr lst)))))
      (car lst))))

(define (solve input proc)
  (apply + (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 53978)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 314))))
