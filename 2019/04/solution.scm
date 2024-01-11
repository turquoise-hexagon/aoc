(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (map number->list (apply range (map string->number (string-split (read-line) "-")))))

(define (proc/1 lst)
  (and
    (every <= lst (cdr lst))
    (any    = lst (cdr lst))))

(define (proc/2 lst)
  (and
    (proc/1 lst)
    (assoc 2 (run-length lst))))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 931)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 609))))
