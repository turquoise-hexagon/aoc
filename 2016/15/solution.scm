(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (apply zip
    (map
      (lambda (i)
        (filter-map string->number (string-split i " .")))
      (read-lines))))

(define (solve m r)
  (chinese-remainder-theorem
    (map
      (lambda (n i)
        (- (- n) i))
      r (range 1 (length r)))
    m))

(apply
  (lambda (m r)
    (let ((part/1 (solve m r)))
      (print part/1) (assert (= part/1 148737)))
    (let ((part/2 (solve (append m '(11))
                         (append r '( 0)))))
      (print part/2) (assert (= part/2 2353212))))
  (import-input))
