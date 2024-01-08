(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (euler)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (chop (filter-map string->number (string-split i "=<, >")) 2))
    (read-lines)))

(define (iterate l)
  (apply
    (lambda (p v)
      (list (map + p v) v))
    l))

(define (size l)
  (let ((l (map first l)))
    (apply *
      (map -
        (apply map max l)
        (apply map min l)))))

(define (solve input)
  (let loop ((l input) (i 0))
    (let* ((a (size l))
           (_ (map iterate l))
           (b (size _)))
      (if (> a b)
        (loop _ (+ i 1))
        (begin
          (output l)
          i)))))

(define (output l)
  (let ((l (map first l)))
    (apply
      (lambda (a b c d)
        (for-each
          (lambda (i)
            (for-each
              (lambda (j)
                (if (member (list j i) l)
                  (display "█")
                  (display " ")))
              (range a c))
            (newline))
          (range b d)))
      (append
        (apply map min l)
        (apply map max l)))))

(let ((part (solve (import-input))))
  (print part) (assert (= part 10577)))
