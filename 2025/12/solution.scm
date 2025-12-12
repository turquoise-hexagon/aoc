(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (apply
    (lambda (lst . _)
      (map
        (lambda (i)
          (map string->number (string-split i "x: ")))
        lst))
    (foldl
      (lambda (acc i)
        (if (string=? i "") (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))))

(define (solve input)
  (count
    (lambda (i)
      (apply
        (lambda (a b . lst)
          (<= (* (apply + lst) 9) (* a b)))
        i))
    input))

(let ((input (import-input)))
  (print (solve input)))
