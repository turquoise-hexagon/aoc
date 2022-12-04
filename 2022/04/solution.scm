(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (_)
      (map string->number (string-split _ "-,")))
    (read-lines)))

(define (valid?/1 lst)
  (apply
    (lambda (a b c d)
      (or (<= a c d b)
          (<= c a b d)))
    lst))

(define (valid?/2 lst)
  (apply
    (lambda (a b c d)
      (and (<= a d)
           (<= c b)))
    lst))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (print (solve input valid?/1))
  (print (solve input valid?/2)))
