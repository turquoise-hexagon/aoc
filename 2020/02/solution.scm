(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (parse-passport str)
  (apply
    (lambda (a b letter password)
      (list
        (string->number a)
        (string->number b)
        letter (string-chop password 1)))
    (string-split str ":- ")))

(define (import-input)
  (map parse-passport (read-lines)))

(define (valid?/1 lst)
  (apply
    (lambda (a b letter password)
      (let ((count
              (count
                (lambda (_)
                  (string=? _ letter))
                password)))
        (<= a count b)))
    lst))

(define (valid?/2 lst)
  (apply
    (lambda (a b letter password)
      (not
        (equal?
          (string=? letter (list-ref password (- a 1)))
          (string=? letter (list-ref password (- b 1))))))
    lst))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (print (solve input valid?/1))
  (print (solve input valid?/2)))
