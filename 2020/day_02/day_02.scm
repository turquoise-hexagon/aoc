(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (parse-passport str)
  (let ((lst (string-split str ":- ")))
    (receive (a b char password) (apply values lst)
      (list (string->number a)
            (string->number b)
            char (string-chop password 1)))))

(define (import-input)
  (map parse-passport (read-lines)))

(define (is-valid/1? passport)
  (receive (a b char password) (apply values passport)
    (<= a (count (cut string=? char <>) password) b)))

(define (is-valid/2? passport)
  (receive (a b char password) (apply values passport)
    (not (equal? (string=? char (list-ref password (- a 1)))
                 (string=? char (list-ref password (- b 1)))))))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (print (solve input is-valid/1?))
  (print (solve input is-valid/2?)))
