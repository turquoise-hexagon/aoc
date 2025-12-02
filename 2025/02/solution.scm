(import
  (chicken io)
  (chicken irregex)
  (chicken string))

(define-constant regex/1 "^([0-9]+)\\1$")
(define-constant regex/2 "^([0-9]+)\\1+$")

(define (import-input)
  (join
    (map
      (lambda (i)
        (map
          (lambda (i)
            (map string->number (string-split i "-")))
          (string-split i ",")))
      (read-lines))))

(define (solve input regex)
  (apply +
    (map
      (lambda (i)
        (apply
          (lambda (a b)
            (do ((i a (+ i 1))
                 (s 0 (if (irregex-match regex (number->string i))
                        (+ s i)
                        s)))
              ((> i b) s)))
          i))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input regex/1)))
    (print part/1) (assert (= part/1 35367539282)))
  (let ((part/2 (solve input regex/2)))
    (print part/2) (assert (= part/2 45814076230))))
