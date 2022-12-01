(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (srfi 1))

(define (import-input)
  (sort
    (map
      (lambda (_)
        (apply + (map string->number (irregex-split "\n" _))))
      (irregex-split "\n{2}" (read-string #f)))
    >))

(define (solve input n)
  (apply + (take input n)))

(let ((input (import-input)))
  (print (solve input 1))
  (print (solve input 3)))
