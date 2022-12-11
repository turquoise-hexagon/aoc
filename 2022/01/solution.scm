(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define (import-input)
  (sort
    (foldl
      (lambda (acc str)
        (if (string=? str "")
          (cons 0 acc)
          (let ((_ (string->number str)))
            (cons (+ (car acc) _) (cdr acc)))))
      '(1) (read-lines))
    >))

(define (solve input n)
  (apply + (take input n)))

(let ((input (import-input)))
  (print (solve input 1))
  (print (solve input 3)))
