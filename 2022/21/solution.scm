(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 69))

(define (parse-monkey str)
  (match (string-split str ": ")
    ((n a o b)
     (list n a (eval (string->symbol o)) b))
    ((n a)
     (cons n (string->number a)))))

(define (import-input)
  (alist->hash-table (map parse-monkey (read-lines))))

(define (solve/1 input)
  (let loop ((i "root"))
    (match (hash-table-ref input i)
      ((a o b)
       (o (loop a)
          (loop b)))
      (a a))))

(define (solve/2 input)
  (match (hash-table-ref input "root")
    ((a _ b)
     (hash-table-set! input "root" (list a - b))))
  (let ((i (signum (solve/1 input))))
    (let loop ((l 0) (h #e1e16))
      (let ((m (quotient (+ l h) 2)))
        (hash-table-set! input "humn" m)
        (let ((_ (signum (solve/1 input))))
          (if (= _ 0) m
            (if (= _ i)
              (loop m h)
              (loop l m))))))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
