(import (chicken io)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map string->number (string-split (read-string #f (open-input-file path)) ",\n")))

(define (solve input n)
  (let ((hash (alist->hash-table (fold
                                   (lambda (a b acc)
                                     (cons (cons a b) acc))
                                   (list) input (iota (length input) 1 1)))))
    (let solve/1/h ((turn (length input)) (last (car (reverse input))))
      (if (= turn n) (print last)
          (let ((new (match (hash-table-ref/default hash last #f)
                            ((? number? a) (- turn a))
                            (_ 0))))
            (hash-table-set! hash last turn)
            (solve/1/h (+ turn 1) new))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input 2020)
    (solve input 30000000)))
