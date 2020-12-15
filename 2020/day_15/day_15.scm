(import (chicken io)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map string->number (string-split (read-string #f (open-input-file path)) ",\n")))

(define (solve input n)
  (let ((hash (fold
                (lambda (a b acc)
                  (hash-table-set! acc a (list b))
                  acc)
                (make-hash-table) input (iota (length input) 1 1))))
    (let solve/1/h ((turn (+ (length input) 1)) (last (car (reverse input))))
      (if (> turn n) (print last)
          (let ((new (match (hash-table-ref/default hash last (list))
                       ((a) (- (- turn 1) a))
                       (_ 0))))
            (hash-table-set! hash last (list (- turn 1)))
            (solve/1/h (+ turn 1) new))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input 2020)
    (solve input 30000000)))
