(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (string->list (read-line)))

(define (solve input n)
  (let loop ((lst input) (acc n))
    (let ((_ (take lst n)))
      (if (= (length (delete-duplicates _ char=?)) n)
        acc
        (loop (cdr lst) (+ acc 1))))))

(let ((input (import-input)))
  (print (solve input 4))
  (print (solve input 14)))
