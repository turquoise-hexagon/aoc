(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (string->list (read-line)))

(define (solve input n)
  (let loop ((lst input) (acc n))
    (if (= (length (delete-duplicates (take lst n) char=?)) n)
      acc
      (loop (cdr lst) (+ acc 1)))))

(let ((input (import-input)))
  (let ((part/1 (solve input 4)))
    (print part/1) (assert (= part/1 1651)))
  (let ((part/2 (solve input 14)))
    (print part/2) (assert (= part/2 3837))))
