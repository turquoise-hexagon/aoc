(import
  (chicken io)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map string->number (read-lines)))

(define (solve/1 input)
  (apply + input))

(define (solve/2 input)
  (let ((mem (make-hash-table)))
    (let loop ((lst (apply circular-list input)) (acc 0))
      (if (hash-table-exists? mem acc)
        acc
        (begin
          (hash-table-set! mem acc #t)
          (loop (cdr lst) (+ acc (car lst))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 592)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 241))))
