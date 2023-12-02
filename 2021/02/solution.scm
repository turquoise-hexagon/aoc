(import
  (chicken io)
  (chicken string))

(define (parse-command str)
  (apply
    (lambda (val dir)
      (delay
        (values
          (string->symbol val)
          (string->number dir))))
    (string-split str " ")))

(define (import-input)
  (map parse-command (read-lines)))

(define (solve input)
  (let loop ((lst input) (pos 0) (depth/1 0) (depth/2 0))
    (if (null? lst)
      (list
        (* pos depth/1)
        (* pos depth/2))
      (let-values (((dir val) (force (car lst))))
        (case dir
          ((up)      (loop (cdr lst) pos (- depth/1 val) depth/2))
          ((down)    (loop (cdr lst) pos (+ depth/1 val) depth/2))
          ((forward) (loop (cdr lst) (+ pos val) depth/1 (+ depth/2 (* depth/1 val)))))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (assert (equal? parts '(1714950 1281977850))))
