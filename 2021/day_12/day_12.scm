(import
  (chicken io)
  (chicken string)
  (srfi 69)
  (srfi 1))

(define (add-connection! graph a b)
  (unless (hash-table-exists? graph a)
    (hash-table-set! graph a (make-hash-table)))
  (hash-table-set! (hash-table-ref graph a) b #t))

(define (import-input)
  (let ((graph (make-hash-table)))
    (for-each
      (lambda (str)
        (receive (a b) (apply values (string-split str "-"))
          (add-connection! graph a b)
          (add-connection! graph b a)))
      (read-lines))
    graph))

(define (string-lower-case? str)
  (every char-lower-case? (string->list str)))

(define (solve graph from to flag)
  (let loop ((from from) (to to) (flag flag) (acc '()))
    (define (next from to flag acc)
      (let ((acc (if (string-lower-case? from) (cons from acc) acc)))
        (apply + (map (cut loop <> to flag acc) (hash-table-keys (hash-table-ref graph from))))))
    (if (string=? from to)
      1
      (if (member from acc)
        (if (or flag (string=? from "start"))
          0
          (next from to #t acc))
        (next from to flag acc)))))

(let ((input (import-input)))
  (print (solve input "start" "end" #t))
  (print (solve input "start" "end" #f)))
