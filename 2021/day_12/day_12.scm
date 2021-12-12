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

(define (solve graph source target flag)
  (let loop ((current source) (flag flag) (acc '()))
    (define (next current flag acc)
      (let ((acc (if (string-lower-case? current) (cons current acc) acc)))
        (apply + (map (cut loop <> flag acc) (hash-table-keys (hash-table-ref graph current))))))
    (if (string=? current target)
      1
      (if (member current acc)
        (if (or flag (string=? current source))
          0
          (next current #t acc))
        (next current flag acc)))))
        
(let ((input (import-input)))
  (print (solve input "start" "end" #t))
  (print (solve input "start" "end" #f)))
