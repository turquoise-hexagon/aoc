(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse-data lst)
  (let ((acc (make-hash-table #:initial 0)))
    (for-each
      (lambda (lst)
        (bind (value color) lst
          (hash-table-update! acc color (lambda (_) (max (string->number value) _)))))
      (chop lst 2))
    acc))

(define (import-input)
  (map
    (lambda (str)
      (bind (_ id . data) (string-split str " :,;")
        (list (string->number id) (parse-data data))))
    (read-lines)))

(define (solve input)
  (let loop ((input input) (acc/1 0) (acc/2 0))
    (if (null? input)
      (list acc/1 acc/2)
      (bind (id data) (car input)
        (let ((r (hash-table-ref data   "red"))
              (g (hash-table-ref data "green"))
              (b (hash-table-ref data  "blue")))
          (loop (cdr input)
            (if (and (<= r 12)
                     (<= g 13)
                     (<= b 14))
              (+ acc/1 id)
              acc/1)
            (+ acc/2 (* r g b))))))))

(let ((parts (solve (import-input))))
  (for-each print parts)
  (assert (equal? parts '(2006 84911))))
