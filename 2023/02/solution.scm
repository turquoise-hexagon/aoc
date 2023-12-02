(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (parse-data lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (lst)
        (apply
          (lambda (value color)
            (hash-table-update!/default acc color (lambda (_) (max (string->number value) _)) 0))
          lst))
      (chop lst 2))
    acc))

(define (parse str)
  (apply
    (lambda (_ id . data)
      (list (string->number id) (parse-data data)))
    (string-split str " :,;")))

(define (import-input)
  (map parse (read-lines)))

(define (solve/1 input)
  (foldl
    (lambda (acc game)
      (apply
        (lambda (id data)
          (if (and (<= (hash-table-ref data   "red") 12)
                   (<= (hash-table-ref data "green") 13)
                   (<= (hash-table-ref data  "blue") 14))
            (+ acc id)
            acc))
        game))
    0 input))

(define (solve/2 input)
  (foldl
    (lambda (acc game)
      (apply
        (lambda (_ data)
          (+ (* (hash-table-ref data   "red")
                (hash-table-ref data "green")
                (hash-table-ref data  "blue"))
             acc))
        game))
    0 input))

(let* ((input (import-input))
       (part/1 (solve/1 input))
       (part/2 (solve/2 input)))
  (print part/1) (assert (= part/1 2006))
  (print part/2) (assert (= part/2 84911)))
