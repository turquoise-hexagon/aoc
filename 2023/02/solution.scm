(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (parse-id str)
  (apply
    (lambda (_ id)
      (string->number id))
    (string-split str " ")))

(define (parse-data lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (str)
        (apply
          (lambda (value color)
            (hash-table-update!/default acc color (lambda (_) (max (string->number value) _)) 0))
          (string-split str " ")))
      lst)
    acc))

(define (parse str)
  (apply
    (lambda (id . data)
      (list (parse-id id) (parse-data data)))
    (string-split str ":,;")))

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

(let* ((input (time (import-input)))
       (part/1 (time (solve/1 input)))
       (part/2 (time (solve/2 input))))
  (print part/1) (assert (= part/1 2006))
  (print part/2) (assert (= part/2 84911)))
