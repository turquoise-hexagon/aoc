(import
  (chicken io)
  (chicken string)
  (srfi 1)
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
        (for-each
          (lambda (str)
            (apply
              (lambda (value color)
                (hash-table-update!/default acc color (lambda (_) (cons (string->number value) _)) '()))
              (string-split str " ")))
          (string-split str ",")))
      lst)
    acc))

(define (parse str)
  (apply
    (lambda (id . data)
      (list (parse-id id) (parse-data data)))
    (string-split str ":;")))

(define (import-input)
  (map parse (read-lines)))

(define (test? data color maximum)
  (every
    (lambda (value)
      (<= value maximum))
    (hash-table-ref data color)))

(define (solve/1 input)
  (foldl
    (lambda (acc game)
      (apply
        (lambda (id data)
          (if (and (test? data   "red" 12)
                   (test? data "green" 13)
                   (test? data  "blue" 14))
            (+ acc id)
            acc))
        game))
    0 input))

(define (solve/2 input)
  (foldl
    (lambda (acc game)
      (apply
        (lambda (_ data)
          (+ (* (apply max (hash-table-ref data   "red"))
                (apply max (hash-table-ref data "green"))
                (apply max (hash-table-ref data  "blue")))
             acc))
        game))
    0 input))

(let* ((input (import-input))
       (part/1 (solve/1 input))
       (part/2 (solve/2 input)))
  (print part/1) (assert (= part/1 2006))
  (print part/2) (assert (= part/2 84911)))
