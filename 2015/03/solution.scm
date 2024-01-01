(import
  (chicken io)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (string->list (read-line)))

(define (run! table lst)
  (foldl
    (lambda (coord i)
      (hash-table-set! table coord #t)
      (map + coord
        (case i
          ((#\^) '(-1  0))
          ((#\>) '( 0  1))
          ((#\v) '( 1  0))
          ((#\<) '( 0 -1)))))
    '(0 0) lst))

(define (solve/1 input)
  (let ((acc (make-hash-table)))
    (run! acc input)
    (hash-table-size acc)))

(define (solve/2 input)
  (let ((acc (make-hash-table)))
    (let ((_ (iota (length input))))
      (run! acc (compress (map even? _) input))
      (run! acc (compress (map odd?  _) input)))
    (hash-table-size acc)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2081)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 2341))))
