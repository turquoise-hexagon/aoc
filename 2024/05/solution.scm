(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (bind (rules pages)

    (foldr
      (lambda (line acc)
        (if (string=? line "")
          (cons '() acc)
          (let ((item (map string->number (string-split line "|,"))))
            (cons (cons item (car acc)) (cdr acc)))))
      '(()) (read-lines))

    (let ((graph (make-hash-table)))
      (for-each
        (lambda (rule)
          (bind (a b) rule
            (hash-table-update!/default graph a (lambda (acc) (hash-table-set! acc b #t) acc) (make-hash-table))))
        rules)

      (fold
        (lambda (unsorted sorted acc)
          (bind (invalid valid) acc
            (if (equal? unsorted sorted)
              (list invalid (cons sorted valid))
              (list (cons sorted invalid) valid))))
        '(() ()) pages

        (map
          (lambda (page)
            (sort page (lambda (a b) (hash-table-exists? (hash-table-ref graph a) b))))
          pages)))))

(define (solve input)
  (apply +
    (map
      (lambda (page)
        (list-ref page (quotient (length page) 2)))
      input)))

(bind (invalid valid) (import-input)
  (let ((part/1 (solve valid)))
    (print part/1) (assert (= part/1 6034)))
  (let ((part/2 (solve invalid)))
    (print part/2) (assert (= part/2 6305))))
