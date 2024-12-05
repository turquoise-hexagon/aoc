(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler-syntax)
  (srfi 1))

(define-constant N 100)

(define (import-input)
  (bind (rules pages)

    (foldr
      (lambda (line acc)
        (if (string=? line "")
          (cons '() acc)
          (let ((item (map string->number (string-split line "|,"))))
            (cons (cons item (car acc)) (cdr acc)))))
      '(()) (read-lines))

    (let ((graph (make-vector (* N N) #f)))
      (for-each
        (lambda (rule)
          (bind (a b) rule
            (vector-set! graph (+ (* a N) b) #t)))
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
            (sort page (lambda (a b) (vector-ref graph (+ (* a N) b)))))
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
