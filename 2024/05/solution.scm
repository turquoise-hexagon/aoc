(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (bind (rules pages)
    (foldr
      (lambda (line acc)
        (if (string=? line "")
          (cons '() acc)
          (let ((item (map string->number (string-split line "|,"))))
            (cons (cons item (car acc)) (cdr acc)))))
      '(()) (read-lines))
    (fold
      (lambda (unsorted sorted acc)
        (bind (invalid valid) acc
          (if (equal? unsorted sorted)
            (list invalid (cons sorted valid))
            (list (cons sorted invalid) valid))))
      '(() ()) pages
      (map
        (lambda (page)
          (sort page
            (lambda (page/a page/b)
              (any
                (lambda (rule)
                  (bind (rule/a rule/b) rule
                    (and (= page/a rule/a)
                         (= page/b rule/b))))
                rules))))
        pages))))

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
