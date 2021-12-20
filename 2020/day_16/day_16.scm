(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (parse-fields lst)
  (map
    (lambda (str)
      (receive (name content) (apply values (irregex-split ":" str))
        (cons name (filter-map string->number (irregex-split "[- ]" str)))))
    lst))

(define (parse-tickets lst)
  (receive (_ tickets) (car+cdr lst)
    (map (cut map string->number <>)
      (map (cut irregex-split "," <>) tickets))))

(define (import-input)
  (let ((lst (map (cut irregex-split "\n" <>) (irregex-split "\n{2}" (read-string #f)))))
    (receive (fields yours others) (apply values lst)
      (values (parse-fields  fields)
              (parse-tickets  yours)
              (parse-tickets others)))))

(define (find-matching-fields fields lst)
  (filter
    (lambda (field)
      (receive (_ a b c d) (apply values field)
        (every
           (lambda (value)
             (or (<= a value b)
                 (<= c value d)))
           lst)))
    fields))

(define (find-invalid-values fields ticket)
  (foldl
    (lambda (acc value)
      (if (null? (find-matching-fields fields (list value)))
        (cons value acc)
        acc))
    '() ticket))

(define (find-matched-field fields lst)
  (let loop ((lst lst))
    (receive (vals lst) (car+cdr lst)
      (let ((result (find-matching-fields fields vals)))
        (if (= (length result) 1)
          (values (first result) vals)
          (loop lst))))))

(define (match-fields fields tickets)
  ;; save untouched columns for indexing
  (let ((columns (apply zip tickets)))
    (let loop ((lst columns) (fields fields) (acc '()))
      (if (null? lst)
        acc
        (receive (field vals) (find-matched-field fields lst)
          (let ((value (cons (list-index (cut equal? <> vals) columns) field)))
            (loop (delete vals lst) (delete field fields) (cons value acc))))))))

(define (solve/1 fields tickets)
  (let loop ((lst tickets) (error-rate 0) (acc '()))
    (if (null? lst)
      (values error-rate acc)
      (receive (ticket lst) (car+cdr lst)
        (let ((result (find-invalid-values fields ticket)))
          (if (null? result)
            (loop lst error-rate (cons ticket acc))
            (loop lst (foldl + error-rate result) acc)))))))

(define (solve/2 fields yours others)
  (let ((yours (flatten yours)))
    (foldl
      (lambda (acc match)
        (receive (num name . _) (apply values match)
          (if (irregex-match? "^departure.*$" name)
            (* acc (list-ref yours num))
            acc)))
      1 (match-fields fields others))))

(receive (fields yours others) (import-input)
  (receive (error-rate others) (solve/1 fields others)
    (print error-rate)
    (print (solve/2 fields yours others))))
