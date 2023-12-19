(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (read-chunks)
  (foldr
    (lambda (i acc)
      (if (string=? i "")
        (cons '() acc)
        (cons (cons i (car acc)) (cdr acc))))
    '(()) (read-lines)))

(define (rating lst)
  (alist->hash-table (map cons '("x" "m" "a" "s") lst)))

(define (_parse-comparison str ope idb)
  (apply
    (case-lambda
      ((ida val)
       (list ida (eval (string->symbol ope)) (string->number val) idb))
      (_ #f))
    (string-split str ope)))

(define (parse-comparison str)
  (apply
    (case-lambda
      ((comparison idb)
       (cond
         ((_parse-comparison comparison ">" idb) => identity)
         ((_parse-comparison comparison "<" idb) => identity)))
      (idb idb))
    (string-split str ":")))

(define (parse-workflows lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (id . comparisons)
            (hash-table-set! acc id (map parse-comparison comparisons)))
          (string-split i "{,}")))
      lst)
    acc))

(define (parse-rating str)
  (let ((lst (map string->number (string-split str "{xmas=,}"))))
    (rating (map list lst))))

(define (import-input)
  (apply
    (lambda (workflows ratings)
      (values (parse-workflows workflows) (map parse-rating ratings)))
    (read-chunks)))

(define (next rating id val)
  (let ((acc (hash-table-copy rating)))
    (hash-table-set! acc id val)
    acc))

(define (process workflows rating #!optional (id "in"))
  (cond
    ((string=? id "R") '())
    ((string=? id "A") (list (hash-table-values rating)))
    (else
     (let loop ((lst (hash-table-ref workflows id)))
       (apply
         (case-lambda
           ((ida ope val idb)
            (let-values (((a b) (partition (lambda (i) (ope i val)) (hash-table-ref rating ida))))
              (cond
                ((null? a) (loop (cdr lst)))
                ((null? b) (process workflows rating idb))
                (else
                 (append
                   (process workflows (next rating ida a) idb)
                   (process workflows (next rating ida b) id))))))
           ((idb) (process workflows rating idb)))
         (car lst))))))

(define (solve/1 workflows ratings)
  (apply +
    (flatten
      (map
        (lambda (i)
          (process workflows i))
        ratings))))

(define (solve/2 workflows)
  (apply +
    (map
      (lambda (i)
        (apply * (map length i)))
      (process workflows (rating (make-list 4 (iota 4000 1)))))))

(let-values (((workflows ratings) (import-input)))
  (let ((part/1 (solve/1 workflows ratings)))
    (print part/1) (assert (= part/1 480738)))
  (let ((part/2 (solve/2 workflows)))
    (print part/2) (assert (= part/2 131550418841958))))
