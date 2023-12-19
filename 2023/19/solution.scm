(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant regex "([A-z]+)([<>])([0-9]+):([A-z]+)")

(define (parse-condition str)
  (let ((match (irregex-match regex str)))
    (if match
      (bind (ida ope val idb)
        (map
          (lambda (i)
            (irregex-match-substring match i))
          (iota 4 1))
        (list ida (eval (string->symbol ope)) (string->number val) idb))
      (list str))))

(define (parse-workflows lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (id . conditions) (string-split i "{,}")
          (hash-table-set! acc id (map parse-condition conditions))))
      lst)
    acc))

(define (parse-rating str)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (id val) i
          (let ((val (string->number val)))
            (hash-table-set! acc id (list val)))))
      (chop (string-split str "{=,}") 2))
    acc))

(define (import-input)
  (bind (workflows ratings)
    (foldr
      (lambda (i acc)
        (if (string=? i "")
          (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))
    (values (parse-workflows workflows) (map parse-rating ratings))))

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
            (let-values (((a b) (partition (cut ope <> val) (hash-table-ref rating ida))))
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
  (let ((rating (alist->hash-table (map cons '("x" "m" "a" "s") (make-list 4 (iota 4000 1))))))
    (apply +
      (map
        (lambda (i)
          (apply * (map length i)))
        (process workflows rating)))))

(let-values (((workflows ratings) (import-input)))
  (let ((part/1 (solve/1 workflows ratings)))
    (print part/1) (assert (= part/1 480738)))
  (let ((part/2 (solve/2 workflows)))
    (print part/2) (assert (= part/2 131550418841958))))
