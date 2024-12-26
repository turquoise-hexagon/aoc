(import
  (chicken fixnum)
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

(define (input)
  (foldr
    (lambda (i acc)
      (if (string=? i "")
        (cons '() acc)
        (cons (cons i (car acc)) (cdr acc))))
    '(()) (read-lines)))

(define (parse-start lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (id val) (string-split i ": ")
          (hash-table-set! acc id (string->number val))))
      lst)
    acc))

(define (parse-gates lst)
  (map
    (lambda (i)
      (string-split i " ->"))
    lst))

(define (import-input)
  (bind (start gates) (input)
    (values
      (parse-start start)
      (parse-gates gates))))

(define (run start gates)
  (let ((mem start))
    (let loop ((lst gates))
      (if (null? lst)
        mem
        (loop
          (let subloop ((lst lst))
            (if (null? lst)
              '()
              (bind (head . tail) lst
                (bind (a o b c) head
                  (if (and (hash-table-exists? mem a)
                           (hash-table-exists? mem b))
                    (begin
                      (hash-table-set! mem c
                        ((case (string->symbol o)
                           ((AND) fxand)
                           ((XOR) fxxor)
                           ( (OR) fxior))
                         (hash-table-ref mem a)
                         (hash-table-ref mem b)))
                      (subloop tail))
                    (cons head (subloop tail))))))))))))

(define (solve/1 start gates)
  (let ((acc (run start gates)))
    (list->number
      (map
        (lambda (i)
          (hash-table-ref acc i))
        (sort
          (filter
            (lambda (i)
              (substring=? i "z"))
            (hash-table-keys acc))
          string>?))
      2)))

(define (solve/2 gates)
  (string-intersperse
    (sort
      (map last
        (filter
          (lambda (i)
            (bind (a o b c) i
              (or (and (string=? o "XOR")
                       (every (lambda (i) (member (substring i 0 1) '("a" "b" "c"))) (list a b c)))
                  (and (string=? o "AND")
                       (not (member "x00" (list a b)))
                       (any (lambda (i) (bind (a o b _) i (and (string=? o "XOR") (member c (list a b))))) gates))
                  (and (string=? o "XOR")
                       (not (member "x00" (list a b)))
                       (any (lambda (i) (bind (a o b _) i (and (string=? o  "OR") (member c (list a b))))) gates))
                  (and (not (string=? o "XOR"))
                       (not (string=? c "z45"))
                       (substring=? c "z")))))
          gates))
      string<?)
    ","))

(let-values (((start gates) (import-input)))
  (let ((part/1 (solve/1 start gates)))
    (print part/1) (assert (= part/1 57588078076750)))
  (let ((part/2 (solve/2 gates)))
    (print part/2) (assert (string=? part/2 "kcd,shj,tpk,wkb,z07,z23,z27"))))
