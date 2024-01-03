(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (let ((_ (string->number i)))
            (if _ _ i)))
        (string-split i " ")))
    (read-lines)))

(define (solve input)
  (let ((mem (make-hash-table #:initial '())) (acc 0))
    (do ((lst input
          (foldl
            (lambda (lst i)
              (match i
                (("value" v _ _ t n)
                 (hash-table-update! mem (list t n) (cut cons v <>))
                 lst)
                ((st sn _ _ _ tt1 tn1 _ _ _ tt2 tn2)
                 (let ((v (hash-table-ref/default mem (list st sn) '())))
                   (if (= (length v) 2)
                     (let ((a (apply min v))
                           (b (apply max v)))
                       (when (and (= a 17)
                                  (= b 61))
                         (set! acc sn))
                       (hash-table-update! mem (list tt1 tn1) (cut cons a <>))
                       (hash-table-update! mem (list tt2 tn2) (cut cons b <>))
                       (hash-table-set! mem (list st sn) '())
                       lst)
                     (cons i lst))))))
            '() lst)))
      ((null? lst)
       (list acc
         (apply *
           (append
             (hash-table-ref mem '("output" 0))
             (hash-table-ref mem '("output" 1))
             (hash-table-ref mem '("output" 2)))))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '(118 143153)))
