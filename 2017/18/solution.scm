(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define % modulo)

(define (import-input)
  (list->vector
    (map
      (lambda (i)
        (map
          (lambda (i)
            (if (string->number i)
              (string->number i)
              (string->symbol i)))
          (string-split i " ")))
      (read-lines))))

(define-inline (run!-get n)
  (if (number? n) n (hash-table-ref/default mem n 0)))

(define (run! mem index rcv insts)
  (let ((limit (vector-length insts)))
    (let loop ((index index) (snd '()) (rcv rcv))
      (if (< -1 index limit)
        (apply
          (case-lambda
            ((op a)
             (case op
               ((snd)
                (loop (+ index 1) (cons (run!-get a) snd) rcv))
               ((rcv)
                (if rcv
                  (if (null? rcv)
                    (values index snd)
                    (begin
                      (hash-table-set! mem a (last rcv))
                      (loop (+ index 1) snd (butlast rcv))))
                  (if (= (run!-get a) 0)
                    (loop (+ index 1) snd rcv)
                    (car snd))))))
            ((op a b)
             (case op
               ((set)
                (hash-table-set! mem a (run!-get b))
                (loop (+ index 1) snd rcv))
               ((add) (hash-table-update!/default mem a (lambda (_) (+ _ (run!-get b))) 0) (loop (+ index 1) snd rcv))
               ((mul) (hash-table-update!/default mem a (lambda (_) (* _ (run!-get b))) 0) (loop (+ index 1) snd rcv))
               ((mod) (hash-table-update!/default mem a (lambda (_) (% _ (run!-get b))) 0) (loop (+ index 1) snd rcv))
               ((jgz)
                (let ((a (run!-get a))
                      (b (run!-get b)))
                  (if (> a 0)
                    (loop (+ index b) snd rcv)
                    (loop (+ index 1) snd rcv)))))))
          (vector-ref insts index))
        (values index snd)))))

(define (solve/1 input)
  (let ((acc (make-hash-table)))
    (run! acc 0 #f input)))

(define (solve/2 input)
  (let ((acc/0 (make-hash-table))
        (acc/1 (make-hash-table)))
    (hash-table-set! acc/0 'p 0)
    (hash-table-set! acc/1 'p 1)
    (let loop ((index/0 0) (index/1 0) (rcv/0 '()) (rcv/1 '()) (sum 0))
      (let*-values
        (((index/0 rcv/0) (run! acc/0 index/0 rcv/0 input))
         ((index/1 rcv/1) (run! acc/1 index/1 rcv/1 input)))
        (if (and (null? rcv/0)
                 (null? rcv/1))
          sum
          (loop index/0 index/1 rcv/1 rcv/0 (+ sum (length rcv/1))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 9423)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 7620))))
