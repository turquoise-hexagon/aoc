(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (import-input)
  (list->vector
    (map
      (lambda (i)
        (string-split i " "))
      (read-lines))))

(define-inline (convert n)
  (let ((_ (string->number n)))
    (if _ _ (hash-table-ref/default acc n 0))))

(define (solve input init)
  (let ((acc (make-hash-table)) (len (vector-length input)))
    (hash-table-set! acc "c" init)
    (let loop ((i 0))
      (if (>= i len)
        (hash-table-ref acc "a")
        (apply
          (case-lambda
            ((op a)
             (hash-table-update!/default acc a
               (case (string->symbol op)
                 ((inc) add1)
                 ((dec) sub1))
               0)
             (loop (+ i 1)))
            ((op a b)
             (case (string->symbol op)
               ((cpy)
                (hash-table-set! acc b (convert a))
                (loop (+ i 1)))
               ((jnz)
                (let ((a (convert a))
                      (b (convert b)))
                  (if (= a 0)
                    (loop (+ i 1))
                    (loop (+ i b))))))))
          (vector-ref input i))))))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= part/1 318077)))
  (let ((part/2 (solve input 1)))
    (print part/2) (assert (= part/2 9227731))))
