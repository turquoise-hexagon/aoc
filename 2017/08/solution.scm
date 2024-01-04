(import
  (chicken io)
  (chicken string)
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

(define (condition? mem a op v)
  (let ((_ (hash-table-ref/default mem a 0)))
    (case (string->symbol op)
      ((< )      (<  _ v))
      ((<=)      (<= _ v))
      ((> )      (>  _ v))
      ((>=)      (>= _ v))
      ((==)      (=  _ v))
      ((!=) (not (=  _ v))))))

(define (execute! mem a op v)
  (let ((_ (hash-table-ref/default mem a 0)))
    (hash-table-set! mem a
      (case (string->symbol op)
        ((inc) (+ _ v))
        ((dec) (- _ v))))))

(define (run! mem instructions)
  (foldl
    (lambda (acc instruction)
      (apply
        (lambda (a op1 v1 _ b op2 v2)
          (when
            (condition? mem b op2 v2)
            (execute!   mem a op1 v1)))
        instruction)
      (apply max acc (hash-table-values mem)))
    0 instructions))

(define (solve input)
  (let* ((mem (make-hash-table)) (acc (run! mem input)))
    (list (apply max (hash-table-values mem)) acc)))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '(5221 7491)))
