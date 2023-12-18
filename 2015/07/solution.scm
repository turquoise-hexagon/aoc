(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (string-split i " ->"))
    (read-lines)))

(define (ready? table args)
  (every
    (lambda (i)
      (or (string->number i) (hash-table-exists? table i)))
    args))

(define (convert table args)
  (map
    (lambda (i)
      (let ((_ (string->number i))) (if _ _ (hash-table-ref table i))))
    args))

(define (process! table instruction target . args)
  (if (not (ready? table args)) #f
    (hash-table-set! table target
      (apply
        (case (string->symbol instruction)
          ((SET)    identity)
          ((NOT)    fxnot)
          ((OR)     fxxor)
          ((AND)    fxand)
          ((RSHIFT) fxshr)
          ((LSHIFT) fxshl))
        (convert table args)))))

(define (run lst)
  (let ((acc (make-hash-table)))
    (let loop ((lst lst))
      (let ((prev (length lst))
            (next (remove
                    (lambda (i)
                      (apply
                        (case-lambda
                          ((a t)      (process! acc "SET" t a))
                          ((op a t)   (process! acc    op t a))
                          ((a op b t) (process! acc    op t a b)))
                        i))
                    lst)))
        (if (= (length next) prev)
          (hash-table-ref acc "a")
          (loop next))))))

(define (edit lst value)
  (let loop ((lst lst))
    (if (string=? "b" (last (car lst)))
      (cons (list (number->string value) "b") (cdr lst))
      (cons (car lst) (loop (cdr lst))))))

(define (solve input)
  (run input))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 3176))
    (let ((part/2 (solve (edit input part/1))))
      (print part/2) (assert (= part/2 14710)))))
