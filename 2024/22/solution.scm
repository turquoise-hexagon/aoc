(import
  (chicken fixnum)
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-inline (process n)
  (let* ((n (fxmod (fxxor n (fx* n   64)) 16777216))
         (n (fxmod (fxxor n (fx/ n   32)) 16777216))
         (n (fxmod (fxxor n (fx* n 2048)) 16777216)))
    n))

(define (generate n)
  (let loop ((cnt 0) (n n))
    (if (fx> cnt 2000)
      '()
      (cons n (loop (fx+ cnt 1) (process n))))))

(define (import-input)
  (map
    (lambda (i)
      (generate (string->number i)))
    (read-lines)))

(define (solve/1 input)
  (foldl
    (lambda (acc i)
      (fx+ acc (last i)))
    0 input))

(define (id lst)
  (string-intersperse (map number->string lst) ","))

(define (solve/2 input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (lst)
        (let ((dif (map
                     (lambda (a b)
                       (fx- (fxmod b 10) (fxmod a 10)))
                     lst (cdr lst)))
              (mem (make-hash-table)))
          (for-each
            (lambda (n . tmp)
              (let ((id (id tmp)))
                (unless (hash-table-exists? mem id)
                  (hash-table-set! mem id 0)
                  (hash-table-set! acc id (fx+ (hash-table-ref/default acc id 0) (fxmod n 10))))))
            (list-tail lst 4)
            (list-tail dif 0)
            (list-tail dif 1)
            (list-tail dif 2)
            (list-tail dif 3))))
      input)
    (foldl fxmax 0 (hash-table-values acc))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 17960270302)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 2042))))
