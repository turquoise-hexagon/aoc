(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define-constant cards "23456789TJQKA")

(define card-values
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (card value)
        (hash-table-set! acc card value))
      (string->list cards) (iota (string-length cards) 2))
    acc))

(define hand-values
  (alist->hash-table
    '(((1 1 1 1 1) . 1)
      ((2 1 1 1)   . 2)
      ((2 2 1)     . 3)
      ((3 1 1)     . 4)
      ((3 2)       . 5)
      ((4 1)       . 6)
      ((5)         . 7))))

(define (import-input)
  (map
    (lambda (i)
      (bind (hand bid) (string-split i " ")
        (list (string->list hand) (string->number bid))))
    (read-lines)))

(define (card-value n)
  (hash-table-ref card-values n))

(define (hand-value/1 lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (hash-table-update!/default acc i add1 0))
      lst)
    (hash-table-ref hand-values (sort (hash-table-values acc) >))))

(define (hand-value/2 lst)
  (if (member 1 lst)
    (apply max
      (map hand-value/1
        (map
          (lambda (value)
            (map (lambda (i) (if (= i 1) value i)) lst))
          (hash-table-values card-values))))
    (hand-value/1 lst)))

(define (strongest-first a b)
  (if (or (null? a) (null? b))
    #t
    (if (= (car a) (car b))
      (strongest-first (cdr a) (cdr b))
      (< (car a) (car b)))))

(define (solve input proc)
  (apply +
    (map
      (lambda (i index)
        (bind (_ _ bid) i (* index bid)))
      (sort
        (map
          (lambda (i)
            (bind (hand bid) i (let ((_ (map card-value hand))) (list _ (proc _) bid))))
          input)
        (lambda (a b)
          (bind (hand/a value/a _ hand/b value/b _) (append a b)
            (cond
              ((< value/a value/b) #t)
              ((> value/a value/b) #f)
              (else (strongest-first hand/a hand/b))))))
      (iota (length input) 1))))

(let ((input (import-input)))
  (let ((part/1 (solve input hand-value/1)))
    (print part/1) (assert (= part/1 253933213)))
  (hash-table-set! card-values #\J 1)
  (let ((part/2 (solve input hand-value/2)))
    (print part/2) (assert (= part/2 253473930))))
