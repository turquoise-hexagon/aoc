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

(define card-values
  (alist->hash-table
    '((#\2 .  2)
      (#\3 .  3)
      (#\4 .  4)
      (#\5 .  5)
      (#\6 .  6)
      (#\7 .  7)
      (#\8 .  8)
      (#\9 .  9)
      (#\T . 10)
      (#\J . 11)
      (#\Q . 12)
      (#\K . 13)
      (#\A . 14))))

(define hand-values
  (alist->hash-table
    '(((1 1 1 1 1) . 1)
      ((2 1 1 1)   . 2)
      ((2 2 1)     . 3)
      ((3 1 1)     . 4)
      ((3 2)       . 5)
      ((4 1)       . 6)
      ((5)         . 7))))

(define (parse str)
  (bind (hand bid) (string-split str " ")
    (list (string->list hand) (string->number bid))))

(define (import-input)
  (map parse (read-lines)))

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
            (map
              (lambda (i)
                (if (= i 1) value i))
              lst))
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
      (lambda (lst index)
        (bind (_ _ bid) lst (* index bid)))
      (sort
        (map
          (lambda (lst)
            (bind (hand bid) lst
              (let ((_ (map card-value hand)))
                (list _ (proc _) bid))))
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
