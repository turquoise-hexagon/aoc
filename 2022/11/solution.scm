(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (matchable)
  (srfi 1))

(define-record _monkey start lst operation test a b)

(define (parse-start lst)
  (filter-map string->number lst))

(define (parse-operation lst)
  (match lst
    ((_ _ _ _ operator value)
     (lambda (i)
       ((case (string->symbol operator)
          ((*) *)
          ((+) +))
        (let ((_ (string->number value)))
          (if _ _ i))
        i)))))

(define (parse-others lst)
  (car (filter-map string->number lst)))

(define (parse-monkey chunk)
  (apply
    (lambda (_ start operation test a b)
      (make-_monkey
        (parse-start start) '()
        (parse-operation operation)
        (parse-others test)
        (parse-others a)
        (parse-others b)))
    (map
      (lambda (str)
        (irregex-split "[, ]" str))
      (irregex-split "\n" chunk))))

(define (import-input)
  (map parse-monkey
    (irregex-split "\n{2}" (read-string #f))))

(define (iterate-monkey! monkey monkeys relief magic)
  (match monkey
    (($ _monkey _ lst operator test a b)
     (for-each
       (lambda (i)
         (let*
           ((i (operator i))
            (i (quotient i relief))
            (i (modulo i magic))
            (t (list-ref monkeys
                 (if (= (modulo i test) 0)
                   a
                   b))))
           (_monkey-lst-set! t
             (append (_monkey-lst t) (list i)))))
       lst)
     (_monkey-lst-set! monkey '())
     (length lst))))

(define (monkey-reset! monkey)
  (match monkey
    (($ _monkey start)
     (_monkey-lst-set! monkey start))))

(define (solve input iterations relief)
  (for-each monkey-reset! input)
  (let*
    ((magic (* relief (apply lcm (map _monkey-test input))))
     (result (foldl
               (lambda (acc _)
                 (map + acc
                   (map
                     (lambda (monkey)
                       (iterate-monkey! monkey input relief magic))
                     input)))
               (make-list (length input) 0) (iota iterations))))
    (apply * (take (sort result >) 2))))

(let* ((input (import-input)))
  (print (solve input 20 3))
  (print (solve input 10000 1)))
