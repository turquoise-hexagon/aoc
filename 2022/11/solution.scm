(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (srfi 1))

(define-record monkey start lst operation test a b)

(define (parse-start lst)
  (filter-map string->number lst))

(define (parse-operation lst)
  (apply
    (lambda (_ _ _ _ operator value)
      (let ((operator
              (case (string->symbol operator)
                ((+) +)
                ((*) *)))
            (value (string->number value)))
        (lambda (i) (operator i (if value value i)))))
    lst))

(define (parse-others lst)
  (string->number (last lst)))

(define (parse-chunk chunk)
  (apply
    (lambda (_ start operation test a b)
      (make-monkey
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
  (map parse-chunk (irregex-split "\n{2}" (read-string))))

(define (monkey-reset! monkey)
  (monkey-lst-set! monkey (monkey-start monkey)))

(define (monkey-iterate! monkey monkeys relief magic)
  (for-each
    (lambda (i)
      (let* ((i (modulo (quotient ((monkey-operation monkey) i) relief) magic))
             (target
               (list-ref monkeys
                 (if (= (modulo i (monkey-test monkey)) 0)
                   (monkey-a monkey)
                   (monkey-b monkey)))))
        (monkey-lst-set! target
          (cons i (monkey-lst target)))))
    (monkey-lst monkey))
  (let ((result (length (monkey-lst monkey))))
    (monkey-lst-set! monkey '())
    result))

(define (monkey-iterate!/all monkeys relief magic)
  (map
    (lambda (monkey)
      (monkey-iterate! monkey monkeys relief magic))
    monkeys))

(define (solve . arguments)
  (define (_solve input iterations relief)
    (for-each monkey-reset! input)
    (let ((magic (* relief (apply lcm (map monkey-test input)))))
      (foldl
        (lambda (acc _)
          (map + acc (monkey-iterate!/all input relief magic)))
        (make-list (length input) 0) (iota iterations))))
  (apply * (take (sort (apply _solve arguments) >) 2)))

(let ((input (import-input)))
  (print (solve input 20 3))
  (print (solve input 10000 1)))
