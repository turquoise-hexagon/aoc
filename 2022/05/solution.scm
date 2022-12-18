(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (parse-crates crates)
  (map
    (lambda (crate)
      (filter char-alphabetic? crate))
    (filter
      (lambda (lst)
        (char-numeric? (last lst)))
      (apply zip (map string->list crates)))))

(define (parse-procedure procedure)
  (map
    (lambda (instruction)
      (apply
        (lambda (n a b)
          (list n (- a 1) (- b 1)))
        (filter-map string->number (irregex-split " " instruction))))
    procedure))

(define (import-input)
  (apply
    (lambda (crates procedure)
      (list (parse-crates crates) (parse-procedure procedure)))
    (map
      (lambda (chunk)
        (irregex-split "\n" chunk))
      (irregex-split "\n{2}" (read-string #f)))))

(define (solve input proc)
  (apply
    (lambda (crates procedure)
      (let ((acc (list->vector crates)))
        (for-each
          (lambda (instruction)
            (apply
              (lambda (n a b)
                (let ((lst/a (vector-ref acc a))
                      (lst/b (vector-ref acc b)))
                  (let-values (((head tail) (split-at lst/a n)))
                    (vector-set! acc a tail)
                    (vector-set! acc b (proc cons lst/b head)))))
              instruction))
          procedure)
        (list->string (map car (vector->list acc)))))
    input))

(let ((input (import-input)))
  (print (solve input fold))
  (print (solve input fold-right)))
