(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (strip-crate lst)
  (filter char-alphabetic? lst))

(define (parse-crates lst)
  (map strip-crate
    (filter
      (lambda (_)
        (char-numeric? (last _)))
      (apply zip (map string->list lst)))))

(define (parse-procedure lst)
  (map
    (lambda (_)
      (apply
        (lambda (n a b)
          (list n (- a 1) (- b 1)))
        (map string->number (irregex-split "[a-z ]" _))))
    lst))

(define (import-input)
  (apply
    (lambda (crates procedure)
      (list (parse-crates crates) (parse-procedure procedure)))
    (map
      (lambda (_)
        (irregex-split "\n" _))
      (irregex-split "\n{2}" (read-string #f)))))

(define (solve input proc)
  (apply
    (lambda (crates procedure)
      (let ((acc (list->vector crates)))
        (for-each
          (lambda (instruction)
            (apply
              (lambda (n a b)
                (let-values (((head tail) (split-at (vector-ref acc a) n)))
                  (vector-set! acc a tail)
                  (vector-set! acc b
                    (append (proc head) (vector-ref acc b)))))
              instruction))
          procedure)
        (list->string (map car (vector->list acc)))))
    input))

(let ((input (import-input)))
  (print (solve input reverse))
  (print (solve input identity)))
