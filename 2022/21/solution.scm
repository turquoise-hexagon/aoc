(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 69))

(define (parse-operator str)
  (case (string->symbol str)
    ((=) (lambda (a b) (signum (- a b))))
    ((+) +)
    ((-) -)
    ((*) *)
    ((/) /)))

(define (parse-monkey! table str)
  (match (string-split str ": ")
    ((name a op b)
     (hash-table-set! table name (list a (parse-operator op) b)))
    ((name a)
     (hash-table-set! table name (string->number a)))))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (_)
        (parse-monkey! acc _))
      (read-lines))
    acc))

(define (run table)
  (let ((acc (make-hash-table)))
    (hash-table-set! acc name value)
    (define (_run name)
      (match (hash-table-ref table name)
        ((a op b)
         (hash-table-set! acc name
           (op (hash-table-ref/default acc a (_run a))
               (hash-table-ref/default acc b (_run b)))))
        (a (hash-table-set! acc name a))))
    (for-each _run (hash-table-keys table))
    (hash-table-ref acc "root")))

(define (solve/1 input)
  (run input))

(define (solve/2 input)
  (apply
    (lambda (a op b)
      (hash-table-set! input "root" (list a (parse-operator "=") b)))
    (hash-table-ref input "root"))
  (let loop ((l 1) (h #e1e16))
    (let ((m (quotient (+ l h) 2)))
      (hash-table-set! input "humn" m)
      (case (run input)
        (( 1) (loop m h))
        ((-1) (loop l m))
        (( 0) m)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
