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

(define (parse-monkey str)
  (match (string-split str ": ")
    ((name a op b)
     (list name a (parse-operator op) b))
    ((name a)
     (cons name (string->number a)))))

(define (import-input)
  (alist->hash-table (map parse-monkey (read-lines))))

(define (run table)
  (let ((acc (make-hash-table)))
    (let loop ((name "root"))
      (match (hash-table-ref table name)
        ((a op b)
         (let ((_ (op (hash-table-ref/default acc a (loop a))
                      (hash-table-ref/default acc b (loop b)))))
           (hash-table-set! acc name _) _))
        (a (hash-table-set! acc name a) a)))
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
