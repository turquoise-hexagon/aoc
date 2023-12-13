(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse-instructions str)
  (apply circular-list
    (map
      (lambda (i)
        (case i
          ((#\L) car)
          ((#\R) cdr)))
      (string->list str))))

(define (parse-network lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (current left right)
            (hash-table-set! acc current (cons left right)))
          (string-split i " =(,)")))
      lst)
    acc))

(define (import-input)
   (apply
     (lambda (instructions _ . network)
       (values (parse-instructions instructions) (parse-network network)))
     (read-lines)))

(define (run instructions network node)
  (let loop ((instructions instructions) (node node) (acc 0))
    (if (char=? (string-ref node 2) #\Z)
      acc
      (loop (cdr instructions) ((car instructions) (hash-table-ref network node)) (+ acc 1)))))

(define (solve/1 instructions network)
  (run instructions network "AAA"))

(define (solve/2 instructions network)
  (apply lcm
    (map
      (lambda (i)
        (run instructions network i))
      (filter
        (lambda (i)
          (char=? (string-ref i 2) #\A))
        (hash-table-keys network)))))

(let-values (((instructions network) (import-input)))
  (let ((part/1 (solve/1 instructions network)))
    (print part/1) (assert (= part/1 20569)))
  (let ((part/2 (solve/2 instructions network)))
    (print part/2) (assert (= part/2 21366921060721))))
