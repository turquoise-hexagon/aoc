(import
  (chicken io)
  (chicken irregex)
  (srfi 69))

(define (parse-instructions str)
  (list->vector
    (map
      (lambda (i)
        (case i
          ((#\L) car)
          ((#\R) cdr)))
      (string->list str))))

(define (parse-network str)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (current left right)
            (hash-table-set! acc current (cons left right)))
          (irregex-split "[ =(,)]" i)))
      (irregex-split "\n" str))
    acc))

(define (import-input)
  (apply
    (lambda (instructions network)
      (values (parse-instructions instructions) (parse-network network)))
    (irregex-split "\n\n" (read-string))))

(define (run instructions network from)
  (let ((len (vector-length instructions)))
    (let loop ((i 0) (node from))
      (if (char=? (string-ref node 2) #\Z)
        i
        (loop (+ i 1) ((vector-ref instructions (modulo i len)) (hash-table-ref network node)))))))

(define (solve/1 instructions network)
  (run instructions network "AAA"))

(define (solve/2 instructions network)
  (foldl
    (lambda (acc i)
      (if (char=? (string-ref i 2) #\A)
        (lcm acc (run instructions network i))
        acc))
    1 (hash-table-keys network)))

(let-values (((instructions network) (import-input)))
  (let ((part/1 (solve/1 instructions network)))
    (print part/1) (assert (= part/1 20569)))
  (let ((part/2 (solve/2 instructions network)))
    (print part/2) (assert (= part/2 21366921060721))))
