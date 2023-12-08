(import
  (chicken io)
  (chicken irregex)
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

(define (parse-network str)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (i l r)
            (hash-table-set! acc i (cons l r)))
          (irregex-split "[ =(,)]" i)))
      (irregex-split "\n" str))
    acc))

(define (import-input)
  (apply
    (lambda (instructions network)
      (values (parse-instructions instructions) (parse-network network)))
    (irregex-split "\n\n" (read-string))))

(define (run instructions network node regex)
  (let loop ((instructions instructions) (count 0) (node node))
    (if (irregex-match? regex node)
      count
      (loop (cdr instructions) (+ count 1) ((car instructions) (hash-table-ref network node))))))

(define (solve/1 instructions network)
  (run instructions network "AAA" "ZZZ"))

(define (solve/2 instructions network)
  (apply lcm
    (map
      (lambda (i)
        (run instructions network i ".*Z"))
      (filter
        (lambda (i)
          (irregex-match? ".*A" i))
        (hash-table-keys network)))))

(let-values (((instructions network) (import-input)))
  (let ((part/1 (solve/1 instructions network)))
    (print part/1) (assert (= part/1 20569)))
  (let ((part/2 (solve/2 instructions network)))
    (print part/2) (assert (= part/2 21366921060721))))
