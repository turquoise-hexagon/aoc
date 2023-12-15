(import
  (chicken io)
  (chicken string)
  (chicken irregex)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (string-split (read-line) ","))

(define (HASH str)
  (foldl
    (lambda (acc i)
      (modulo (* (+ acc (char->integer i)) 17) 256))
    0 (string->list str)))

(define (parse str)
  (let ((_ (irregex-match "([a-z]+)([=-])([0-9]+)?" str)))
    (map
      (lambda (i)
        (irregex-match-substring _ i))
      (iota (irregex-match-num-submatches _) 1))))

(define (compare? a b)
  (string=? (car a) (car b)))

(define-inline (_delete lst)
  (delete-first lst _ compare?))

(define-inline (_adjoin lst)
  (cond
    ((find
       (lambda (i)
         (compare? i _))
       lst)
     => (lambda (i) (replace lst i _ compare?)))
    (else (cons _ lst))))

(define (process! table l o v)
  (let ((_ (cons l v)))
    (case (string->symbol o)
      ((-) (hash-table-update! table (HASH l) _delete))
      ((=) (hash-table-update! table (HASH l) _adjoin)))))

(define (solve/1 input)
  (apply + (map HASH input)))

(define (solve/2 input)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply process! acc i))
      (map parse input))
    (hash-table-fold acc
      (lambda (box-id lst acc)
        (fold
          (lambda (value index acc)
            (+ acc (* (+ box-id 1) (string->number (cdr value)) (+ index 1))))
          acc (reverse lst) (iota (length lst))))
      0)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 516469)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 221627))))
