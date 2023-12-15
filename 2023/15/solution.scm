(import
  (chicken io)
  (chicken string)
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
  (string-split str "-="))

(define (process! table label . args)
  (hash-table-update! table (HASH label)
    (lambda (lst)
      (apply
        (case-lambda
          (()      (alist-delete label       lst string=?))
          ((value) (alist-update label value lst string=?)))
        args))))

(define (focusing-power table)
  (hash-table-fold table
    (lambda (id lst acc)
      (fold
        (lambda (pair index acc)
          (+ acc (* (+ id 1) (+ index 1) (string->number (cdr pair)))))
        acc lst (iota (length lst))))
    0))

(define (solve/1 input)
  (apply + (map HASH input)))

(define (solve/2 input)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply process! acc (parse i)))
      input)
    (focusing-power acc)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 516469)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 221627))))
