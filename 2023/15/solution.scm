(import
  (chicken io)
  (chicken string)
  (chicken irregex)
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

(define-inline (_adjoin lst)
  (if (assoc label lst)
    (alist-update label value lst string=?)
    (cons (cons label value) lst)))

(define-inline (_delete lst)
  (alist-delete label lst string=?))

(define (process! table label operator value)
  (let ((pair (cons label value)))
    (cond
      ((string=? operator "=") (hash-table-update! table (HASH label) _adjoin))
      ((string=? operator "-") (hash-table-update! table (HASH label) _delete)))))

(define (solve/1 input)
  (apply + (map HASH input)))

(define (solve/2 input)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (i)
        (apply process! acc (parse i)))
      input)
    (hash-table-fold acc
      (lambda (id lst acc)
        (fold
          (lambda (pair index acc)
            (+ acc (* (+ id 1) (+ index 1) (string->number (cdr pair)))))
          acc (reverse lst) (iota (length lst))))
      0)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 516469)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 221627))))
