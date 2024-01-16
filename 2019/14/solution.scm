(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (parse i)
  (bind (v k) (string-split i " ")
    (list (string->number v) k)))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (a b) (string-split i "=>")
          (bind (v k) (parse b)
            (hash-table-set! acc k (list v (map parse (string-split a ",")))))))
      (read-lines))
    acc))

(define (compute table n)
  (let ((acc (make-hash-table #:initial 0)))
    (hash-table-set! acc "FUEL" n)
    (let loop ()
      (let ((k (find
                  (lambda (i)
                    (and (not (string=? i "ORE")) (> (hash-table-ref acc i) 0)))
                  (hash-table-keys acc))))
        (if k
          (bind (v l) (hash-table-ref table k)
            (let ((t (ceiling (/ (hash-table-ref acc k) v))))
              (for-each
                (lambda (i)
                  (bind (v k) i
                    (hash-table-update! acc k (lambda (_) (+ _ (* v t))))))
                l)
              (hash-table-update! acc k (lambda (_) (- _ (* v t))))
              (loop)))
          (hash-table-ref acc "ORE"))))))

(define (solve/1 input)
  (compute input 1))

(define (solve/2 input)
  (let loop ((l 0) (h #e1e12))
    (if (<= l h)
      (let* ((m (quotient (+ l h) 2)) (t (compute input m)))
        (cond
          ((< t #e1e12) (loop (+ m 1) h))
          ((> t #e1e12) (loop l (- m 1)))
          (else m)))
      (- l 1))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 337862)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 3687786))))
