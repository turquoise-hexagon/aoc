(import
  (chicken io)
  (chicken irregex)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

(define-constant dimensions '(101 103))

(define (import-input)
  (map
    (lambda (i)
      (chop (map string->number (irregex-extract "-?[0-9]+" i)) 2))
    (read-lines)))

(define (run l n)
  (bind (p v) l
    (map
      (lambda (p v d)
        (modulo (+ p (* v n) d) d))
      p v dimensions)))

(define (solve/1 input)
  (bind (w h) dimensions
    (let ((mw (quotient w 2))
          (mh (quotient h 2))
          (l (map (lambda (i) (run i 100)) input)))
      (* (count (lambda (p) (bind (x y) p (and (> x mw) (> y mh)))) l)
         (count (lambda (p) (bind (x y) p (and (> x mw) (< y mh)))) l)
         (count (lambda (p) (bind (x y) p (and (< x mw) (> y mh)))) l)
         (count (lambda (p) (bind (x y) p (and (< x mw) (< y mh)))) l)))))

(define (solve/2 input)
  (let ((l (length input)))
    (let loop ((n 0))
      (let ((m (make-hash-table)))
        (for-each
          (lambda (i)
            (hash-table-set! m (run i n) #t))
          input)
        (if (not (= (hash-table-size m) l))
          (loop (+ n 1))
          n)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 224969976)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 7892))))
