(import
  (chicken io)
  (chicken irregex)
  (euler)
  (euler-syntax)
  (srfi 1))

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

(define (score l)
  (bind (w h) dimensions
    (let ((mw (quotient w 2))
          (mh (quotient h 2)))
      (* (count (lambda (i) (bind (x y) i (and (> x mw) (> y mh)))) l)
         (count (lambda (i) (bind (x y) i (and (> x mw) (< y mh)))) l)
         (count (lambda (i) (bind (x y) i (and (< x mw) (> y mh)))) l)
         (count (lambda (i) (bind (x y) i (and (< x mw) (< y mh)))) l)))))

(define (solve/1 input)
  (score (map (lambda (i) (run i 100)) input)))

(define (solve/2 input)
  (extremum (range 1 #e1e4) (lambda (n) (score (map (lambda (i) (run i n)) input))) <))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 224969976)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 7892))))
