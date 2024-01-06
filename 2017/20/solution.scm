(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant iterations 400)

(define (iterate point)
  (bind (p v a) point
    (let* ((v (map + v a))
           (p (map + p v)))
      (list p v a))))

(define (accumulate point)
  (let loop ((i 0) (point point) (acc '()))
    (if (= i iterations)
      acc
      (loop (+ i 1) (iterate point) (cons point acc)))))

(define (import-input)
  (map
    (lambda (i)
      (accumulate (chop (map string->number (string-split i "pva=<,> ")) 3)))
    (read-lines)))

(define (distance point)
  (bind (p _ _) point
    (apply + (map abs p))))

(define (adjust n)
  (if (< n 0)
    (- -1 n n)
    (+ n n)))

(define (cantor a b)
  (let* ((a (adjust a))
         (b (adjust b))
         (_ (+ a b)))
    (+ (quotient (* _ (+ _ 1)) 2) b)))

(define (id point)
  (bind (p _ _) point
    (bind (x y z) p
      (cantor (cantor x y) z))))

(define (convert lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (hash-table-set! acc (id i) #t))
      lst)
    acc))

(define (collide? a b)
  (any
    (lambda (i)
      (hash-table-exists? b i))
    (hash-table-keys a)))

(define (solve/1 input)
  (let loop ((lst input) (i 0) (res +inf.0) (acc -1))
    (if (null? lst)
      acc
      (let ((tmp (distance (caar lst))))
        (if (< tmp res)
          (loop (cdr lst) (+ i 1) tmp i)
          (loop (cdr lst) (+ i 1) res acc))))))

(define (solve/2 input)
  (let ((lst (map list (iota (length input)) (map convert input))))
    (length
      (foldl
        (lambda (acc a)
          (remove
            (lambda (b)
              (bind (na a nb b) (append a b)
                (if (= na nb) #f (collide? a b))))
            acc))
        lst lst))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 125)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 461))))
