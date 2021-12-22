(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse-cuboid str)
  (receive (sign coordinates) (car+cdr (string-split str ".,= "))
    (list (if (string=? sign "on") 1 -1)
      (chop (filter-map string->number coordinates) 2))))

(define (import-input)
  (map parse-cuboid (read-lines)))

(define (increment! mem key step)
  (hash-table-update!/default mem key (cut + <> step) 0))

(define (cubes lst)
  (if (any (cut apply > <>) lst)
    0
    (apply * (map
               (lambda (lst)
                 (+ (apply - (reverse lst)) 1))
               lst))))

(define (intersect a b)
  (let-values (((a/1 a/2) (unzip2 a))
               ((b/1 b/2) (unzip2 b)))
    (zip (map max a/1 b/1)
         (map min a/2 b/2))))

(define (iterate mem lst)
  (receive (l/sign l) (apply values lst)
    (let ((acc (make-hash-table)))
      (hash-table-for-each mem
        (lambda (i i/sign)
          (let ((t (intersect l i)))
            (unless (= (cubes t) 0)
              (increment! acc t (- i/sign))))))
      (when (> l/sign 0)
        (increment! acc l l/sign))
      (hash-table-for-each acc
        (lambda (i i/sign)
          (increment! mem i i/sign))))))

(define (solve input)
  (let ((acc (make-hash-table)))
    (for-each (cut iterate acc <>) input)
    (hash-table-fold acc
      (lambda (i i/sign acc)
        (+ (* i/sign (cubes i)) acc))
      0)))

(let ((input (import-input)))
  (print (solve (filter
                  (lambda (lst)
                    (> (cubes (intersect (cadr lst) '((-50 50) (-50 50) (-50 50)))) 0))
                  input)))
  (print (solve input)))
