(import
  (chicken io)
  (euler)
  (srfi 1))

(define (process array a b  proc)
  (filter
    (lambda (a)
      (every
        (lambda (b)
          (char=? (array-ref array (proc a b)) #\.))
        b))
    a))

(define (empty array)
  (apply
    (lambda (h w)
      (let ((h (iota h))
            (w (iota w)))
        (list
          (process array h w (lambda (a b) (list a b)))
          (process array w h (lambda (a b) (list b a))))))
    (array-dimensions array)))

(define (galaxies array)
  (filter
    (lambda (i)
      (char=? (array-ref array i) #\#))
    (array-indexes array)))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (values (combinations (galaxies array) 2) (empty array))))

(define (distance empty expansion a b)
  (+ (apply + (map abs (map - a b)))
     (* (apply +
          (map
            (lambda (a b i)
              (let ((a (min a b))
                    (b (max a b)))
                (count
                  (lambda (i)
                    (<= a i b))
                  i)))
            a b empty))
        (- expansion 1))))

(define (solve pairs empty expansion)
  (apply +
    (map
      (lambda (i)
        (apply distance empty expansion i))
      pairs)))

(let-values (((pairs empty) (import-input)))
  (let ((part/1 (solve pairs empty 2)))
    (print part/1) (assert (= part/1 10292708)))
  (let ((part/2 (solve pairs empty 1000000)))
    (print part/2) (assert (= part/2 790194712336))))
