(import
  (chicken io)
  (euler)
  (srfi 1))

(define (pairs array)
  (combinations
    (filter
      (lambda (i)
        (char=? (array-ref array i) #\#))
      (array-indexes array))
    2))

(define (_empty array a b fun)
  (filter
    (lambda (a)
      (every
        (lambda (b)
          (char=? (array-ref array (fun a b)) #\.))
        b))
    a))

(define (empty array)
  (apply
    (lambda (h w)
      (list
        (_empty array h w (lambda (a b) (list a b)))
        (_empty array w h (lambda (a b) (list b a)))))
    (map iota (array-dimensions array))))

(define (manhattan a b)
  (apply + (map abs (map - a b))))

(define (counts a b empty)
  (apply +
    (map
      (lambda (a b i)
        (let ((a (min a b))
              (b (max a b)))
          (count
            (lambda (i)
              (<= a i b))
            i)))
      a b empty)))

(define (process pairs empty)
  (map
    (lambda (i)
      (apply
        (lambda (a b)
          (cons (manhattan a b) (counts a b empty)))
        i))
    pairs))

(define (import-input)
  (let ((array (list->array (map string->list (read-lines)))))
    (process
      (pairs array)
      (empty array))))

(define (solve input expansion)
  (apply +
    (map
      (lambda (i)
        (+ (car i) (* (cdr i) (- expansion 1))))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 2)))
    (print part/1) (assert (= part/1 10292708)))
  (let ((part/2 (solve input 1000000)))
    (print part/2) (assert (= part/2 790194712336))))
