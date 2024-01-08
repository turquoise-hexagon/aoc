(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define-constant image
  '(".#."
    "..#"
    "###"))

(define (flip lst)
  (map reverse lst))

(define (rotate lst)
  (apply zip (flip lst)))

(define (import-input)
  (map
    (lambda (i)
      (apply
        (lambda (a dest)
          (let*
            ((b (rotate a))
             (c (rotate b))
             (d (rotate c))
             (e (flip   a))
             (f (rotate e))
             (g (rotate f))
             (h (rotate g)))
            (list (list a b c d e f g h) dest)))
        (map
          (lambda (i)
            (map string->list (string-split i "/")))
          (string-split i " =>"))))
    (read-lines)))

(define (transform lst rules)
  (let loop ((rules rules))
    (apply
      (lambda (match dest)
        (if (any
              (lambda (i)
                (equal? i lst))
              match)
          dest
          (loop (cdr rules))))
      (car rules))))

(define (iterate lst rules)
  (let ((n (if (even? (length lst)) 2 3)))
    (join
      (map
        (lambda (i)
          (apply map append
            (map
              (lambda (i)
                (transform i rules))
              (apply zip
                (map
                  (lambda (i)
                    (chop i n))
                  i)))))
        (chop lst n)))))

(define (solve input n)
  (count
    (lambda (i)
      (char=? i #\#))
    (join
      (foldl
        (lambda (acc _)
          (iterate acc input))
        (map string->list image) (iota n)))))

(let ((input (import-input)))
  (let ((part/1 (solve input 5)))
    (print part/1) (assert (= part/1 136)))
  (let ((part/2 (solve input 18)))
    (print part/2) (assert (= part/2 1911767))))
