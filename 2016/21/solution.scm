(import
  (chicken io)
  (chicken string)
  (euler)
  (matchable)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (let ((_ (string->number i))) (if _ _ i)))
        (string-split i " ")))
    (read-lines)))

(define (string-swap! str a b)
  (let ((m (string-ref str a))
        (n (string-ref str b)))
    (string-set! str a n)
    (string-set! str b m)))

(define (string-rotate! str n)
  (let ((l (string-length str))
        (c (string-copy   str)))
    (do ((i 0 (+ i 1))) ((= i l))
      (string-set! str i (string-ref c (modulo (+ i n) l))))))

(define (scramble str lst)
  (let ((acc (string-copy str)))
    (for-each
      (match-lambda
        (("swap" "position" a _ _ b)
         (string-swap! acc a b))
        (("swap" "letter" a _ _ b)
         (string-swap! acc
           (substring-index a acc)
           (substring-index b acc)))
        (("rotate" "left"  a _) (string-rotate! acc (+ a)))
        (("rotate" "right" a _) (string-rotate! acc (- a)))
        (("rotate" _ _ _ _ _ a)
         (let ((n (substring-index a acc)))
           (string-rotate! acc (- (+ n (if (>= n 4) 2 1))))))
        (("reverse" _ a _ b)
         (let ((lim (quotient (- b a) 2)))
           (do ((i 0 (+ i 1))) ((> i lim))
             (string-swap! acc (+ a i) (- b i)))))
        (("move" _ a _ _ b)
         (let ((l (string-length acc)) (v (string-ref acc a)))
           (do ((i a (+ i 1))) ((= (+ i 1) l)) (string-set! acc i (string-ref acc (+ i 1))))
           (do ((i (- l 1) (- i 1))) ((= i b)) (string-set! acc i (string-ref acc (- i 1))))
           (string-set! acc b v))))
      lst)
    acc))

(define (solve/1 input)
  (scramble "abcdefgh" input))

(define (solve/2 input)
  (find
    (lambda (i)
      (string=? (scramble i input) "fbgdceah"))
    (map
      (lambda (i)
        (apply string-append i))
      (permutations (string-chop "abcdefgh" 1)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (string=? part/1 "ghfacdbe")))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "fhgcdaeb"))))
