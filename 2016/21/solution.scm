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

(define (string-rotate str n)
  (let* ((len (string-length str)) (acc (make-string len)))
    (do ((i 0 (+ i 1)))
      ((= i len) acc)
      (string-set! acc i (string-ref str (modulo (+ i n) len))))))

(define (scramble str lst)
  (foldl
    (lambda (str i)
      (match i
        (("swap" "position" a _ _ b)
         (string-swap! str a b)
         str)
        (("swap" "letter" a _ _ b)
         (string-swap! str
           (substring-index a str)
           (substring-index b str))
         str)
        (("rotate" "left"  a _) (string-rotate str (+ a)))
        (("rotate" "right" a _) (string-rotate str (- a)))
        (("rotate" _ _ _ _ _ a)
         (let ((n (substring-index a str)))
           (string-rotate str (- (+ n (if (>= n 4) 2 1))))))
        (("reverse" _ a _ b)
         (let ((acc (string-copy str)))
           (do ((i 0 (+ i 1)))
             ((> (+ a i) b) acc)
             (string-set! acc (+ a i) (string-ref str (- b i))))))
        (("move" _ a _ _ b)
         (let ((len (string-length str)) (v (string-ref str a)))
           (do ((i a (+ i 1))) ((= (+ i 1) len)) (string-set! str i (string-ref str (+ i 1))))
           (do ((i (- len 1) (- i 1))) ((= i b)) (string-set! str i (string-ref str (- i 1))))
           (string-set! str b v)
           str))))
    str lst))

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
