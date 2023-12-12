(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 1))

(include-relative "utils.scm")

(define (import-input)
  (map (lambda (i) (string-split i " ")) (read-lines)))

(define (transform lst n)
  (map
    (match-lambda
      ((a b)
       (let ((a (string-intersperse (make-list n a) "?"))
             (b (string-intersperse (make-list n b) ",")))
         (list (string->list a) (map string->number (string-split b ","))))))
    lst))

(define (_process a b loop)
  (match (list a b)
    ((() ()) 1)
    ((()  _) 0)
    (((#\# . _) ()) 0)
    (((#\. . a)  b) (loop a b))
    (((#\? . a)  b) (+ (loop a b) (loop (cons #\# a) b)))
    ((_ (i . b))
     (if (or (< (length a) i)
             (member #\. (_take a i))
             (char=? #\# (_list-ref a i #\.)))
       0
       (loop (_drop a (+ i 1)) b)))))

(define (process a b)
  (let ((cache (make-vector 10000 #f)))
    (let loop ((a a) (b b))
      (let ((id (_id a b)))
        (let ((acc (vector-ref cache id)))
          (if acc
            acc
            (let ((acc (_process a b loop)))
              (vector-set! cache id acc)
              acc)))))))

(define (solve input)
  (apply + (map (lambda (i) (apply process i)) input)))

(let ((input (import-input)))
  (let ((part/1 (solve (transform input 1))))
    (print part/1) (assert (= part/1 6488)))
  (let ((part/2 (solve (transform input 5))))
    (print part/2) (assert (= part/2 815364548481))))
