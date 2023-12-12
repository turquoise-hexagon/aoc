(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 1)
  (srfi 69))

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

(define _process
  (match-lambda*
    ((() ()) 1)
    ((()  _) 0)
    (((#\# . _) ()) 0)
    (((#\. . a)  b) (process a b))
    (((#\? . a)  b)
     (+ (process (cons #\. a) b)
        (process (cons #\# a) b)))
    ((a (i . b))
     (if (and (>= (length a) i)
              (not (member #\. (_take a i)))
              (not (char=? #\# (_list-ref a i))))
       (process (_drop a (+ i 1)) b)
       0))))

;; speed up caching
(define (id a b)
  (string-append (list->string a) (string-intersperse (map number->string b))))

(define process
  (let ((cache (make-hash-table)))
    (lambda (a b)
      (if (hash-table-exists? cache (id a b))
        (hash-table-ref cache (id a b))
        (let ((acc (_process a b)))
          (hash-table-set! cache (id a b) acc)
          acc)))))

(define (solve input)
  (apply + (map (lambda (i) (apply process i)) input)))

(let ((input (import-input)))
  (let ((part/1 (solve (transform input 1))))
    (print part/1) (assert (= part/1 6488)))
  (let ((part/2 (solve (transform input 5))))
    (print part/2) (assert (= part/2 815364548481))))
