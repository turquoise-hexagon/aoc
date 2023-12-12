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
    (((#\? . a)  b) (+ (process a b) (process (cons #\# a) b)))
    ((a (i . b))
     (if (or (< (length a) i)
             (member #\. (_take a i))
             (char=? #\# (_list-ref a i)))
       0
       (process (_drop a (+ i 1)) b)))))

;; speed up caching
(define (id a b)
  (string-append (list->string a) (string-intersperse (map number->string b))))

(define process
  (let ((cache (make-hash-table #:size #e1e6)))
    (lambda (a b)
      (let ((id (id a b)))
        (if (hash-table-exists? cache id)
          (hash-table-ref cache id)
          (let ((acc (_process a b)))
            (hash-table-set! cache id acc)
            acc))))))

(define (solve input)
  (apply + (map (lambda (i) (apply process i)) input)))

(let ((input (import-input)))
  (let ((part/1 (solve (transform input 1))))
    (print part/1) (assert (= part/1 6488)))
  (let ((part/2 (solve (transform input 5))))
    (print part/2) (assert (= part/2 815364548481))))
