(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(include-relative "../knot-hash.scm")

(define (list-pad lst len val)
  (let ((_ (- len (length lst))))
    (let loop ((i 0))
      (if (>= i _)
        lst
        (cons val (loop (+ i 1)))))))

(define (import-input)
  (let ((input (read-line)))
    (list->array
      (map
        (lambda (i)
          (list-pad (number->list (string->number (knot-hash (string-append input "-" (number->string i))) 16) 2) 128 0))
        (iota 128)))))

(define (neighbors array coord)
  (filter
    (lambda (i)
      (and (array-exists? array i) (= (array-ref array i) 1)))
    (map
      (lambda (i)
        (map + coord i))
      offsets)))

(define (group! array coord)
  (let loop ((coord coord))
    (unless (= (array-ref array coord) 2)
      (array-set! array coord 2)
      (for-each loop (neighbors array coord)))))

(define (solve/1 input)
  (count
    (lambda (i)
      (= (array-ref input i) 1))
    (array-indexes input)))

(define (solve/2 input)
  (let loop ((lst (filter
                    (lambda (i)
                      (= (array-ref input i) 1))
                    (array-indexes input)))
             (acc 0))
    lst
    (if (null? lst)
      acc
      (begin
        (group! input (car lst))
        (loop (remove
                (lambda (i)
                  (= (array-ref input i) 2))
                lst)
          (+ acc 1))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 8304)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1018))))
