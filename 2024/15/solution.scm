(import
  (chicken io)
  (euler)
  (srfi 1))

(define (offset value)
  (case value
    ((#\^) '(-1  0))
    ((#\>) '( 0  1))
    ((#\v) '( 1  0))
    ((#\<) '( 0 -1))))

(define (translate value)
  (case value
    ((#\#) '(#\# #\#))
    ((#\.) '(#\. #\.))
    ((#\@) '(#\@ #\.))
    ((#\O) '(#\[ #\]))))

(define (import-input)
  (apply
    (lambda (array lst)
      (values (list->array array) (map offset (join lst))))
    (foldr
      (lambda (line acc)
        (if (string=? line "")
          (cons '() acc)
          (cons (cons (string->list line) (car acc)) (cdr acc))))
      '(()) (read-lines))))

(define (__run array a b)
  (let ((va (array-ref array a))
        (vb (array-ref array b)))
    (array-set! array a vb)
    (array-set! array b va)
    #t))

(define (_run array coord offset)
  (let loop ((coord coord))
    (let ((next (map + coord offset)))
      (case (array-ref array next)
        ((#\[) (if (and (loop (map + next '(0  1))) (loop next)) (__run array coord next) #f))
        ((#\]) (if (and (loop (map + next '(0 -1))) (loop next)) (__run array coord next) #f))
        ((#\O) (if (loop next) (__run array coord next) #f))
        ((#\.) (__run array coord next))
        ((#\#) #f)))))

(define (robot array)
  (find
    (lambda (coord)
      (char=? (array-ref array coord) #\@))
    (array-indexes array)))

(define (run array lst)
  (let loop ((lst lst) (coord (robot array)) (array (array-copy array)))
    (if (null? lst)
      array
      (let ((copy (array-copy array)))
        (if (_run copy coord (car lst))
          (loop (cdr lst) (map + coord (car lst)) copy)
          (loop (cdr lst) coord array))))))

(define (convert array)
  (list->array
    (map
      (lambda (lst)
        (join (map translate lst)))
      (array->list array))))

(define (solve array lst)
  (let ((array (run array lst)))
    (foldl
      (lambda (acc coord)
        (case (array-ref array coord)
          ((#\O #\[) (apply + acc (map * coord '(100 1))))
          (else acc)))
      0 (array-indexes array))))

(let-values (((array lst) (import-input)))
  (let ((part/1 (solve array lst)))
    (print part/1) (assert (= part/1 1568399)))
  (let ((part/2 (solve (convert array) lst)))
    (print part/2) (assert (= part/2 1575877))))
