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

(define (_run array coord offset)
  (let loop ((coord coord))
    (let* ((next (map + coord offset)) (value (array-ref array next)))
      (if (and (not (char=? value #\#))
               (or (not (char=? value #\[)) (and (loop (map + next '(0  1))) (loop next)))
               (or (not (char=? value #\])) (and (loop (map + next '(0 -1))) (loop next)))
               (or (not (char=? value #\O)) (loop next)))
        (let ((a (array-ref array coord))
              (b (array-ref array next)))
          (array-set! array coord b)
          (array-set! array next  a)
          #t)
        #f))))

(define (robot array)
  (find
    (lambda (coord)
      (char=? (array-ref array coord) #\@))
    (array-indexes array)))

(define (run array lst)
  (let loop ((lst lst) (coord (robot array)) (array (array-copy array)))
    (if (null? lst)
      array
      (let ((offset (car lst)) (copy (array-copy array)))
        (if (_run copy coord offset)
          (loop (cdr lst) (map + coord offset) copy)
          (loop (cdr lst) coord array))))))

(define (_translate value)
  (case value
    ((#\#) '(#\# #\#))
    ((#\.) '(#\. #\.))
    ((#\@) '(#\@ #\.))
    ((#\O) '(#\[ #\]))))

(define (translate array)
  (list->array
    (map
      (lambda (lst)
        (join (map _translate lst)))
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
  (let ((part/2 (solve (translate array) lst)))
    (print part/2) (assert (= part/2 1575877))))
