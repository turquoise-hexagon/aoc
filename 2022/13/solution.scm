(import
  (chicken sort)
  (srfi 1)
  (srfi 158)
  (srfi 180))

(define (convert i)
  (if (vector? i)
    (let ((_ (vector->list i)))
      (map convert _))
    i))

(define (import-input)
  (chop (generator-map->list convert (json-lines-read)) 2))

(define (compare? l r)
  (cond
    ((and (integer? l)
          (integer? r))
     (cond ((> l r) #f)
           ((= l r) '_)
           ((< l r) #t)))
    ((integer? l) (compare? (list l) r))
    ((integer? r) (compare? l (list r)))
    ((and (null? l)
          (null? r))
     '_)
    ((null? l) #t)
    ((null? r) #f)
    (else
     (let ((_ (compare? (car l)
                        (car r))))
       (if (boolean? _)
         _
         (compare? (cdr l)
                   (cdr r)))))))

(define (solve/1 input)
  (fold
    (lambda (lst index acc)
      (if (apply compare? lst)
        (+ acc index)
        acc))
    0 input (iota (length input) 1)))

(define (solve/2 input)
  (let* ((separators '([[2]] [[6]])) (input (delete-duplicates (join input separators))))
    (fold
      (lambda (lst index acc)
        (if (member lst separators)
          (* acc index)
          acc))
      1 (sort input compare?) (iota (length input) 1))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
