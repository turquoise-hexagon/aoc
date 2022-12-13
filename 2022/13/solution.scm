(import
  (chicken io)
  (chicken irregex)
  (chicken port)
  (chicken sort)
  (chicken string)
  (srfi 1))

(define (internalize str)
  (let ((_ (string-append "'" (string-translate str "[]," "() "))))
    (eval (call-with-input-string _ read))))

(define (import-input)
  (map
    (lambda (_)
      (map internalize (irregex-split "\n" _)))
    (irregex-split "\n\n" (read-string))))

(define (order l r)
  (cond
    ((and (integer? l)
          (integer? r))
     (cond ((> l r) #f)
           ((= l r) '_)
           ((< l r) #t)))
    ((and (null? l)
          (null? r))
     2)
    ((null? l) #t)
    ((null? r) #f)
    ((and (list? l)
          (list? r))
     (let ((_ (order (car l)
                     (car r))))
       (if (boolean? _)
         _
         (order (cdr l)
                (cdr r)))))
    ((integer? l) (order (list l) r))
    ((integer? r) (order l (list r)))))

(define (solve/1 input)
  (fold
    (lambda (lst index acc)
      (if (apply order lst)
        (+ acc index)
        acc))
    0 input (iota (length input) 1)))

(define (solve/2 input)
  (let* ((separators '(((2)) ((6)))) (input (delete-duplicates (join input separators))))
    (fold
      (lambda (lst index acc)
        (if (member lst separators)
          (* acc index)
          acc))
      1 (sort input order) (iota (length input) 1))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
