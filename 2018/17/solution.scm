(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (walled? array coord offset)
  (let loop ((i coord))
    (if (array-exists? array i)
      (case (array-ref array i)
        ((#\#) #t)
        ((#\.) #f)
        (else (loop (map + i offset))))
      #f)))

(define (settle! array coord offset)
  (let loop ((i coord))
    (when (array-exists? array i)
      (case (array-ref array i)
        ((#\| #\~)
         (array-set! array i #\~)
         (loop (map + i offset)))))))

(define (transform! array coord)
  (when (and (walled? array coord '(0  1))
             (walled? array coord '(0 -1)))
    (settle! array coord '(0  1))
    (settle! array coord '(0 -1))))

(define (flood! array coord)
  (set! stack (list coord))
  (let loop ()
    (unless (null? stack)
      (let*
        ((i (car stack))
         (d (map + i '(1  0)))
         (r (map + i '(0  1)))
         (l (map + i '(0 -1))))
        (if (or (char=? (array-ref array i) #\#) (not (array-exists? array d)))
          (begin
            (set! stack (cdr stack))
            (loop))
          (begin
            (when (char=? (array-ref array i) #\|)
              (set! stack (cdr stack))
              (transform! array d))
            (case (array-ref array d)
              ((#\# #\~)
               (when (char=? (array-ref array r) #\.) (set! stack (cons r stack)))
               (when (char=? (array-ref array l) #\.) (set! stack (cons l stack))))
              ((#\.)
               (set! stack (cons d stack))))
            (array-set! array i #\|)
            (loop)))))))

(define (adjust lst)
  (let ((t (list (apply min (map car lst)) 0)))
    (map
      (lambda (i)
        (map - i t))
      lst)))

(define (generate lst)
  (let* ((lst (adjust lst)) (acc (make-array (map + '(2 2) (apply map max lst)) #\.)))
    (for-each
      (lambda (i)
        (array-set! acc i #\#))
      lst)
    (flood! acc '(0 500))
    acc))

(define (import-input)
  (generate
    (join
      (map
        (lambda (i)
          (apply
            (lambda (t a _ b c)
              (cond
                ((string=? t "x") (product (range b c) (list a)))
                ((string=? t "y") (product (list a) (range b c)))))
            (map
              (lambda (i)
                (let ((t (string->number i))) (if t t i)))
              (string-split i "=, ."))))
        (read-lines)))))

(define (solve/1 input)
  (count
    (lambda (i)
      (case (array-ref input i)
        ((#\|) #t)
        ((#\~) #t)
        (else  #f)))
    (array-indexes input)))

(define (solve/2 input)
  (count
    (lambda (i)
      (case (array-ref input i)
        ((#\~) #t)
        (else  #f)))
    (array-indexes input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 31038)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 25250))))
