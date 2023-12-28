(import
  (chicken io)
  (chicken fixnum)
  (euler)
  (srfi 1))

(define-constant offsets
  '((-1 -1) (-1 0) (-1 1)
    ( 0 -1)        ( 0 1)
    ( 1 -1) ( 1 0) ( 1 1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define-inline (map2 fun a b)
  (list
    (fun (car  a) (car  b))
    (fun (cadr a) (cadr b))))

(define (neighbors array coord)
  (filter
    (lambda (i)
      (array-exists? array i))
    (map
      (lambda (i)
        (map2 fx+ coord i))
      offsets)))

(define (iterate array)
  (let ((acc (array-copy array)))
    (for-each
      (lambda (i)
        (let ((count (count
                       (lambda (i)
                         (not (char=? (array-ref array i) #\.)))
                       (neighbors array i)))
              (value (array-ref array i)))
          (unless        (char=? value #\X)
            (if (or      (= count 3)
                    (and (= count 2)
                         (char=? value #\#)))
              (array-set! acc i #\#)
              (array-set! acc i #\.)))))
      (array-indexes array))
    acc))

(define (value array)
  (count
    (lambda (i)
      (not (char=? (array-ref array i) #\.)))
    (array-indexes array)))

(define (edit! array)
  (apply
    (lambda (h w)
      (for-each
        (lambda (i)
          (array-set! array i #\X))
        (list (list 0 0)
              (list 0 w)
              (list h 0)
              (list h w))))
    (map sub1 (array-dimensions array))))

(define (solve input)
  (do ((i 0 (+ i 1))
       (acc input (iterate acc)))
    ((= i 100) (value acc))))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 1061)))
  (edit! input)
  (let ((part/2 (solve input)))
    (print part/2) (assert (= part/2 1006))))
