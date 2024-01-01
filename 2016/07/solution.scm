(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

; multi-list filter
(define (_filter f . l)
  (let loop ((l l))
    (if (any null? l)
      '()
      (let ((_ (map car l)))
        (if (apply f _)
          (cons _ (loop (map cdr l)))
          (loop (map cdr l)))))))

(define (import-input)
  (map
    (lambda (i)
      (let* ((l (map string->list (string-split i "[]"))) (_ (iota (length l))))
        (list
          (compress (map even? _) l)
          (compress (map odd?  _) l))))
    (read-lines)))

(define (abba? l)
  (any
    (lambda (a b c d)
      (and
        (not (char=? a b))
        (char=? a d)
        (char=? b c)))
    (list-tail l 0)
    (list-tail l 1)
    (list-tail l 2)
    (list-tail l 3)))

(define (aba l)
  (_filter
    (lambda (a b c)
      (and
        (not (char=? a b))
        (char=? a c)))
    (list-tail l 0)
    (list-tail l 1)
    (list-tail l 2)))

(define (proc/1 a b)
  (and      (any abba? a)
       (not (any abba? b))))

(define (proc/2 a b)
  (any
    (lambda (i)
      (apply
        (lambda (a b _ c d _)
          (and
            (char=? a d)
            (char=? b c)))
        (join i)))
    (product
      (join (map aba b))
      (join (map aba a)))))

(define (solve input proc)
  (count (lambda (i) (apply proc i)) input))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 118)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 260))))
