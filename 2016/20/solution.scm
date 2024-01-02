(import
  (chicken io)
  (chicken string)
  (chicken sort))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (import-input)
  (sort
    (map
      (lambda (i)
        (map string->number (string-split i "-")))
      (read-lines))
    (lambda (a b)
      (bind (a b c d) (append a b)
        (cond
          ((< a c) #t)
          ((> a c) #f)
          (else (< b d)))))))

(define (solve/1 input)
  (let loop ((l input) (acc 0))
    (bind (a b) (car l)
      (if (> a (+ acc 1))
        (+ acc 1)
        (loop (cdr l) (max acc b))))))

(define (solve/2 input)
  (let loop ((l input) (i 0) (acc 0))
    (if (null? l) acc
      (bind (a b) (car l)
        (loop (cdr l) (max i b)
          (if (> a (+ i 1))
            (- (+ acc a) i 1)
            acc))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 17348574)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 104))))
