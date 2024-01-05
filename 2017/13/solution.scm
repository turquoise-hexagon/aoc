(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ": ")))
    (read-lines)))

(define (position a b)
  (let ((v (modulo b (* (- a 1) 2))))
    (if (>= v a)
      (- (* (- a 1) 2) v)
      v)))

(define (solve/1 input)
  (foldl
    (lambda (acc l)
      (bind (a b) l
        (if (= (position b a) 0)
          (+ acc (* a b))
          acc)))
    0 input))

(define (solve/2 input)
  (let loop ((i 0))
    (if (any
          (lambda (l)
            (bind (a b) l
              (= (position b (+ a i)) 0)))
          input)
      (loop (+ i 1))
      i)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2384)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 3921270))))
