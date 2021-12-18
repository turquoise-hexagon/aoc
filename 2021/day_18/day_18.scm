(import
  (chicken io)
  (chicken string)
  (chicken port)
  (euler))

(define (internalize str)
  ;; "[[1,2],3]" => "'[[1 2] 3]" => '((1 2) 3)
  (let ((str (string-append "'" (string-translate str "," " "))))
    (eval (call-with-input-string str read))))

(define (import-input)
  (map internalize (read-lines)))

(define (1st lst) (car  lst))
(define (2nd lst) (cadr lst))

(define (add-r arg n)
  (if (number? arg)
    (+ arg n)
    (list (1st arg) (add-r (2nd arg) n))))

(define (add-l arg n)
  (if (number? arg)
    (+ arg n)
    (list (add-l (1st arg) n) (2nd arg))))

(define (explode/h arg depth)
  (apply values
   (cond
     ;; handle base cases
     ((number? arg)
      (list #f arg 0 0))
     ((< depth 4)
      (call/cc
        (lambda (_)
          (receive (bool next l r) (explode/h (1st arg) (+ depth 1))
            (when bool (_ (list #t `(,next ,(add-l (2nd arg) r)) l 0))))
          (receive (bool next l r) (explode/h (2nd arg) (+ depth 1))
            (when bool (_ (list #t `(,(add-r (1st arg) l) ,next) 0 r))))
          (_ (list #f arg 0 0)))))
     (else (list #t 0 (1st arg) (2nd arg))))))

(define (explode arg)
  (receive (_ arg _ _) (explode/h arg 0)
    arg))

(define (split arg)
  (if (number? arg)
    (if (> arg 9)
      ;; float -> integer is annoying, do things by hand
      (let ((q (quotient arg 2)))
        (list q (+ q (if (odd? arg) 1 0))))
      arg)
    (let ((l (split (1st arg)))
          (r (split (2nd arg))))
      (if (equal? l (1st arg))
        (list (1st arg) r)
        (list l (2nd arg))))))

(define (reduce arg)
  (let loop ((arg arg))
    (let ((next (explode arg)))
      (if (equal? next arg)
        (let ((next (split arg)))
          (if (equal? next arg)
            arg
            (loop next)))
        (loop next)))))

(define (add a b)
  (reduce (list a b)))

(define (magnitude arg)
  (if (number? arg)
    arg
    (+ (* 3 (magnitude (1st arg)))
       (* 2 (magnitude (2nd arg))))))

(define (solve/1 input)
  (magnitude
    (foldl
      (lambda (acc cur)
        (add acc cur))
      (car input) (cdr input))))

(define (solve/2 input)
  (apply max
     (map
       (lambda (lst)
         (magnitude (apply add lst)))
       (combinations input 2))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
