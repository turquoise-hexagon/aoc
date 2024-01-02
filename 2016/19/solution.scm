(import
  (chicken io))

(define (import-input)
  (string->number (read-line)))

(define (solve/1 input)
  (let ((mem (make-vector input)))
    (do ((i 0 (+ i 1)))
      ((= i input))
      (vector-set! mem i (cons 1 (modulo (+ i 1) input))))
    (let loop ((i 0))
      (let*
        ((_ (vector-ref mem i))
         (a (car _)) (b (cdr _))
         (_ (vector-ref mem b))
         (c (car _)) (d (cdr _)))
        (if (= a input)
          (+ i 1)
          (begin
            (vector-set! mem i (cons (+ a c) d))
            (loop d)))))))

(define (solve/2 input)
  (let loop ((i 1))
    (let ((_ (* i 3)))
      (if (> _ input)
        (- input i)
        (loop _)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1816277)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1410967))))
