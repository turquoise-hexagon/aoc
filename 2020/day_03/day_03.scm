(import
  (chicken io))

(define (import-input)
  (map string->list (read-lines)))

(define (encountered-trees input y-o x-o)
  (let ((h (length input)) (w (length (car input))))
    (do ((x 0 (+ x x-o))
         (y 0 (+ y y-o))
         (t 0 (+ t (if (char=? (list-ref (list-ref input x) (modulo y w)) #\#)
                     1
                     0))))
      ((>= x h) t))))

(define (solve input offsets)
  (apply * (map (cut apply encountered-trees input <>) offsets)))

(let ((input (import-input)))
  (print (solve input '((3 1))))
  (print (solve input '((1 1) (3 1) (5 1) (7 1) (1 2)))))
