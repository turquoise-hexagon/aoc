(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define (parse-dot str)
  (map string->number (irregex-split "," str)))

(define (parse-fold str)
  (receive (_ _ dimension value) (apply values (irregex-split "[= ]" str))
    (list (string->symbol dimension) (string->number value))))

(define (import-input)
  (let ((lst (map (cut irregex-split "\n" <>) (irregex-split "\n{2}" (read-string #f)))))
    (receive (dots folds) (apply values lst)
      (values (map parse-dot dots) (map parse-fold folds)))))

(define (fold-dot coord axis)
  (receive (x y dimension value) (apply values (flatten (list coord axis)))
    (case dimension
      ((x) (list (min x (- (* 2 value) x)) y))
      ((y) (list x (min y (- (* 2 value) y)))))))

(define (fold-dots dots folds)
  (foldl
    (lambda (acc axis)
      (map (cut fold-dot <> axis) acc))
    dots folds))

(define (place-dots dots)
  (let ((grid (make-hash-table)))
    (for-each (cut hash-table-set! grid <> #t) dots)
    grid))

(define (output grid)
  (receive (x y) (unzip2 (hash-table-keys grid))
    (let ((h (+ (apply max x) 1))
          (w (+ (apply max y) 1)))
      (for-each
        (lambda (i)
          (for-each
            (lambda (j)
              (display (if (hash-table-exists? grid (list j i)) "#" " ")))
            (iota h))
          (newline))
        (iota w)))))

(define (solve/1 dots folds)
  (hash-table-size (place-dots (fold-dots dots (list (first folds))))))

(define (solve/2 dots folds)
  (output (place-dots (fold-dots dots folds))))

(receive (dots folds) (import-input)
  (print (solve/1 dots folds))
  (solve/2 dots folds))
