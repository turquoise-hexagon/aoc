(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define (parse-dot str)
  (map string->number (irregex-split "," str)))

(define (parse-fold str)
  (let ((lst (irregex-split "[= ]" str)))
    (receive (_ _ direction value) (apply values lst)
      (list (string->symbol direction) (string->number value)))))

(define (import-input)
  (let ((chunks (irregex-split "\n{2}" (read-string #f))))
    (let ((chunks (map (cut irregex-split "\n" <>) chunks)))
      (receive (dots folds) (apply values chunks)
        (values (map parse-dot dots) (map parse-fold folds))))))

(define (fold-dot coord axis)
  (receive (x y direction value) (apply values (flatten coord axis))
    (case direction
      ((x) (list (min x (- (* 2 value) x)) y))
      ((y) (list x (min y (- (* 2 value) y)))))))

(define (fold-dots dots folds)
  (foldl
    (lambda (acc axis)
      (map (cut fold-dot <> axis) acc))
    dots folds))

(define (place-dots dots)
  (let ((mem (make-hash-table)))
    (for-each (cut hash-table-set! mem <> #t) dots)
    mem))

(define (output mem)
  (receive (x y) (unzip2 (hash-table-keys mem))
    (let ((x-min (apply min x))
          (y-min (apply min y)))
      (let ((h (- (apply max x) x-min -1))
            (w (- (apply max y) y-min -1)))
        (for-each
          (lambda (y)
            (for-each
              (lambda (x)
                (display (if (hash-table-exists? mem (list x y)) "#" " ")))
              (iota h x-min))
            (newline))
          (iota w y-min))))))

(define (solve/1 dots folds)
  (hash-table-size (place-dots (fold-dots dots `(,(car folds))))))

(define (solve/2 dots folds)
  (output (place-dots (fold-dots dots folds))))

(receive (dots folds) (import-input)
  (print (solve/1 dots folds))
  (solve/2 dots folds))
