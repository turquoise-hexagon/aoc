(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (filter-map string->number (string-split (read-line) ".,= ")))

(define (fly x-v y-v x-1 x-2 y-1 y-2)
  (call/cc
    (lambda (_)
      (do ((x 0 (+ x x-v))
           (y 0 (+ y y-v))
           (h 0 (max h y))
           (x-v x-v (cond ((> x-v 0) (- x-v 1))
                          ((< x-v 0) (+ x-v 1))
                          (else x-v)))
           (y-v y-v (- y-v 1)))
        ((and (<= x-1 x x-2)
              (<= y-1 y y-2))
         h)
        (when (or (> x x-2)
                  (< y y-1))
          (_ #f))))))

(define (solve x-1 x-2 y-1 y-2)
  (foldl
    (lambda (acc velocities)
      (receive (maxi cnt) (apply values acc)
        (let ((result (apply (cut fly <> <> x-1 x-2 y-1 y-2) velocities)))
          (if result (list (max maxi result) (+ cnt 1))
            acc))))
    '(0 0) (product (range 0 x-2) (range y-1 (- y-1)))))

(let ((input (import-input)))
  (for-each print (apply solve input)))
