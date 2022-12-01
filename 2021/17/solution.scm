(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (fly x-v y-v x-min x-max y-min y-max)
  (call/cc
    (lambda (_)
      (do ((x 0 (+ x x-v))
           (y 0 (+ y y-v))
           (h 0 (max h y))
           (x-v x-v (- x-v (signum x-v)))
           (y-v y-v (- y-v 1)))
        ((and (<= x-min x x-max)
              (<= y-min y y-max))
         h)
        (when (or (> x x-max)
                  (< y y-min))
          (_ #f))))))

(define (import-input)
  (filter-map string->number (string-split (read-line) ".,= ")))

(define (iterate x-min x-max y-min y-max)
  (filter-map
    (lambda (velocities)
      (apply (cut fly <> <> x-min x-max y-min y-max) velocities))
    (product (range 0 x-max) (range y-min (- y-min)))))

(let ((input (import-input)))
  (let ((result (apply iterate input)))
    (print (apply max result))
    (print (length result))))
