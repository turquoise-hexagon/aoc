(import
  (chicken io)
  (chicken string)
  (euler)
  (matchable)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ", @")))
    (read-lines)))

(define (solve input)
  (count
    (match-lambda
      (((xa1 ya1 _ dxa dya _)
        (xb1 yb1 _ dxb dyb _))
       (let* ((sa (/ (- ya1 (+ ya1 dya)) (- xa1 (+ xa1 dxa))))
              (sb (/ (- yb1 (+ yb1 dyb)) (- xb1 (+ xb1 dxb))))
              (oa (- ya1 (* sa xa1)))
              (ob (- yb1 (* sb xb1))))
         (if (= sa sb) #f
           (let* ((_ (/ (- oa ob) (- sb sa)))
                  (xi _) (yi (+ (* sa _) oa)))
             (if (and (= (signum (- xi xa1)) (signum dxa))
                      (= (signum (- yi ya1)) (signum dya))
                      (= (signum (- xi xb1)) (signum dxb))
                      (= (signum (- yi yb1)) (signum dyb)))
               (and (< #e2e14 xi #e4e14)
                    (< #e2e14 yi #e4e14))
               #f))))))
    (combinations input 2)))

(let ((part (solve (import-input))))
  (print part) (assert (= part 16502)))
