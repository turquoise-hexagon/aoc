(import
  (chicken io)
  (chicken irregex)
  (euler-syntax))

(define (run lst offset)
  (bind (xa ya xb yb xp yp) lst
   (let* ((xp (+ xp offset))
          (yp (+ yp offset))
          (a (/ (- (* xb yp) (* xp yb)) (- (* xb ya) (* xa yb))))
          (b (/ (- (* xa yp) (* xp ya)) (- (* xa yb) (* xb ya)))))
     (if (and (integer? a)
              (integer? b))
       (+ (* 3 a) b)
       0))))

(define (import-input)
  (chop (map string->number (irregex-extract "[0-9]+" (read-string))) 6))

(define (solve input offset)
  (apply +
    (map
      (lambda (i)
        (run i offset))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= part/1 37901)))
  (let ((part/2 (solve input #e1e13)))
    (print part/2) (assert (= part/2 77407675412647))))
