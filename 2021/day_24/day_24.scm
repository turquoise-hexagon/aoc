(import
  (chicken io)
  (chicken irregex)
  (euler)
  (matchable)
  (srfi 1))

(define (parse-instruction str)
  (let ((lst (irregex-split " " str)))
    (match lst
      ((op a b)
       (list op a
         (let ((t (string->number b)))
           (if t t b))))
      (_ lst))))

(define (import-input)
  (map (cut map parse-instruction <>)
    (map (cut irregex-split "\n" <>)
      (irregex-split "inp w" (read-string #f)))))

(define (solve/h mini maxi i j d)
  (vector-set! maxi i 9)
  (vector-set! maxi j (- 9 d))
  (vector-set! mini i (+ 1 d))
  (vector-set! mini j 1))

(define (solve input)
  (let ((mini (make-vector 14))
        (maxi (make-vector 14)))
    (fold
      (lambda (lst i acc)
        (let ((a (list-ref lst 3))
              (b (list-ref lst 4))
              (c (list-ref lst 14)))
          (if (equal? a '("div" "z" 1))
            (cons `(,i ,(last c)) acc)
            (match acc
              (((j t) . acc)
               (let ((t (+ t (last b))))
                 (if (> t 0)
                   (solve/h mini maxi i j t)
                   (solve/h mini maxi j i (- t))))
               acc)))))
      '() input (iota (length input)))
    (map list->number
      (map vector->list `(,maxi ,mini)))))

(let ((input (import-input)))
  (for-each print (solve input)))
