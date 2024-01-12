(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant dimensions '(25 6))

(define (import-input)
  (foldl chop
    (map
      (lambda (i)
        (case i
          ((#\0) " ")
          ((#\1) "█")
          ((#\2) "░")))
      (string->list (read-line))) dimensions))

(define (solve/1 input)
  (let ((acc (extremum input
                (lambda (i)
                  (count
                    (lambda (i)
                      (string=? i " "))
                    (join i)))
                <)))
    (* (count (lambda (i) (string=? i "█")) (join acc))
       (count (lambda (i) (string=? i "░")) (join acc)))))

(define (solve/2 input)
  (for-each
    (lambda (i)
      (print
        (apply string-append
          (map
            (lambda (i)
              (find
                (lambda (i)
                  (not (string=? i "░")))
                i))
            i))))
    (apply map zip input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 2193)))
  (solve/2 input))
