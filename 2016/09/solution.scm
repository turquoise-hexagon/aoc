(import
  (chicken io)
  (chicken string))

(define (import-input)
  (read-line))

(define (solve input flag)
  (let loop ((str input) (acc 0))
    (let ((s (substring-index "(" str))
          (e (substring-index ")" str)))
      (if (and s e)
        (apply
          (lambda (t r)
            (let ((a (+ e 1))
                  (b (+ e t 1)))
              (if flag
                (+ (loop (substring str b) acc) (* (loop (substring str a b) 0) r))
                (loop (substring str b) (+ acc s (* t r))))))
          (let ((m (substring str (+ s 1) e)))
            (map string->number (string-split m "x"))))
        (+ acc (string-length str))))))

(let ((input (import-input)))
  (let ((part/1 (solve input #f)))
    (print part/1) (assert (= part/1 138735)))
  (let ((part/2 (solve input #t)))
    (print part/2) (assert (= part/2 11125026826))))
