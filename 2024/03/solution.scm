(import
  (chicken io)
  (chicken irregex))

(define (solve input part)
  (set! flag #t)
  (foldl
    (lambda (acc i)
      (cond
        ((string=? i    "do()") (set! flag #t) acc)
        ((string=? i "don't()") (set! flag #f) acc)
        ((or part flag) (+ acc (apply * (map string->number (irregex-extract "[0-9]+" i)))))
        (else acc)))
    0 (irregex-extract "(do(n\'t)?\\(\\)|mul\\([0-9]+,[0-9]+\\))" input)))

(let ((input (read-string)))
  (let ((part/1 (solve input #t)))
    (print part/1) (assert (= part/1 170778545)))
  (let ((part/2 (solve input #f)))
    (print part/2) (assert (= part/2 82868252))))
