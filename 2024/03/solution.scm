(import
  (chicken io)
  (chicken irregex))

(define-constant REGEX/1 '(or (: "do" (? "n't") "()") (: "mul(" (+ (/ "09")) "," (+ (/ "09")) ")")))
(define-constant REGEX/2 '(+ (/ "09")))

(define (solve input part)
  (set! flag #t)
  (foldl
    (lambda (acc i)
      (cond
        ((string=? i    "do()") (set! flag #t) acc)
        ((string=? i "don't()") (set! flag #f) acc)
        ((or part flag) (+ acc (apply * (map string->number (irregex-extract REGEX/2 i)))))
        (else acc)))
    0 (irregex-extract REGEX/1 input)))

(let ((input (read-string)))
  (let ((part/1 (solve input #t)))
    (print part/1) (assert (= part/1 170778545)))
  (let ((part/2 (solve input #f)))
    (print part/2) (assert (= part/2 82868252))))
