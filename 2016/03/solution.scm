(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i " ")))
    (read-lines)))

(define (solve input)
  (count
    (lambda (i)
      (apply
        (lambda (a b c)
          (and (> (+ a b) c)
               (> (+ a c) b)
               (> (+ b c) a)))
        i))
    input))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 993)))
  (let ((part/2 (solve (join (map (cut chop <> 3) (apply zip input))))))
    (print part/2) (assert (= part/2 1849))))
