(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (string->list (read-line)))

(define (dragon lst)
  (append lst (list #\0)
    (map
      (lambda (i)
        (if (char=? i #\0) #\1 #\0))
      (reverse lst))))

(define (checksum lst n)
  (let loop ((lst (take lst n)))
    (if (odd? (length lst))
      lst
      (loop
        (map
          (lambda (i)
            (if (apply char=? i) #\1 #\0))
          (chop lst 2))))))

(define (solve input n)
  (list->string
    (let loop ((lst input))
      (if (< (length lst) n)
        (loop (dragon lst))
        (checksum lst n)))))

(let ((input (import-input)))
  (let ((part/1 (solve input 272)))
    (print part/1) (assert (string=? part/1 "10101001010100001")))
  (let ((part/2 (solve input 35651584)))
    (print part/2) (assert (string=? part/2 "10100001110101001"))))
