(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (map string->list (irregex-split "\n" str)))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (solve proc input)
  (print (apply + (map length (map (cut apply proc char=? <>) input)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve lset-union input)
    (solve lset-intersection input)))
