(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (import-input)
  (map
    (lambda (str)
      (map string->list (irregex-split "\n" str)))
    (irregex-split "\n{2}" (read-string #f))))

(define (solve input proc)
  (length (flatten (map (cut apply proc char=? <>) input))))

(let ((input (import-input)))
  (print (solve input lset-union))
  (print (solve input lset-intersection)))
