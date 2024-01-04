(import
  (chicken io)
  (chicken string))

(define (import-input)
  (map string->symbol (string-split (read-line) ",")))

(define-inline (offset direction)
  (case direction
    (( n) '( 0  1 -1))
    ((ne) '( 1  0 -1))
    ((se) '( 1 -1  0))
    (( s) '( 0 -1  1))
    ((sw) '(-1  0  1))
    ((nw) '(-1  1  0))))

(define (distance coord)
  (quotient (apply + (map abs coord)) 2))

(define (run lst)
  (foldl
    (lambda (acc i)
      (cons (map + (car acc) (offset i)) acc))
    '((0 0 0)) lst))

(define (solve input)
  (let ((acc (map distance (run input))))
    (list (car acc) (apply max acc))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '(796 1585)))
