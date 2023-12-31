(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant keypad/1
  '("123"
    "456"
    "789"))

(define-constant keypad/2
  '("  1  "
    " 234 "
    "56789"
    " ABC "
    "  D  "))

(define-inline (offset direction)
  (case direction
    ((#\U) '(-1  0))
    ((#\R) '( 0  1))
    ((#\D) '( 1  0))
    ((#\L) '( 0 -1))))

(define (import-input)
  (map string->list (read-lines)))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\5))
    (array-indexes array)))

(define (run array lst)
  (foldl
    (lambda (acc i)
      (let ((next (map + acc (offset i))))
        (if (and (array-exists? array next) (not (char=? (array-ref array next) #\ )))
          next
          acc)))
    (start array) lst))

(define (solve input keypad)
  (let ((array (list->array (map string->list keypad))))
    (list->string
      (map
        (lambda (i)
          (array-ref array (run array i)))
        input))))

(let ((input (import-input)))
  (let ((part/1 (solve input keypad/1)))
    (print part/1) (assert (string=? part/1 "36629")))
  (let ((part/2 (solve input keypad/2)))
    (print part/2) (assert (string=? part/2 "99C3D"))))
