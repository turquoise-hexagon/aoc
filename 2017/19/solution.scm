(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (start array)
  (find
    (lambda (i)
      (char=? (array-ref array i) #\|))
    (array-indexes array)))

(define (run input)
  (let loop ((coord (start input)) (offset '(1 0)) (acc '()))
    (let subloop ((offsets (if (char=? (array-ref input coord) #\+) offsets (list offset))))
      (if (null? offsets)
        (cons coord acc)
        (let* ((offset (car offsets)) (next (map + coord offset)))
          (if (and (or (null? acc) (not (equal? next (car acc)))) (array-exists? input next) (not (char=? (array-ref input next) #\ )))
            (loop next offset (cons coord acc))
            (subloop (cdr offsets))))))))

(define (solve input)
  (let ((acc (run input)))
    (list
      (list->string
        (foldl
          (lambda (acc coord)
            (let ((value (array-ref input coord)))
              (case value
                ((#\+ #\- #\|) acc)
                (else (cons value acc)))))
          '() acc))
      (length acc))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '("DTOUFARJQ" 16642)))
