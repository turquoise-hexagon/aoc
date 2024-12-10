(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 69))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array
    (map
      (lambda (i)
        (map string->number (string-chop i 1)))
      (read-lines))))

(define (score array coord)
  (if (= (array-ref array coord) 0)
    (let ((acc/1 (make-hash-table)) (acc/2 0))
      (let loop ((coord coord))
        (let ((value (array-ref array coord)))
          (if (= value 9)
            (begin
              (hash-table-set! acc/1 coord #t)
              (set! acc/2 (+ acc/2 1)))
            (for-each
              (lambda (offset)
                (let ((next (map + coord offset)))
                  (if (and (array-exists? array next) (= (array-ref array next) (+ value 1)))
                    (loop next))))
              offsets))))
      (list (hash-table-size acc/1) acc/2))
    '(0 0)))

(define (solve input)
  (apply map +
    (map
      (lambda (coord)
        (score input coord))
      (array-indexes input))))

(let ((parts (solve (import-input))))
  (for-each print parts) (assert (equal? parts '(535 1186))))
