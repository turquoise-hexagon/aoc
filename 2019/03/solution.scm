(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (offset direction)
  (case direction
    ((U) '(-1  0))
    ((R) '( 0  1))
    ((D) '( 1  0))
    ((L) '( 0 -1))))

(define (increment! table coord steps index)
  (hash-table-update! table coord
    (lambda (value)
      (if (not (assoc index value))
        (cons (cons index steps) value)
        value))))

(define (process lst)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (instructions index)
        (let loop ((lst instructions) (coord '(0 0)) (steps 0))
          (unless (null? lst)
            (apply
              (lambda (direction value)
                (let ((offset (offset direction)))
                  (do ((coord coord (map + coord offset))
                       (value value (- value 1))
                       (steps steps (+ steps 1)))
                    ((= value 0)
                     (loop (cdr lst) coord steps))
                    (increment! acc coord steps index))))
              (car lst)))))
      lst (iota (length lst)))
    acc))

(define (intersections table)
  (hash-table-delete! table '(0 0))
  (map
    (lambda (coord)
      (cons coord (hash-table-ref table coord)))
    (filter
      (lambda (coord)
        (> (length (hash-table-ref table coord)) 1))
      (hash-table-keys table))))

(define (import-input)
  (intersections
    (process
      (map
        (lambda (i)
          (map
            (lambda (i)
              (list
                (string->symbol (substring i 0 1))
                (string->number (substring i 1))))
            (string-split i ",")))
        (read-lines)))))

(define (distance a b)
  (apply + (map abs (map - a  b))))

(define (solve/1 input)
  (apply min
    (map
      (lambda (coord)
        (distance coord '(0 0)))
      (map car input))))

(define (solve/2 input)
  (apply min
    (map
      (lambda (value)
        (apply + (map cdr (cdr value))))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1084)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 9240))))
