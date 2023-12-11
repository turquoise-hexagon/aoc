(import
  (chicken io)
  (euler)
  (srfi 1))

(define offsets (delete-first (power '(-1 0 1) 2) '(0 0) equal?))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (neighbors/1 array coord)
  (filter
    (lambda (i)
      (array-exists? array i))
    (map
      (lambda (i)
        (map + coord i))
      offsets)))

(define (lookup array coord offset)
  (let loop ((i coord))
    (let ((next (map + i offset)))
      (if (array-exists? array next)
        (let ((value (array-ref array next)))
          (if (or (char=? value #\#)
                  (char=? value #\L))
            next
            (loop next)))
        #f))))

(define (neighbors/2 array coord)
  (filter-map
    (lambda (i)
      (lookup array coord i))
    offsets))

(define (iterate array proc n)
  (let ((acc (array-copy array)))
    (for-each
      (lambda (i)
        (let ((value (array-ref array i)))
          (when (or (char=? value #\#)
                    (char=? value #\L))
            (let ((count (count
                           (lambda (i)
                             (char=? (array-ref array i) #\#))
                           (proc array i))))
              (cond
                ((= count 0) (array-set! acc i #\#))
                ((> count n) (array-set! acc i #\L)))))))
      (array-indexes array))
    acc))

(define (value array)
  (count
    (lambda (i)
      (char=? (array-ref array i) #\#))
    (array-indexes array)))

(define (solve input proc n)
  (let loop ((i input) (cnt (value input)))
    (let* ((next/i (iterate i proc n)) (next/cnt (value next/i)))
      (if (= cnt next/cnt)
        cnt
        (loop next/i next/cnt)))))

(let ((input (import-input)))
  (let ((part/1 (solve input neighbors/1 3)))
    (print part/1) (assert (= part/1 2277)))
  (let ((part/2 (solve input neighbors/2 4)))
    (print part/2) (assert (= part/2 2066))))
