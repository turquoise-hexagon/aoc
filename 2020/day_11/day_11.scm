(import
  (chicken io)
  (euler)
  (srfi 1))

(define offsets
  (delete '(0 0) (combinations '(-1 0 1) 2)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (count-neighbors/1 array coord)
  (count (cut char=? <> #\#)
    (map (cut array-ref array <>)
      (filter (cut array-exists? array <>)
        (map (cut map + <> coord) offsets)))))

(define (count-neighbors/h array coord offset)
  (let ((coord (map + coord offset)))
    (if (array-exists? array coord)
      (case (array-ref array coord)
        ((#\#) 1)
        ((#\L) 0)
        ((#\.) (count-neighbors/h array coord offset)))
      0)))

(define (count-neighbors/2 array coord)
  (apply + (map (cut count-neighbors/h array coord <>) offsets)))

(define (iterate array proc n)
  (let ((acc (array-copy array)))
    (for-each
      (lambda (coord)
        (let ((value (array-ref array coord)))
          (unless (char=? value #\.)
            (let ((cnt (proc array coord)))
              (case value
                ((#\L) (when (=  cnt 0) (array-set! acc coord #\#)))
                ((#\#) (when (>= cnt n) (array-set! acc coord #\L))))))))
      (array-indexes array))
    (if (equal? array acc)
      #f
      acc)))

(define (solve input proc n)
  (let loop ((acc input))
    (let ((next (iterate acc proc n)))
      (if next (loop next)
        (count (cut char=? <> #\#)
          (map (cut array-ref acc <>)
            (array-indexes acc)))))))

(let ((input (import-input)))
  (print (solve input count-neighbors/1 4))
  (print (solve input count-neighbors/2 5)))
