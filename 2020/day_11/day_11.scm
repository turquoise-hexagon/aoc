(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  (delete '(0 0) (combinations '(-1 0 1) 2)))

(define (pack-input lst)
  (let ((table (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (hash-table-set! table coord
            (list-ref (list-ref lst x) y))))
      (product (iota (length lst))
               (iota (length (car lst)))))
    table))

(define (import-input)
  (pack-input (map string->list (read-lines))))

(define (count-neighbors/1 table coord)
  (count (cut char=? <> #\#)
    (map (cut hash-table-ref/default table <> #\.)
      (map (cut map + coord <>) offsets))))

(define (count-neighbors/h table coord offset)
  (let ((coord (map + coord offset)))
    (if (hash-table-exists? table coord)
      (case (hash-table-ref table coord)
        ((#\#) 1)
        ((#\L) 0)
        ((#\.) (count-neighbors/h table coord offset)))
      0)))

(define (count-neighbors/2 table coord)
  (apply + (map (cut count-neighbors/h table coord <>) offsets)))

(define (iterate table proc n)
  (let ((copy (hash-table-copy table)))
    (hash-table-for-each table
      (lambda (coord value)
        (unless (char=? value #\.)
          (let ((cnt (proc table coord)))
            (case value
              ((#\L) (when (=  cnt 0) (hash-table-set! copy coord #\#)))
              ((#\#) (when (>= cnt n) (hash-table-set! copy coord #\L))))))))
    (if (equal? copy table) #f
      copy)))

(define (solve input proc n)
  (let loop ((table input))
    (let ((next (iterate table proc n)))
      (if next (loop next)
        (count (cut char=? <> #\#) (hash-table-values table))))))

(let ((input (import-input)))
  (print (solve input count-neighbors/1 4))
  (print (solve input count-neighbors/2 5)))
