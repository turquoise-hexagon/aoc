(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map string->list (read-lines)))

(define (init-tuple lst size)
  (append lst (make-list (- size (length lst)) 0)))

(define (generate-tuples-table input size)
  (let ((table (make-hash-table)))
    (for-each
      (lambda (coord)
        (receive (x y) (apply values coord)
          (when (char=? (list-ref (list-ref input x) y) #\#)
            (hash-table-set! table (init-tuple coord size) #t))))
      (product (iota (length input)) (iota (length (car input)))))
    table))

(define (generate-neighbors tuple offsets)
  (delete tuple (map (cut map + tuple <>) offsets)))

(define (generate-neighbors-table tuples-table offsets)
  (let ((table (make-hash-table)))
    (for-each
      (lambda (tuple)
        (for-each
          (lambda (tuple)
            (hash-table-set! table tuple (+ (hash-table-ref/default table tuple 0) 1)))
          (generate-neighbors tuple offsets)))
      (hash-table-keys tuples-table))
    table))

(define (iterate tuples-table offsets)
  (let ((next (make-hash-table)))
    (hash-table-for-each (generate-neighbors-table tuples-table offsets)
      (lambda (tuple count)
        (when (or      (= count 3)
                  (and (= count 2)
                       (hash-table-exists? tuples-table tuple)))
          (hash-table-set! next tuple #t))))
    next))

(define (solve input size n)
  (let ((table (generate-tuples-table input size)) (offsets (power '(-1 0 1) size)))
    (let loop ((table table) (n n))
      (if (= n 0) (hash-table-size table)
        (loop (iterate table offsets) (- n 1))))))

(let ((input (import-input)))
  (print (solve input 3 6))
  (print (solve input 4 6)))
