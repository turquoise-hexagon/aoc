(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  (delete '(0 0) (combinations '(-1 0 1) 2)))

(define (import-input)
  (list->array
    (map
      (lambda (str)
        (map string->number (string-chop str 1)))
      (read-lines))))

(define (neighbors array coord)
  (filter
    (lambda (neighbor)
      (array-exists? array neighbor))
    (map
      (lambda (offset)
        (map + offset coord))
      offsets)))

(define (iterate! array)
  (let ((acc (make-hash-table)))
    (define (helper! coord)
      (unless (hash-table-exists? acc coord)
        (let ((tmp (array-ref array coord)))
          (if (= tmp 9)
            (begin
              (hash-table-set! acc coord #t)
              (array-set! array coord 0)
              (for-each helper!
                (neighbors array coord)))
            (array-set! array coord (+ tmp 1))))))
    (for-each helper! (array-indexes array))
    (hash-table-size acc)))

(define (valid? array)
  (every
    (lambda (coord)
      (= (array-ref array coord) 0))
    (array-indexes array)))

(define (solve/1 input)
  (apply +
    (map
      (lambda (_)
        (iterate! input))
      (range 1 100))))

(define (solve/2 input)
  (let loop ((i 0))
    (if (valid? input)
      i
      (begin
        (iterate! input)
        (loop (+ i 1))))))

(let ((input (import-input)))
  (print (solve/1 (array-copy input)))
  (print (solve/2 (array-copy input))))
