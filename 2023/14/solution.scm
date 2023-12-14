(import
  (chicken io)
  (euler)
  (srfi 69)
  (srfi 1))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (indexes array direction)
  (apply product
    (map
      (lambda (i lst)
        (case i
          ((-1) lst)
          (( 0) lst)
          (( 1) (reverse lst))))
      direction (map iota (array-dimensions array)))))

(define (tilt! array direction)
  (for-each
    (lambda (coord)
      (when (char=? (array-ref array coord) #\O)
        (let loop ((i coord))
          (let ((next (map + i direction)))
            (if (and (array-exists? array next) (char=? (array-ref array next) #\.))
              (loop next)
              (unless (equal? coord i)
                (array-set! array coord #\.)
                (array-set! array i     #\O)))))))
    (indexes array direction)))

(define (score array)
  (let ((dimensions (array-dimensions array)))
    (apply +
      (map
        (lambda (coord)
          (- (car dimensions) (car coord)))
        (filter
          (lambda (coord)
            (char=? (array-ref array coord) #\O))
          (array-indexes array))))))

(define (id array)
  (apply string-append (map list->string (array->list array))))

(define (solve/1 input)
  (tilt! input '(-1 0))
  (score input))

(define (solve/2 input n)
  (let ((cache (make-hash-table)))
    (let loop ((i 1))
      (if (= i n)
        (score input)
        (begin
          (for-each (lambda (i) (tilt! input i)) '((-1 0) (0 -1) (1 0) (0 1)))
          (let ((id (id input)))
            (if (hash-table-exists? cache id)
              (let ((_ (hash-table-ref cache id)))
                (hash-table-clear! cache)
                (loop (- n (modulo (- n _) (- i _)))))
              (begin
                (hash-table-set! cache id i)
                (loop (+ i 1))))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 (array-copy input))))
    (print part/1) (assert (= part/1 108641)))
  (let ((part/2 (solve/2 (array-copy input) 1000000000)))
    (print part/2) (assert (= part/2 84328))))
