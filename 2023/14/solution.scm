(import
  (chicken io)
  (euler)
  (srfi 69)
  (srfi 1))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (search array coord direction)
  (let loop ((i coord))
    (let ((next (map + i direction)))
      (if (and (array-exists? array next) (char=? (array-ref array next) #\.))
        (loop next)
        i))))

(define (indexes array direction)
  (apply product
    (map
      (lambda (i lst)
        (case i
          ((-1) lst)
          (( 0) lst)
          (( 1) (reverse lst))))
      direction (map iota (array-dimensions array)))))

(define (tilt array direction)
  (let ((acc array))
    (for-each
      (lambda (coord)
        (when (char=? (array-ref acc coord) #\O)
          (let ((next (search acc coord direction)))
            (unless (equal? coord next)
              (array-set! acc coord #\.)
              (array-set! acc next  #\O)))))
      (indexes acc direction))
    acc))

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
  (let ((acc (array-copy input)))
    (tilt acc '(-1 0)) (score acc)))

(define (solve/2 input n)
  (let ((cache (make-hash-table)))
    (let loop ((i 1) (acc (array-copy input)))
      (if (= i n)
        (score acc)
        (begin
          (for-each (lambda (i) (tilt acc i)) '((-1 0) (0 -1) (1 0) (0 1)))
          (let ((id (id acc)))
            (if (hash-table-exists? cache id)
              (let ((_ (hash-table-ref cache id)))
                (hash-table-clear! cache)
                (loop (- n (modulo (- n _) (- i _))) acc))
              (begin
                (hash-table-set! cache id i)
                (loop (+ i 1) acc)))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 108641)))
  (let ((part/2 (solve/2 input 1000000000)))
    (print part/2) (assert (= part/2 84328))))
