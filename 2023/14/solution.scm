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

(define (iterate! array direction)
  (foldl
    (lambda (acc coord)
      (if (char=? (array-ref array coord) #\O)
        (let ((next (search array coord direction)))
          (if (equal? coord next)
            acc
            (begin
              (array-set! array coord #\.)
              (array-set! array next  #\O)
              #t)))
        acc))
    #f (array-indexes array)))

(define (tilt array direction)
  (let ((acc (array-copy array)))
    (let loop ()
      (when (iterate! acc direction)
        (loop)))
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
  (score (tilt input '(-1 0))))

(define (solve/2 input n)
  (let ((cache (make-hash-table)))
    (let loop ((i 1) (acc input))
      (if (= i n)
        (score acc)
        (let* ((acc (foldl tilt acc '((-1 0) (0 -1) (1 0) (0 1)))) (id (id acc)))
          (if (hash-table-exists? cache id)
            (let ((_ (hash-table-ref cache id)))
              (hash-table-clear! cache)
              (loop (- n (modulo (- n _) (- i _))) acc))
            (begin
              (hash-table-set! cache id i)
              (loop (+ i 1) acc))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 108641)))
  (let ((part/2 (solve/2 input 1000000000)))
    (print part/2) (assert (= part/2 84328))))
