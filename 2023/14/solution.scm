(import
  (chicken io)
  (euler)
  (euler-syntax)
  (srfi 69))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (_tilt! array dir coord)
  (let loop ((i coord))
    (let ((_ (list (+ (car i) (car dir)) (+ (cadr i) (cadr dir)))))
      (if (and (array-exists? array _) (char=? (array-ref array _) #\.))
        (loop _)
        (unless (equal? coord i)
          (array-set! array coord #\.)
          (array-set! array i     #\O))))))

(define-memoized (indexes dimensions dir)
  (apply product
    (map
      (lambda (i n)
        (if (= i 1) (range n 0) (range n)))
      dir (map sub1 dimensions))))

(define (tilt! array dir)
  (for-each
    (lambda (i)
      (when (char=? (array-ref array i) #\O)
        (_tilt! array dir i)))
    (indexes (array-dimensions array) dir)))

(define (score array)
  (let ((_ (car (array-dimensions array))))
    (foldl
      (lambda (acc i)
        (if (char=? (array-ref array i) #\O)
          (+ acc (- _ (car i)))
          acc))
      0 (array-indexes array))))

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
