(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)
    ( 0  0)))

(define (iterate-wind! table coord offset dimensions)
  (hash-table-set! table coord #t)
  (list (map
          (lambda (c o d)
            (modulo (+ c o d) d))
          coord offset dimensions)
    offset dimensions))

(define (generate-cache lst n)
  (let ((acc (make-vector n)))
    (foldl
      (lambda (lst index)
        (let* ((tmp (make-hash-table)) (lst (map (lambda (_) (apply iterate-wind! tmp _)) lst)))
          (vector-set! acc index tmp) lst))
      lst (range 0 (- n 1)))
    acc))

(define (process-input lst)
  (let* ((array (list->array lst)) (dimensions (array-dimensions array)))
    (list
      (generate-cache
        (foldl
          (lambda (acc coord)
            (case (array-ref array coord)
              ((#\^) (cons (list coord '(-1  0) dimensions) acc))
              ((#\>) (cons (list coord '( 0  1) dimensions) acc))
              ((#\v) (cons (list coord '( 1  0) dimensions) acc))
              ((#\<) (cons (list coord '( 0 -1) dimensions) acc))
              (else acc)))
          '() (array-indexes array))
        (apply lcm dimensions))
      array '(-1 0) (map + '(-1 0) dimensions))))

(define (import-input)
  (let ((_ (map string->list (read-lines))))
    (process-input
      (map
        (lambda (_)
          (cdr (butlast _)))
        (cdr (butlast _))))))

(define (neighbors table array coord from to)
  (remove
    (lambda (coord)
      (hash-table-exists? table coord))
    (filter
      (lambda (coord)
        (or (array-exists? array coord)
            (equal? coord from)
            (equal? coord to)))
      (map
        (lambda (offset)
          (map + coord offset))
        offsets))))

(define (find-path vec array from to start)
  (let loop ((i start) (acc (list from)))
    (if (member to acc) i
      (loop (+ i 1)
        (let ((cache (make-hash-table)) (table (vector-ref vec (modulo i (vector-length vec)))))
          (foldl
            (lambda (acc coord)
              (foldl
                (lambda (acc coord)
                  (if (hash-table-exists? cache coord)
                    acc
                    (begin
                      (hash-table-set! cache coord #t)
                      (cons coord acc))))
                acc (neighbors table array coord from to)))
            '() acc))))))

(define (solve/1 vec array from to)
  (- (find-path vec array from to 0) 1))

(define (solve/2 vec array from to)
  (let loop ((i 0) (acc 0) (from from) (to to))
    (if (= i 3)
      (- acc 1)
      (loop (+ i 1) (find-path vec array from to acc) to from))))

(let ((input (import-input)))
  (print (apply solve/1 input))
  (print (apply solve/2 input)))
