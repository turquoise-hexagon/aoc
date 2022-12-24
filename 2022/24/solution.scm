(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define MAGIC 1000)

(define OFFSETS
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)
    ( 0  0)))

(define (iterate! table coord offset dims)
  (hash-table-set! table coord #t)
  (list
    (map
      (lambda (c o d)
        (modulo (+ c o d) d))
      coord offset dims)
    offset dims))

(define (generate-cache lst)
  (let ((acc (make-vector (+ MAGIC 1))))
    (foldl
      (lambda (lst index)
        (let* ((tmp (make-hash-table)) (lst (map (lambda (_) (apply iterate! tmp _)) lst)))
          (vector-set! acc index tmp) lst))
      lst (range 0 MAGIC))
    acc))

(define (process-input lst)
  (let* ((array (list->array lst)) (dims (array-dimensions array)))
    (list
      (generate-cache
        (foldl
          (lambda (acc coord)
            (case (array-ref array coord)
              ((#\^) (cons (list coord '(-1  0) dims) acc))
              ((#\>) (cons (list coord '( 0  1) dims) acc))
              ((#\v) (cons (list coord '( 1  0) dims) acc))
              ((#\<) (cons (list coord '( 0 -1) dims) acc))
              (else acc)))
          '() (array-indexes array)))
      array '(-1 0) (apply (lambda (h w) (list h (- w 1))) dims))))

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
        OFFSETS))))

(define (find-path vec array from to start)
  (let loop ((i start) (acc (list from)))
    (if (member to acc) i
      (loop (+ i 1)
        (let ((cache (make-hash-table)) (table (vector-ref vec i)))
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
  (let loop ((i 0) (start 0) (from from) (to to))
    (if (= i 3)
      (- start 1)
      (loop (+ i 1) (find-path vec array from to start) to from))))

(let ((input (import-input)))
  (print (apply solve/1 input))
  (print (apply solve/2 input)))
