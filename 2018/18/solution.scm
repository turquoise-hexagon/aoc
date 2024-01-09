(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant offsets
  '((-1 -1) (-1  0) (-1  1)
    ( 0 -1)         ( 0  1)
    ( 1 -1) ( 1  0) ( 1  1)))

(define (neighbors array coord)
  (filter
    (lambda (i)
      (array-exists? array i))
    (map
      (lambda (i)
        (map + coord i))
      offsets)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (compute lst match)
  (count
    (lambda (i)
      (char=? i match))
    lst))

(define (iterate array)
  (let ((acc (array-copy array)))
    (for-each
      (lambda (i)
        (let ((lst (map
                     (lambda (i)
                       (array-ref array i))
                     (neighbors array i))))
          (case (array-ref array i)
            ((#\.)
             (when (>= (compute lst #\|) 3)
               (array-set! acc i #\|)))
            ((#\|)
             (when (>= (compute lst #\#) 3)
               (array-set! acc i #\#)))
            ((#\#)
             (unless (and (>= (compute lst #\|) 1)
                          (>= (compute lst #\#) 1))
               (array-set! acc i #\.))))))
      (array-indexes array))
    acc))

(define (solve input n)
  (let ((cache (make-hash-table)))
    (let loop ((i 0) (acc input))
      (if (= i n)
        (let ((lst (join (array->list acc))))
          (* (compute lst #\|)
             (compute lst #\#)))
        (let ((id (apply string-append (map list->string (array->list acc)))))
          (if (hash-table-exists? cache id)
            (let ((_ (hash-table-ref cache id)))
              (hash-table-clear! cache)
              (loop (- n (modulo (- n _) (- i _))) acc))
            (begin
              (hash-table-set! cache id i)
              (loop (+ i 1) (iterate acc)))))))))

(let ((input (import-input)))
  (let ((part/1 (solve input 10)))
    (print part/1) (assert (= part/1 638400)))
  (let ((part/2 (solve input #e1e9)))
    (print part/2) (assert (= part/2 195952))))
