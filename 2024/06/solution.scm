(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant offsets #((-1 0) (0 1) (1 0) (0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (start array)
  (find
    (lambda (coord)
      (char=? (array-ref array coord) #\^))
    (array-indexes array)))

(define cache #f)

(define (make-run array)
  (unless cache
    (set! cache (make-array (array-dimensions array) '())))
  (let ((start (start array)))
    (lambda (array)
      (for-each
        (lambda (coord)
          (array-set! cache coord '()))
        (array-indexes cache))
      (let loop ((coord start) (dir 0))
        (let ((value (array-ref cache coord)))
          (if (member dir value)
            #f
            (let ((next (map + coord (vector-ref offsets dir))))
              (array-set! cache coord (cons dir value))
              (if (array-exists? array next)
                (if (char=? (array-ref array next) #\#)
                  (loop coord (modulo (+ dir 1) 4))
                  (loop next dir))
                cache))))))))

(define (solve/1 input)
  (let* ((run (make-run input)) (acc (run input)))
    (count
      (lambda (coord)
        (not (null? (array-ref acc coord))))
      (array-indexes acc))))

(define (solve/2 input)
  (let ((run (make-run input)))
    (count
      (lambda (coord)
        (array-set! input coord #\#)
        (let ((acc (run input)))
          (array-set! input coord #\.)
          (not acc)))
      (let ((cache (run input)))
        (filter
          (lambda (coord)
            (and (not (null? (array-ref cache coord))) (char=? (array-ref input coord) #\.)))
          (array-indexes cache))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 5086)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1770))))
