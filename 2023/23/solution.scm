(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (neighbors array coord)
  (filter
    (lambda (coord)
      (and (array-exists? array coord) (not (char=? (array-ref array coord) #\#))))
    (map
      (lambda (offset)
        (map + coord offset))
      offsets)))

(define (graph/1 array)
  (let ((acc (make-array (array-dimensions array) '())))
    (for-each
      (lambda (coord)
        (array-set! acc coord
          (case (array-ref array coord)
            ((#\^) (list (list (map + coord '(-1  0)) 1)))
            ((#\>) (list (list (map + coord '( 0  1)) 1)))
            ((#\v) (list (list (map + coord '( 1  0)) 1)))
            ((#\<) (list (list (map + coord '( 0 -1)) 1)))
            ((#\.)
             (map
               (lambda (next)
                 (list next 1))
               (neighbors array coord))))))
      (array-indexes array))
    acc))

(define (graph/2 array)
  (let ((acc (make-array (array-dimensions array) '())))
    (for-each
      (lambda (coord)
        (array-set! acc coord
          (map
            (lambda (next)
              (let loop ((coord coord) (next next) (dist 1))
                (let ((_ (neighbors array next)))
                  (if (= (length _) 2)
                    (loop next (first (delete-first _ coord equal?)) (+ dist 1))
                    (list next dist)))))
            (neighbors array coord))))
      (remove
        (lambda (coord)
          (char=? (array-ref array coord) #\#))
        (array-indexes array)))
    acc))

(define (solve graph)
  (let* ((dimensions (array-dimensions graph)) (cache (make-array dimensions #f)) (goal (map - dimensions '(1 2))))
    (let loop ((coord '(0 1)) (total 0))
      (if (equal? coord goal)
        total
        (begin
            (array-set! cache coord #t)
          (let ((acc (foldl
                       (lambda (acc i)
                         (apply
                           (lambda (next dist)
                             (if (not (array-ref cache next))
                               (max acc (loop next (+ total dist)))
                               acc)) i))
                       total (array-ref graph coord))))
            (array-set! cache coord #f)
            acc))))))

(let ((input (import-input)))
  (let ((part/1 (solve (graph/1 input))))
    (print part/1) (assert (= part/1 2306)))
  (let ((part/2 (solve (graph/2 input))))
    (print part/2) (assert (= part/2 6718))))
