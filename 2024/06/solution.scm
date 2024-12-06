(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant OFFSETS
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (start array)
  (find
    (lambda (coord)
      (char=? (array-ref array coord) #\^))
    (array-indexes array)))

(define (run array)
  (let ((acc (make-array (array-dimensions array) '())))
    (let loop ((coord (start array)) (index 0))
      (if (member index (array-ref acc coord))
        #f
        (begin
          (array-set! acc coord (cons index (array-ref acc coord)))
          (let ((next (map + coord (vector-ref OFFSETS index))))
            (if (array-exists? array next)
              (if (char=? (array-ref array next) #\#)
                (loop coord (modulo (+ index 1) 4))
                (loop next index))
              acc)))))))

(define (solve/1 input)
  (let ((acc (run input)))
    (count
      (lambda (coord)
        (not (null? (array-ref acc coord))))
      (array-indexes acc))))

(define (solve/2 input)
  (let ((acc (run input)))
    (count
      (lambda (coord)
        (if (null? (array-ref acc coord))
          #f
          (if (char=? (array-ref input coord) #\.)
            (begin
              (array-set! input coord #\#)
              (let ((_ (run input)))
                (array-set! input coord #\.)
                (not _)))
            #f)))
      (array-indexes input))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 5086)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 1770))))
