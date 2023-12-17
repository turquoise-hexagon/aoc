(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define-constant U '(-1  0))
(define-constant R '( 0  1))
(define-constant D '( 1  0))
(define-constant L '( 0 -1))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (pi a b)
  (let ((_ (+ a b))) (+ (quotient (* _ (+ _ 1)) 2) b)))

(define-inline (split a b c d)
  (let ((id (pi (apply pi dir) (apply pi coord))))
    (unless (hash-table-exists? mem id)
      (hash-table-set! mem id #t)
      (if (or (equal? dir a)
              (equal? dir b))
        (begin
          (next c)
          (next d))
        (next dir)))))

(define (run array dir coord)
  (let ((acc (array-copy array)) (mem (make-hash-table)))
    (let loop ((dir dir) (coord coord))
      (define (next dir)
        (loop dir (list (+ (car coord) (car dir)) (+ (cadr coord) (cadr dir)))))
      (when (array-exists? array coord)
        (array-set! acc coord #\#)
        (case (array-ref array coord)
          ((#\/) (next (map - (reverse dir))))
          ((#\\) (next (map + (reverse dir))))
          ((#\-) (split U D L R))
          ((#\|) (split L R U D))
          (else  (next dir)))))
    (count (lambda (i) (char=? (array-ref acc i) #\#)) (array-indexes acc))))

(define (solve/1 input)
  (run input R '(0 0)))

(define (solve/2 input)
  (apply
    (lambda (h w)
      (apply max
        (map
          (lambda (i)
            (apply run input i))
          (append
            (product (list U) (product (list  h) (range w)))
            (product (list R) (product (range h)      '(0)))
            (product (list D) (product      '(0) (range w)))
            (product (list L) (product (range h) (list  w)))))))
    (map sub1 (array-dimensions input))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 8249)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 8444))))
