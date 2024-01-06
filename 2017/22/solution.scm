(import
  (chicken io)
  (chicken fixnum)
  (euler))

(define-constant dimensions
  '(500 500))

(define-constant dirs
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define-inline (map2 fun a b)
  (list
    (fun (car  a) (car  b))
    (fun (cadr a) (cadr b))))

(define (center array)
  (map
    (lambda (i)
      (quotient i 2))
    (array-dimensions array)))

(define (import-input)
  (let* ((acc (make-array dimensions 0)) (offset (center acc)) (tmp (list->array (map string->list (read-lines)))))
    (for-each
      (lambda (i)
        (array-set! acc (map2 fx+ i offset) (if (char=? (array-ref tmp i) #\.) 0 2)))
      (array-indexes tmp))
    (values acc (map2 fx+ offset (center tmp)))))

(define-constant rules/1
  #(( 2 -1  1)
    ( 0  0  0)
    ( 0  1  0)
    ( 0  0  0)))

(define-constant rules/2
  #(( 1 -1  0)
    ( 2  0  1)
    ( 3  1  0)
    ( 0  2  0)))

(define (solve array coord rules iterations)
  (let ((array (array-copy array)))
    (let loop ((i 0) (cur coord) (dir 0) (acc 0))
      (if (= i iterations)
        acc
        (apply
          (lambda  (next/val dir+ acc+)
            (let* ((next/dir (fxmod (fx+ dir dir+) 4))
                   (next/cur (map2 fx+ cur (vector-ref dirs next/dir)))
                   (next/acc (fx+ acc acc+)))
              (array-set! array cur next/val)
              (loop (+ i 1) next/cur next/dir next/acc)))
          (vector-ref rules (array-ref array cur)))))))

(let-values (((array coord) (import-input)))
  (let ((part/1 (solve array coord rules/1 #e1e4)))
    (print part/1) (assert (= part/1 5404)))
  (let ((part/2 (solve array coord rules/2 #e1e7)))
    (print part/2) (assert (= part/2 2511672))))
