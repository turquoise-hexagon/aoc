(import
  (chicken bitwise)
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (let ((str (read-line)))
    (values
      (map string->number (string-split str ","))
      (append (map char->integer (string->list str)) '(17 31 73 47 23)))))

(define (swap! vec a b)
  (let
    ((m (vector-ref vec a))
     (n (vector-ref vec b)))
    (vector-set! vec a n)
    (vector-set! vec b m)))

(define (hash/1 vec)
  (* (vector-ref vec 0)
     (vector-ref vec 1)))

(define (hash/2 vec)
  (apply string-append
    (map
      (lambda (i)
        (number->string (apply bitwise-xor i) 16))
      (chop (vector->list vec) 16))))

(define (solve input iterations hash)
  (let ((vec (list->vector (iota 256))))
     (let loop ((iteration 0) (index 0) (skip 0) (lst input))
       (if (= iteration iterations)
         (hash vec)
         (if (null? lst)
           (loop (+ iteration 1) index skip input)
           (let* ((value (car lst)) (limit (quotient value 2)))
             (let subloop ((offset 0))
               (if (= offset limit)
                 (loop iteration (+ index value skip) (+ skip 1) (cdr lst))
                 (begin
                   (swap! vec
                     (modulo (+ index offset) 256)
                     (modulo (- (+ index value) offset 1) 256))
                   (subloop (+ offset 1)))))))))))

(let-values (((input/1 input/2) (import-input)))
  (let ((part/1 (solve input/1  1 hash/1)))
    (print part/1) (assert (= part/1 8536)))
  (let ((part/2 (solve input/2 64 hash/2)))
    (print part/2) (assert (string=? part/2 "aff593797989d665349efe11bb4fd99b"))))
