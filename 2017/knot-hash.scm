(import
  (chicken fixnum))

(define-constant knot-hash-extra-lengths '(17 31 73 47 23))

(define-inline (knot-hash-convert-chunk chunk)
  (let ((str (number->string chunk 16)))
    (if (fx= (string-length str) 1)
      (string-append "0" str)
      str)))

(define-inline (knot-hash-convert vec)
  (let loop ((i 1) (chunk (vector-ref vec 0)) (acc ""))
    (if (fx= i 256)
      (string-append acc (knot-hash-convert-chunk chunk))
      (if (fx= (fxmod i 16) 0)
        (loop (fx+ i 1) (vector-ref vec i) (string-append acc (knot-hash-convert-chunk chunk)))
        (loop (fx+ i 1) (fxxor chunk (vector-ref vec i)) acc)))))

(define-inline (vector-swap! vec a b)
  (let ((m (vector-ref vec a))
        (n (vector-ref vec b)))
    (vector-set! vec a n)
    (vector-set! vec b m)))

(define-inline (knot-hash-process lengths)
  (let ((vec (make-vector 256)))
    (do ((i 0 (fx+ i 1))) ((fx= i 256))
      (vector-set! vec i i))
    (let loop ((iteration 0) (index 0) (skip 0) (lst lengths))
      (if (fx= iteration 64)
        (knot-hash-convert vec)
        (if (null? lst)
          (loop (fx+ iteration 1) index skip lengths)
          (let* ((value (car lst)) (limit (fx/ value 2)) (next (fx+ index value)))
            (do ((offset 0 (fx+ offset 1)))
              ((fx= offset limit)
               (loop iteration (fx+ next skip) (fx+ skip 1) (cdr lst)))
              (vector-swap! vec
                (fxmod (fx+ index offset) 256)
                (fxmod (fx- (fx- next offset) 1) 256)))))))))

(define (knot-hash input)
  (knot-hash-process (append (map char->integer (string->list input)) knot-hash-extra-lengths)))
