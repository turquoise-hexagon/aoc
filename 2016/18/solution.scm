(import
  (chicken io)
  (chicken fixnum))

(define (import-input)
  (map
    (lambda (i)
      (case i
        ((#\^) 1)
        ((#\.) 0)))
    (string->list (read-line))))

(define-inline (_vector-ref vec index)
  (if (and (fx< -1 index) (fx< index len))
    (vector-ref vec index)
    0))

(define (solve vec iterations rule)
  (let* ((len (vector-length vec)) (vec (vector vec (make-vector len))))
    (let main ((iteration 0) (flag 0) (acc 0))
      (if (fx= iteration iterations)
        acc
        (let*
          ((next (fxxor flag 1))
           (a (vector-ref vec flag))
           (b (vector-ref vec next)))
          (let loop ((index 0) (acc acc))
            (if (fx= index len)
              (main (fx+ iteration 1) next acc)
              (let*
                ((value 0)
                 (value (fxior (fxshl value 1) (_vector-ref a (fx+ index -1))))
                 (value (fxior (fxshl value 1) (_vector-ref a (fx+ index  0))))
                 (value (fxior (fxshl value 1) (_vector-ref a (fx+ index +1)))))
                (vector-set! b index (fxand (fxshr rule value) 1))
                (loop (fx+ index 1) (fx+ acc (fxxor (vector-ref a index) 1)))))))))))

(let ((input (import-input)))
  (let ((part/1 (solve (list->vector input) 40 90)))
    (print part/1) (= part/1 1951))
  (let ((part/2 (solve (list->vector input) 400000 90)))
    (print part/2) (= part/2 20002936)))
