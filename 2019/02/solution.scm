(import
  (chicken io)
  (chicken string)
  (euler)
  (only (srfi 133) vector-copy))

(define (import-input)
  (list->vector (map string->number (string-split (read-line) ","))))

(define-inline (process! vec function i)
  (vector-set! vec (vector-ref vec (+ i 3))
    (function
      (vector-ref vec (vector-ref vec (+ i 1)))
      (vector-ref vec (vector-ref vec (+ i 2))))))

(define (run! vec)
  (let ((limit (vector-length vec)))
    (let loop ((i 0))
      (when (< -1 i limit)
        (case (vector-ref vec i)
          (( 1) (process! vec + i) (loop (+ i 4)))
          (( 2) (process! vec * i) (loop (+ i 4)))
          ((99)))))))

(define (edit! vec noun verb)
  (vector-set! vec 1 noun)
  (vector-set! vec 2 verb))

(define (solve/1 input)
  (let ((acc (vector-copy input)))
    (edit! acc 12 2) (run! acc)
    (vector-ref acc 0)))

(define (solve/2 input)
  (call/cc
    (lambda (return)
      (do ((i 0 (+ i 1))) ((= i 99))
        (do ((j 0 (+ j 1))) ((= j 99))
          (let ((acc (vector-copy input)))
            (edit! acc i j) (run! acc)
            (when (= (vector-ref acc 0) 19690720)
              (return (+ (* 100 i) j)))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 5110675)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 4847))))
