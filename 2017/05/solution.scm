(import
  (chicken io)
  (chicken fixnum))

(define (import-input)
  (map string->number (read-lines)))

(define (solve input proc)
  (let* ((vec (list->vector input)) (len (vector-length vec)))
    (let loop ((i 0) (acc 0))
      (if (fx>= i len)
        acc
        (let ((tmp (vector-ref vec i)))
          (vector-set! vec i (proc tmp))
          (loop (fx+ i tmp) (fx+ acc 1)))))))

(let ((input (import-input)))
  (let ((part/1 (solve input (lambda (i) (fx+ i 1)))))
    (print part/1) (assert (= part/1 373160)))
  (let ((part/2 (solve input (lambda (i) (fx+ i (if (fx> i 2) -1 +1))))))
    (print part/2) (assert (= part/2 26395586))))
