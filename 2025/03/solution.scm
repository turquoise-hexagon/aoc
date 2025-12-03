(import
  (chicken io)
  (chicken string))

(define (import-input)
  (map
    (lambda (i)
      (list->vector (map string->number (string-chop i 1))))
    (read-lines)))

(define (_compute vec ptr lim)
  (let loop ((ptr ptr) (val 0) (acc 0))
    (if (= ptr lim)
      acc
      (let ((tmp (vector-ref vec ptr)))
        (if (> tmp val)
          (loop (+ ptr 1) tmp ptr)
          (loop (+ ptr 1) val acc))))))

(define (compute vec n)
  (let ((len (vector-length vec)))
    (let loop ((i 0) (ptr 0) (acc 0))
      (if (= i n)
        acc
        (let* ((lim (+ (- len n) i 1)) (ptr (_compute vec ptr lim)))
          (loop (+ i 1) (+ ptr 1) (+ (* acc 10) (vector-ref vec ptr))))))))

(define (solve input n)
  (apply +
    (map
      (lambda (i)
        (compute i n))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 2)))
    (print part/1) (assert (= part/1 17100)))
  (let ((part/2 (solve input 12)))
    (print part/2) (assert (= part/2 170418192256861))))
