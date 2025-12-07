(import
  (chicken io)
  (euler-syntax))

(define (import-input)
  (read-lines))

(define (solve input)
  (let* ((1st (car input)) (len (string-length 1st)) (mem (make-vector len 0)))
    (let main ((input input) (acc 0))
      (if (null? input)
        (list acc (apply + (vector->list mem)))
        (bind (str . input) input
          (let loop ((i 0) (acc acc))
            (if (= i len)
              (main input acc)
              (case (string-ref str i)
                ((#\^)
                 (let ((tmp (vector-ref mem i)))
                   (vector-set! mem i 0)
                   (vector-set! mem (- i 1) (+ (vector-ref mem (- i 1)) tmp))
                   (vector-set! mem (+ i 1) (+ (vector-ref mem (+ i 1)) tmp))
                   (loop (+ i 1) (if (> tmp 0) (+ acc 1) acc))))
                ((#\S)
                 (vector-set! mem i 1)
                 (loop (+ i 1) acc))
                (else
                 (loop (+ i 1) acc))))))))))

(let* ((input (import-input)) (parts (solve input)))
  (for-each print parts) (assert (equal? parts '(1703 171692855075500))))
