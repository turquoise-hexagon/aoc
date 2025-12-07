(import
  (chicken io)
  (euler-syntax)
  (srfi 152))

(define (import-input)
  (read-lines))

(define (solve input)
  (bind (str . input) input
    (let* ((len (string-length str)) (acc (make-vector len 0)))
      (vector-set! acc (string-index str (lambda (i) (char=? i #\S))) 1)
      (let main ((input input) (acc/1 0) (acc/2 acc))
        (if (null? input)
          (list acc/1 (apply + (vector->list acc/2)))
          (bind (str . input) input
            (if (string-any (lambda (i) (char=? i #\^)) str)
              (let* ((len (string-length str)) (nxt/2 (make-vector len 0)))
                (let loop ((i 0) (nxt/1 acc/1))
                  (if (= i len)
                    (main input nxt/1 nxt/2)
                    (let ((tmp (vector-ref acc/2 i)))
                      (case (string-ref str i)
                        ((#\^)
                         (vector-set! nxt/2 (- i 1) (+ (vector-ref nxt/2 (- i 1)) tmp))
                         (vector-set! nxt/2 (+ i 1) (+ (vector-ref nxt/2 (+ i 1)) tmp))
                         (loop (+ i 1) (if (> tmp 0) (+ nxt/1 1) nxt/1)))
                        ((#\.)
                         (vector-set! nxt/2 i (+ (vector-ref nxt/2 i) tmp))
                         (loop (+ i 1) nxt/1)))))))
              (main input acc/1 acc/2))))))))

(let* ((input (import-input)) (parts (solve input)))
  (for-each print parts) (assert (equal? parts '(1703 171692855075500))))
