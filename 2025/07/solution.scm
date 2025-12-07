(import
  (chicken io)
  (euler-syntax)
  (srfi 1))

(define (import-input)
  (map string->list (read-lines)))

(define (solve input)
  (let main ((input input) (acc/1 0) (acc/2 #()))
    (if (null? input)
      (list acc/1 (apply + (vector->list acc/2)))
      (bind (lst . input) input
        (cond
          ((any (lambda (i) (char=? i #\S)) lst)
           (let ((nxt/2 (make-vector (length lst) 0)))
             (let loop ((lst lst) (i 0))
               (bind (head . lst) lst
                 (if (char=? head #\S)
                   (begin
                     (vector-set! nxt/2 i 1)
                     (main input acc/1 nxt/2))
                   (loop lst (+ i 1)))))))
          ((any (lambda (i) (char=? i #\^)) lst)
           (let ((nxt/2 (make-vector (length lst) 0)))
             (let loop ((lst lst) (i 0) (nxt/1 acc/1))
               (if (null? lst)
                 (main input nxt/1 nxt/2)
                 (bind (head . lst) lst
                   (let ((tmp (vector-ref acc/2 i)))
                     (if (char=? head #\^)
                       (begin
                         (vector-set! nxt/2 (- i 1) (+ (vector-ref nxt/2 (- i 1)) tmp))
                         (vector-set! nxt/2 (+ i 1) (+ (vector-ref nxt/2 (+ i 1)) tmp))
                         (loop lst (+ i 1) (if (> tmp 0) (+ nxt/1 1) nxt/1)))
                       (begin
                         (vector-set! nxt/2 i (+ (vector-ref nxt/2 i) tmp))
                         (loop lst (+ i 1) nxt/1)))))))))
          (else (main input acc/1 acc/2)))))))

(let ((input (import-input)))
  (let ((parts (solve input)))
    (for-each print parts) (assert (equal? parts '(1703 171692855075500)))))
