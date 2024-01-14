(import
  (chicken io)
  (chicken string)
  (euler))

(define (import-input)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (if (string->number i)
            (string->number i)
            (string->symbol i)))
        (string-split i " ")))
    (read-lines)))

(define (solve/2 input iterations size card)
  (let loop ((lst (reverse input)) (a 1) (b 0))
    (if (null? lst)
      (let ((_ (modular-expt a iterations size)))
        (modulo (+ (* card _) (* b (- 1 _) (modular-inverse (- 1 a) size))) size))
      (apply
        (case-lambda
          ((_ id _ n)
           (case id
             ((with)
              (let ((_ (modular-inverse n size)))
                (loop (cdr lst) (* a _) (* b _))))
             ((into)
              (loop (cdr lst) (- a) (- size b 1)))))
          ((_ n)
           (loop (cdr lst) a (+ b n))))
        (car lst)))))

(define (solve/1 input size card)
  (do ((i 0 (+ i 1)))
    ((= (solve/2 input 1 size i) card)
     i)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input 10007 2019)))
    (print part/1) (assert (= part/1 6431)))
  (let ((part/2 (solve/2 input 101741582076661 119315717514047 2020)))
    (print part/2) (assert (= part/2 101741582076661))))
