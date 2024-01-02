(import
  (chicken io)
  (euler)
  (simple-md5)
  (srfi 1))

(define-constant limit #e3e4)

(define (import-input)
  (read-line))

(define-inline (hash/1 input n)
  (string->md5sum (string-append input (number->string n))))

(define-inline (hash/2 input n)
  (let loop ((i 0) (acc (hash/1 input n)))
    (if (= i 2016)
      acc
      (loop (+ i 1) (string->md5sum acc)))))

(define (make-hash input hash)
  (let ((acc (make-vector limit)))
    (do ((i 0 (+ i 1)))
      ((= i limit))
      (vector-set! acc i (run-length (string->list (hash input i)) char=?)))
    (lambda (n)
      (vector-ref acc n))))

(define (valid? hash n)
  (let ((m (find
             (lambda (i)
               (>= (car i) 3))
             (hash n))))
    (if m
      (let loop ((i 1))
        (if (> i 1000)
          #f
          (if (any
                (lambda (i)
                  (and (>= (car i) 5)
                       (char=?
                         (cdr i)
                         (cdr m))))
                (hash (+ n i)))
            #t
            (loop (+ i 1)))))
      #f)))

(define (solve input n hash)
  (let ((hash (make-hash input hash)))
    (let loop ((i 0) (c 0))
      (let ((c (if (valid? hash i)
                 (+ c 1)
                 c)))
        (if (= c n)
          i
          (loop (+ i 1) c))))))

(let ((input (import-input)))
  (let ((part/1 (solve input 64 hash/1)))
    (print part/1) (assert (= part/1 18626)))
  (let ((part/2 (solve input 64 hash/2)))
    (print part/2) (assert (= part/2 20092))))
