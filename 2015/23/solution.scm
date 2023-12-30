(import
  (chicken io)
  (chicken string)
  (chicken fixnum))

(define (convert n)
  (cond
    ((string->number n) => identity)
    ((string=? n "a") 0)
    ((string=? n "b") 1)))

(define (import-input)
  (list->vector
    (map
      (lambda (i)
        (apply
          (case-lambda
            ((op a)   (list (string->symbol op) (convert a)))
            ((op a b) (list (string->symbol op) (convert a) (convert b))))
          (string-split i " ,")))
      (read-lines))))

(define-inline (fxone? n)
  (fx= n 1))

(define (solve input init)
  (let ((acc (vector init 0)) (len (vector-length input)))
    (let loop ((i 0))
      (if (>= i len)
        (vector-ref acc 1)
        (apply
          (case-lambda
            ((op a)
             (case op
               ((hlf) (vector-set! acc a (fx/ (vector-ref acc a) 2)) (loop (fx+ i 1)))
               ((tpl) (vector-set! acc a (fx* (vector-ref acc a) 3)) (loop (fx+ i 1)))
               ((inc) (vector-set! acc a (fx+ (vector-ref acc a) 1)) (loop (fx+ i 1)))
               ((jmp) (loop (fx+ i a)))))
            ((op a b)
             (case op
               ((jie) (loop (fx+ i (if (fxeven? (vector-ref acc a)) b 1))))
               ((jio) (loop (fx+ i (if (fxone?  (vector-ref acc a)) b 1)))))))
          (vector-ref input i))))))

(let ((input (import-input)))
  (let ((part/1 (solve input 0)))
    (print part/1) (assert (= part/1 255)))
  (let ((part/2 (solve input 1)))
    (print part/2) (assert (= part/2 334))))
