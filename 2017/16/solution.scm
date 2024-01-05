(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (let*
        ((l (string-length i))
         (a (substring i 0 1))
         (b (substring i 1 l)))
        (cons (string->symbol a)
          (map
            (lambda (i)
              (let ((_ (string->number i))) (if _ _ i)))
            (string-split b "/")))))
    (string-split (read-line) ",")))

(define (string-swap! str a b)
  (let
    ((m (string-ref str a))
     (n (string-ref str b)))
    (string-set! str a n)
    (string-set! str b m)))

(define (string-rotate! str n)
  (let ((l (string-length str)) (c (string-copy str)))
    (do ((i 0 (+ i 1))) ((> i l))
      (string-set! str (modulo i l) (string-ref c (modulo (- i n) l))))))

(define (iterate! str lst)
  (for-each
    (lambda (i)
      (apply
        (case-lambda
          ((_ a) (string-rotate! str a))
          ((op a b)
           (case op
             ((x) (string-swap! str a b))
             ((p)
              (string-swap! str
                (substring-index a str)
                (substring-index b str))))))
        i))
    lst))

(define (solve/1 acc input)
  (iterate! acc input)
  acc)

(define (solve/2 acc input n)
  (let ((cache (make-hash-table)))
    (let loop ((i 1))
      (if (= i n)
        acc
        (let ((id (string-copy acc)))
          (if (hash-table-exists? cache id)
            (let ((_ (hash-table-ref cache id)))
              (hash-table-clear! cache)
              (loop (- n (modulo (- n _) (- i _)))))
            (begin
              (hash-table-set! cache id i)
              (iterate! acc input)
              (loop (+ i 1)))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 "abcdefghijklmnop" input)))
    (print part/1) (assert (string=? part/1 "fgmobeaijhdpkcln")))
  (let ((part/2 (solve/2 "abcdefghijklmnop" input 1000000001)))
    (print part/2) (assert (string=? part/2 "lgmkacfjbopednhi"))))
