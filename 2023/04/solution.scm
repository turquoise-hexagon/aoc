(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse str)
  (bind (_ . data) (string-split str ":|")
    (map
      (lambda (str)
        (string-split str " "))
      data)))

(define (matches lst)
  (let ((mem (make-hash-table)))
    (bind (a b) lst
      (for-each
        (lambda (i)
          (hash-table-set! mem i #t))
        a)
      (count
        (lambda (i)
          (hash-table-exists? mem i))
        b))))

(define (import-input)
  (map matches (map parse (read-lines))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (value)
        (if (= value 0) 0 (expt 2 (- value 1))))
      input)))

(define (solve/2 input)
  (let ((acc (make-vector (length input) 1)))
    (for-each
      (lambda (value index)
        (let ((count (vector-ref acc index)))
          (do ((i 0 (+ i 1))) ((= i value))
            (let ((_ (+ index i 1)))
              (vector-set! acc _ (+ (vector-ref acc _) count))))))
      input (iota (length input)))
    (apply + (vector->list acc))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 23028)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 9236992))))
