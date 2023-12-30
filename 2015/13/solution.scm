(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 69))

(define (connect! graph a b val)
  (hash-table-update!/default graph a
    (lambda (_)
      (hash-table-set! _ b val)
      _)
    (make-hash-table)))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (a _ op val _ _ _ _ _ _ b)
            (let ((val (string->number val)))
              (connect! acc a b
                (cond
                  ((string=? op "gain") (+ val))
                  ((string=? op "lose") (- val))))))
          (string-split i " .")))
      (read-lines))
    acc))

(define (value graph lst)
  (let* ((vec (list->vector lst)) (len (vector-length vec)))
    (do ((i 0 (+ i 1))
         (acc 0 (let ((data (hash-table-ref graph (vector-ref vec i))))
                  (+ (hash-table-ref data (vector-ref vec (modulo (- i 1) len)))
                     (hash-table-ref data (vector-ref vec (modulo (+ i 1) len)))
                     acc))))
      ((= i len) acc))))

(define (edit! graph)
  (for-each
    (lambda (i)
      (connect! graph "you" i 0)
      (connect! graph i "you" 0))
    (hash-table-keys graph)))

(define (solve input)
  (foldl
    (lambda (acc i)
      (max acc (value input i)))
    0 (permutations (hash-table-keys input))))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 618)))
  (edit! input)
  (let ((part/2 (solve input)))
    (print part/2) (assert (= part/2 601))))
