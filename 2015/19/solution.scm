(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler-syntax)
  (srfi 69))

(define (import-input)
  (apply
    (lambda (str rules)
      (list (car str) (map (lambda (i) (string-split i " =>")) rules)))
    (foldl
      (lambda (acc i)
        (if (string=? i "")
          (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))))

(define (run str a b)
  (let loop ((i 0))
    (let ((i (substring-index a str i)))
      (if i 
        (cons (string-append (substring str 0 i) b (substring str (+ i (string-length a)))) (loop (+ i 1)))
        '()))))

(define (solve/1 str rules)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (for-each
          (lambda (i)
            (hash-table-set! acc i #t))
          (apply run str i)))
      rules)
    (hash-table-size acc)))

(define (solve/2 str rules)
  (let ((rules (map reverse rules)))
    (call/cc
      (lambda (return)
        (define-memoized (loop cur acc)
          (if (string=? cur "e")
            (return acc)
            (for-each
              (lambda (i)
                (loop i (+ acc 1)))
              (sort
                (join
                  (map
                    (lambda (i)
                      (apply run cur i))
                    rules))
                (lambda (a b)
                  (< (string-length a)
                     (string-length b)))))))
        (loop str 0)))))

(let ((input (import-input)))
  (let ((part/1 (apply solve/1 input)))
    (print part/1) (assert (= part/1 509)))
  (let ((part/2 (apply solve/2 input)))
    (print part/2) (assert (= part/2 195))))
