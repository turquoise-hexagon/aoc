(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse-rules lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (lst)
        (receive (a b) (apply values lst)
          (hash-table-set! acc (string-chop a 1) b)))
     (map (cut string-split <> "-> ") lst))
    acc))

(define (import-input)
  (receive (template _ . rules) (apply values (read-lines))
    (values (string-chop template 1) (parse-rules rules))))

(define (increment! mem key #!optional (step 1))
  (hash-table-set! mem key (+ (hash-table-ref/default mem key 0) step)))

(define (iterate/h chars pairs rules)
  (let ((acc (make-hash-table)))
    (hash-table-for-each pairs
      (lambda (pair cnt)
        (let ((char (hash-table-ref rules pair)))
          (increment! chars char cnt)
          (increment! acc `(,(car  pair) ,char) cnt)
          (increment! acc `(,char ,(cadr pair)) cnt))))
    acc))

(define (iterate chars pairs rules n)
  (foldl
    (lambda (acc _)
      (iterate/h chars acc rules))
    pairs (iota n)))

(define (solve template rules n)
  (let ((chars (make-hash-table)) (pairs (make-hash-table)))
    (for-each (cut increment! chars <>) template)
    (for-each (cut increment! pairs <>) (zip template (cdr template)))
    (iterate chars pairs rules n)
    (let ((frequencies (hash-table-values chars)))
      (- (apply max frequencies)
         (apply min frequencies)))))

(receive (template rules) (import-input)
  (print (solve template rules 10))
  (print (solve template rules 40)))
