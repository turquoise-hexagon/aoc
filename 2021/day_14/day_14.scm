(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse-rules lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (rule)
        (receive (a b) (apply values rule)
          (hash-table-set! acc (string-chop a 1) b)))
      (map (cut irregex-split " -> " <>) lst))
    acc))

(define (parse-template str)
  (let ((lst (string-chop str 1)))
    (append (zip lst (cdr lst))
      `((,(last lst))))))

(define (import-input)
  (receive (template _ . rules) (apply values (read-lines))
    (values (parse-template template) (parse-rules rules))))

(define (increment! mem key #!optional (step 1))
  (hash-table-set! mem key (+ (hash-table-ref/default mem key 0) step)))

(define (iterate/h mem rules)
  (let ((acc (make-hash-table)))
    (hash-table-for-each mem
      (lambda (pair cnt)
        (if (hash-table-exists? rules pair)
          (let ((char (hash-table-ref rules pair)))
            (increment! acc (list (car  pair) char) cnt)
            (increment! acc (list char (cadr pair)) cnt))
          (increment! acc pair cnt))))
    acc))

(define (iterate mem rules n)
  (foldl
    (lambda (acc _)
      (iterate/h acc rules))
    mem (iota n)))

(define (frequencies mem)
  (let ((acc (make-hash-table)))
    (hash-table-for-each mem
      (lambda (pair cnt)
        (increment! acc (car pair) cnt)))
    (hash-table-values acc)))

(define (solve template rules n)
  (let ((acc (make-hash-table)))
    (for-each (cut increment! acc <>) template)
    (let ((frequencies (frequencies (iterate acc rules n))))
      (- (apply max frequencies)
         (apply min frequencies)))))

(receive (template rules) (import-input)
  (print (solve template rules 10))
  (print (solve template rules 40)))
