(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (parse-rules lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (rule)
        (apply (cut hash-table-set! mem <> <>) rule))
      (map (cut irregex-split " -> " <>) lst))
    mem))

(define (parse-template str)
  (let ((lst (string-chop str 1)))
    (append (map (cut apply string-append <>)
             (zip lst (cdr lst)))
      `(,(last lst)))))
      
(define (import-input)
  (receive (template _ . rules) (apply values (read-lines))
    (values (parse-template template) (parse-rules rules))))

(define (increment! mem key #!optional (n 1))
  (hash-table-set! mem key (+ (hash-table-ref/default mem key 0) n)))

(define (string-ref/h str index)
  (string (string-ref str index)))

(define (iterate/h mem rules)
  (let ((acc (make-hash-table)))
    (hash-table-for-each mem
      (lambda (str cnt)
        (if (hash-table-exists? rules str)
          (let ((res (hash-table-ref rules str)))
            (increment! acc (string-append (string-ref/h str 0) res) cnt)
            (increment! acc (string-append res (string-ref/h str 1)) cnt))
          (increment! acc str cnt))))
    acc))

(define (iterate template rules n)
  (let ((mem (make-hash-table)))
    (for-each (cut increment! mem <>) template)
    (foldl
      (lambda (mem _)
        (iterate/h mem rules))
      mem (iota n))))

(define (freq mem)
  (let ((acc (make-hash-table)))
    (hash-table-for-each mem
      (lambda (str cnt)
        (increment! acc (string-ref/h str 0) cnt)))
    (hash-table-values acc)))

(define (solve template rules n)
  (let ((lst (freq (iterate template rules n))))
    (- (apply max lst)
       (apply min lst))))

(receive (template rules) (import-input)
  (print (solve template rules 10))
  (print (solve template rules 40)))
