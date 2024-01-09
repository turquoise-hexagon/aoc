(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (chicken time posix)
  (euler)
  (matchable)
  (srfi 69))

(define (timestamp str)
  (apply
    (lambda (time _)
      (local-time->seconds (string->time time "%Y-%m-%d %H:%M")))
    (string-split str "[]")))

(define (dump! table id prev time minute)
  (let ((minutes (quotient (- time prev) 60)))
    (do ((i 0 (+ i 1))) ((= i minutes))
      (hash-table-update!/default table id
        (lambda (table)
          (hash-table-update!/default table (modulo (- minute i 1) 60) add1 0)
          table)
        (make-hash-table)))))

(define (process lst)
  (let ((acc (make-hash-table)))
    (let loop ((lst lst) (id 0) (prev 0))
      (if (null? lst) acc
        (match (car lst)
          ((time _ _ _ _ minute "Guard" id _ _)
           (loop (cdr lst) id time))
          ((time _ _ _ _ minute "falls" _)
           (loop (cdr lst) id time))
          ((time _ _ _ _ minute "wakes" _) (dump! acc id prev time minute)
           (loop (cdr lst) id time)))))))

(define (import-input)
  (process
    (sort
      (map
        (lambda (i)
          (cons (timestamp i)
            (map
              (lambda (i)
                (let ((_ (string->number i)))
                  (if _ _ i)))
              (string-split i "[- :]#"))))
        (read-lines))
      (lambda (a b)
        (< (car a)
           (car b))))))

(define (solve input proc)
  (let* ((id
           (extremum
             (hash-table-keys input)
             (lambda (id)
               (apply proc (hash-table-values (hash-table-ref input id))))
             >))
         (minute
           (extremum
             (hash-table-keys (hash-table-ref input id))
             (lambda (minute)
               (hash-table-ref (hash-table-ref input id) minute))
             >)))
    (* id minute)))

(let ((input (import-input)))
  (let ((part/1 (solve input +)))
    (print part/1) (assert (= part/1 106710)))
  (let ((part/2 (solve input max)))
    (print part/2) (assert (= part/2 10491))))
