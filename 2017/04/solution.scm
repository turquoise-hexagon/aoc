(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (map string->list (string-split i " ")))
    (read-lines)))

(define (proc/1 lst)
  (let ((mem (make-hash-table)))
    (let loop ((lst lst))
      (if (null? lst)
        #t
        (if (hash-table-exists? mem (car lst))
          #f
          (begin
            (hash-table-set! mem (car lst) #t)
            (loop (cdr lst))))))))

(define (proc/2 lst)
  (proc/1
    (map
      (lambda (i)
        (sort i char<?))
      lst)))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 337)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 231))))
