(import
  (chicken io)
  (euler)
  (srfi 69))

(define (nodes array)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (coord)
        (let ((value (array-ref array coord)))
          (unless (char=? value #\.)
            (hash-table-update!/default acc value (lambda (tmp) (cons coord tmp)) '()))))
      (array-indexes array))
    acc))

(define (import-input)
  (let ((acc (list->array (map string->list (read-lines)))))
    (values acc (nodes acc))))

(define (proc/1 array acc a b)
  (let ((coord (map + a (map - a b))))
    (when (array-exists? array coord)
      (hash-table-set! acc coord #t))))

(define (proc/2 array acc a b)
  (let ((offset (map - a b)))
    (let loop ((coord a))
      (when (array-exists? array coord)
        (hash-table-set! acc coord #t)
        (loop (map + coord offset))))))

(define (solve array nodes proc/1)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (lst)
        (for-each
          (lambda (lst)
            (apply
              (lambda (a b)
                (proc/1 array acc a b)
                (proc/1 array acc b a))
              lst))
          (combinations lst 2)))
      (hash-table-values nodes))
    (hash-table-size acc)))

(let-values (((array nodes) (import-input)))
  (let ((part/1 (solve array nodes proc/1)))
    (print part/1) (assert (= part/1 413)))
  (let ((part/2 (solve array nodes proc/2)))
    (print part/2) (assert (= part/2 1417))))
