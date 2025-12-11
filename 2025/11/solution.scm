(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (i . lst)
            (hash-table-set! acc i lst))
          (map string->symbol (string-split i ": "))))
      (read-lines))
    acc))

(define (solve/1 input)
  (let loop ((i 'you))
    (if (equal? i 'out)
      1
      (apply + (map loop (hash-table-ref/default input i '()))))))

(define-inline (_loop i dac fft)
  (if (and (equal? i 'out) dac fft)
    1
    (apply +
      (map
        (case i
          ((dac) (lambda (i) (loop i  #t fft)))
          ((fft) (lambda (i) (loop i dac  #t)))
          (else  (lambda (i) (loop i dac fft))))
        (hash-table-ref/default input i '())))))

(define (solve/2 input)
  (define loop
    (let ((mem (make-hash-table)))
      (lambda (i dac fft)
        (let ((id (list i dac fft)))
          (if (hash-table-exists? mem id)
            (hash-table-ref mem id)
            (let ((acc (_loop i dac fft)))
              (hash-table-set! mem id acc)
              acc))))))
  (loop 'svr #f #f))

(let* ((input (import-input))
       (part/1 (solve/1 input))
       (part/2 (solve/2 input)))
  (print part/1) (assert (= part/1 539))
  (print part/2) (assert (= part/2 413167078187872)))
