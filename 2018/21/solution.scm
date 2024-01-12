(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (srfi 69))

(define (import-input)
  (string->number (list-ref (string-split (list-ref (read-lines) 8) " ") 1)))

(define (solve input flag)
  (let ((cache (make-hash-table)))
    (let loop ((i 0) (prev 0))
      (let ((t (fxior i 65536)) (i input))
        (let subloop ((t t) (i i))
          (let ((i (fxand (* (fxand (+ i (fxand t 255)) 16777215) 65899) 16777215)))
            (if (> 256 t)
              (if (not flag)
                i
                (if (hash-table-exists? cache i)
                  prev
                  (begin
                    (hash-table-set! cache i #t)
                    (loop i i))))
              (subloop (quotient t 256) i))))))))

(let ((input (import-input)))
  (let ((part/1 (solve input #f)))
    (print part/1) (assert (= part/1 5745418)))
  (let ((part/2 (solve input #t)))
    (print part/2) (assert (= part/2 5090905))))
