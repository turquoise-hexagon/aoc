(import
  (chicken io)
  (chicken string)
  (srfi 69))

(define (import-input)
  (map string->number (string-split (read-line) "\t")))

(define (id vec)
  (string-intersperse (map number->string (vector->list vec))))

(define (solve input)
  (let* ((vec (list->vector input)) (len (vector-length vec)) (mem (make-hash-table)))
    (let loop ((i 0))
      (let ((id (id vec)))
        (if (hash-table-exists? mem id)
          (list i (- i (hash-table-ref mem id)))
          (begin
            (hash-table-set! mem id i)
            (let loop ((i 0) (val 0) (ind 0))
              (if (= i len)
                (begin
                  (vector-set! vec ind 0)
                  (let loop ((i (modulo (+ ind 1) len)) (val val))
                    (unless (= val 0)
                      (vector-set! vec i (+ (vector-ref vec i) 1))
                      (loop (modulo (+ i 1) len) (- val 1)))))
                (let ((tmp (vector-ref vec i)))
                  (if (> tmp val)
                    (loop (+ i 1) tmp i)
                    (loop (+ i 1) val ind)))))
            (loop (+ i 1))))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '(6681 2392)))
