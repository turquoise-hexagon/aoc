(import
  (chicken io))

(define (import-input)
  (string->list (read-line)))

(define (solve input)
  (let loop ((lst input) (cnt 0) (flag #f) (acc/1 0) (acc/2 0))
    (if (null? lst)
      (list acc/1 acc/2)
      (let ((acc/2 (if flag (+ acc/2 1) acc/2)))
        (case (car lst)
          ((#\{)
           (if flag
             (loop (cdr lst) (+ cnt 0) flag acc/1 acc/2)
             (loop (cdr lst) (+ cnt 1) flag acc/1 acc/2)))
          ((#\})
           (if flag
             (loop (cdr lst) (- cnt 0) flag (+ acc/1   0) acc/2)
             (loop (cdr lst) (- cnt 1) flag (+ acc/1 cnt) acc/2)))
          ((#\<) (loop ( cdr lst) cnt   #t acc/1 (- acc/2 0)))
          ((#\>) (loop ( cdr lst) cnt   #f acc/1 (- acc/2 1)))
          ((#\!) (loop (cddr lst) cnt flag acc/1 (- acc/2 1)))
          (else  (loop ( cdr lst) cnt flag acc/1 (- acc/2 0))))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '(10800 4522)))
