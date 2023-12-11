(import
  (euler)
  (chicken io))

(define (increment! vec i)
  (vector-set! vec i (+ (vector-ref vec i) 1)))

(define (counts array)
  (let ((acc (map (lambda (i) (make-vector i 0)) (array-dimensions array))))
    (for-each
      (lambda (i)
        (when (char=? (array-ref array i) #\#)
          (for-each increment! acc i)))
      (array-indexes array))
    (map vector->list acc)))

(define (import-input)
  (counts (list->array (map string->list (read-lines)))))

(define (adjust lst m)
  (let loop ((lst lst) (total 0))
    (if (null? lst)
      '()
      (let ((_ (car lst)))
        (let subloop ((i 0))
          (if (= i _)
            (loop (cdr lst) (+ total (if (= _ 0) m 1)))
            (cons total (subloop (+ i 1)))))))))

(define (compute lst multiplier)
  (let loop ((lst (adjust lst multiplier)) (i 0) (total 0) (partial 0))
    (if (null? lst)
      total
      (loop (cdr lst) (+ i 1) (+ total (- (* i (car lst)) partial)) (+ partial (car lst))))))

(define (solve input multiplier)
  (apply +
    (map
      (lambda (i)
        (compute i multiplier))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input 2)))
    (print part/1) (assert (= part/1 10292708)))
  (let ((part/2 (solve input 1000000)))
    (print part/2) (assert (= part/2 790194712336))))
