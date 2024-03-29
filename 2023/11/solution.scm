(import
  (chicken io)
  (srfi 1))

(define (counts lst)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (count (lambda (i) (char=? i #\#)) i))
        i))
    (list lst (apply zip lst))))

(define (import-input)
  (counts (map string->list (read-lines))))

(define (adjust lst multiplier)
  (let loop ((lst lst) (total 0))
    (if (null? lst)
      '()
      (let ((_ (car lst)))
        (let subloop ((i 0))
          (if (= i _)
            (loop (cdr lst) (+ total (if (= _ 0) multiplier 1)))
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
