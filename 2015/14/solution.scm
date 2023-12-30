(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map
    (lambda (i)
      (filter-map string->number (string-split i " ")))
    (read-lines)))

(define (compute limit speed run rest)
  (let ((both (+ run rest)))
    (+ (* (quotient limit both) run speed)
       (* (min (modulo limit both) run) speed))))

(define (solve/1 input limit)
  (apply max
    (map
      (lambda (i)
        (apply compute limit i))
      input)))

(define (solve/2 input limit)
  (apply max
    (apply map +
      (map
        (lambda (l)
          (let* ((lst (map
                        (lambda (i)
                          (apply compute l i))
                        input))
                 (res (apply max lst)))
            (map
              (lambda (i)
                (if (= i res) 1 0))
              lst)))
        (iota limit 1)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input 2503)))
    (print part/1) (assert (= part/1 2696)))
  (let ((part/2 (solve/2 input 2503)))
    (print part/2) (assert (= part/2 1084))))
