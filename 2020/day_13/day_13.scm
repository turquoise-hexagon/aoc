(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (let ((earliest (read-line))
        (schedule (read-line)))
    (list (string->number earliest)
      (let ((lst (string-split schedule ",")))
        (map string->number lst)))))

(define (solve-chinese offsets)
  (let loop ((acc 0))
    (let ((lst (filter
                 (lambda (lst)
                   (receive (id offset) (apply values lst)
                     (= (modulo (+ acc offset) id) 0)))
                 offsets)))
      (if (equal? lst offsets) acc
        (loop (+ acc (apply * (unzip1 lst))))))))

(define (solve/1 input)
  (receive (earliest schedule) (apply values input)
    (let ((lst (map
                 (lambda (id)
                   (let loop ((acc 0))
                     (let ((diff (- acc earliest)))
                       (if (> diff 0) (list diff id)
                         (loop (+ acc id))))))
                 (filter number? schedule))))
      (apply * (assoc (apply min (unzip1 lst)) lst)))))

(define (solve/2 input)
  (receive (earliest schedule) (apply values input)
    (solve-chinese (fold
                     (lambda (a b acc)
                       (if (number? a)
                         (cons (list a b) acc)
                         acc))
                     '() schedule (iota (length schedule))))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
