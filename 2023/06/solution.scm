(import
  (chicken io)
  (chicken irregex))

(define (parse str)
  (irregex-extract "[0-9]+" str))

(define (import-input)
  (list
    (parse (read-line))
    (parse (read-line))))

(define (transform/1 lst)
  (map string->number lst))

(define (transform/2 lst)
  (list (string->number (apply string-append lst))))

(define (solve time distance)
  (apply *
    (map
      (lambda (time distance)
        (do ((i 0 (+ i 1))
             (acc 0 (if (> (* (- time i) i) distance)
                      (+ acc 1)
                      acc)))
          ((> i time) acc)))
      time distance)))

(let ((input (import-input)))
  (let ((part/1 (apply solve (map transform/1 input))))
    (print part/1))
  (let ((part/2 (apply solve (map transform/2 input))))
    (print part/2)))
