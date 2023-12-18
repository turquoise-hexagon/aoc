(import
  (chicken io)
  (chicken string))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i "x")))
    (read-lines)))

(define (proc/1 l w h)
  (let ((a (* l w))
        (b (* w h))
        (c (* h l)))
    (+ (* 2 a)
       (* 2 b)
       (* 2 c)
       (min a b c))))

(define (proc/2 l w h)
  (let ((a (* 2 (+ l w)))
        (b (* 2 (+ w h)))
        (c (* 2 (+ h l))))
    (+ (min a b c)
       (* l w h))))

(define (solve input proc)
  (apply +
    (map
      (lambda (i)
        (apply proc i))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 1606483)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 3842356))))
