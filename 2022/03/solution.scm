(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map
    (lambda (_)
      (chop _ (/ (length _) 2)))
    (map string->list (read-lines))))

(define (priority char)
  (let ((_ (char->integer char)))
    (if (char-lower-case? char)
      (+ (- _ (char->integer #\a)) 1)
      (+ (- _ (char->integer #\A)) 27))))

(define (solve input)
  (apply +
    (map
      (lambda (_)
        (priority (car (apply lset-intersection char=? _))))
      input)))

(let* ((input/1 (import-input)) (input/2 (chop (map join input/1) 3)))
  (let ((part/1 (solve input/1)))
    (print part/1) (assert (= part/1 8493)))
  (let ((part/2 (solve input/2)))
    (print part/2) (assert (= part/2 2552))))
