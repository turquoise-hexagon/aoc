(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map
    (lambda (_)
      (chop _ (/ (length _) 2)))
    (map string->list (read-lines))))

(define (priority lst)
  (let ((_ (car lst)))
    (if (char-lower-case? _)
      (+ (- (char->integer _) (char->integer #\a)) 1)
      (+ (- (char->integer _) (char->integer #\A)) 27))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (sack)
        (priority (apply lset-intersection char=? sack)))
      input)))

(define (solve/2 input)
  (apply +
    (map
      (lambda (group)
        (priority (apply lset-intersection char=? (map flatten group))))
      (chop input 3))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
