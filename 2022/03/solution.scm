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

(define (solve input)
  (apply +
    (map
      (lambda (sack)
        (priority (apply lset-intersection char=? sack)))
      input)))

(let* ((input/1 (import-input)) (input/2 (map (lambda (_) (map join _)) (chop input/1 3))))
  (print (solve input/1))
  (print (solve input/2)))
