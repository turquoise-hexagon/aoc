(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse str)
  (bind (a b letter password) (string-split str "- :")
    (list
      (string->number a)
      (string->number b)
      letter (string-chop password 1))))

(define (import-input)
  (map parse (read-lines)))

(define (proc/1 lst)
  (bind (a b letter password) lst
    (<= a (count (lambda (i) (string=? i letter)) password) b)))

(define (proc/2 lst)
  (bind (a b letter password) lst
    (not
      (equal?
        (string=? letter (list-ref password (- a 1)))
        (string=? letter (list-ref password (- b 1)))))))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 564)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 325))))
