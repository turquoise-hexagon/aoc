(import
  (chicken io)
  (euler)
  (srfi 1))

(define (parse-pass str)
  (map
    (lambda (char)
      (case char
        ((#\F #\L) 0)
        ((#\B #\R) 1)))
    (string->list str)))

(define (pass->id lst)
  (receive (col row) (split-at lst 7)
    (+ (* (list->number col 2) 8) (list->number row 2))))

(define (import-input)
  (map pass->id (map parse-pass (read-lines))))

(define (solve/1 input)
  (apply max input))

(define (solve/2 input)
  (let ((alist (map list input)))
    (find
      (lambda (id)
        (and (assoc (+ id 1) alist)
             (assoc (- id 1) alist)
             (not (assoc id  alist))))
      (iota (apply max input)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
