(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (string->list (read-line)))

(define (_collapse l)
  (if (or (null? l)
          (null? (cdr l)))
    l
    (let ((a (car l))
          (b (cadr l)))
      (if (and (char=? (char-downcase a)
                       (char-downcase b))
               (not (equal? (char-lower-case? a)
                            (char-lower-case? b))))
        (_collapse (cddr l))
        (cons a (_collapse (cdr l)))))))

(define (collapse l)
  (let ((_ (_collapse l)))
    (if (equal? _ l)
      l
      (collapse _))))

(define (solve/1 input)
  (length (collapse input)))

(define (solve/2 input)
  (apply min
    (map
      (lambda (i)
        (let* ((a (integer->char (+ i (char->integer #\a))))
               (b (integer->char (+ i (char->integer #\A))))
               (input (remove (lambda (i) (char=? i a)) input))
               (input (remove (lambda (i) (char=? i b)) input)))
          (length (collapse input))))
      (iota 26))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 9562)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 4934))))
