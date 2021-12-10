(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define matched
  '((#\> . #\<)
    (#\} . #\{)
    (#\) . #\()
    (#\] . #\[)))

(define scores/1
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(define scores/2
  '((#\{ . 3)
    (#\( . 1)
    (#\[ . 2)
    (#\< . 4)))

(define (score lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
      acc
      (if (member (car lst) (map cdr matched))
        (loop (cdr lst) (cons (car lst) acc))
        (if (char=? (car acc) (cdr (assoc (car lst) matched)))
          (loop (cdr lst) (cdr acc))
          (cdr (assoc (car lst) scores/1)))))))

(define (import-input)
  (let ((lst (map score (map string->list (read-lines)))))
    (partition list? lst)))

(define (median lst)
  (list-ref (sort lst <) (quotient (length lst) 2)))

(define (solve/1 input)
  (apply + input))

(define (solve/2 input)
  (median (map
            (lambda (lst)
              (foldl
                (lambda (acc char)
                  (+ (* acc 5) (cdr (assoc char scores/2))))
                0 lst))
            input)))

(receive (lsts nums) (import-input)
  (print (solve/1 nums))
  (print (solve/2 lsts)))
