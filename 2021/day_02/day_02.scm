(import
  (chicken io)
  (chicken string)
  (matchable))

(define (import-input)
  (map
    (lambda (str)
      (receive (direction value) (apply values (string-split str " "))
        (list (string->symbol direction) (string->number value))))
    (read-lines)))

(define (part/1 aim horizontal depth direction value)
  (case direction
    ((down)    (list aim horizontal (+ depth value)))
    ((up)      (list aim horizontal (- depth value)))
    ((forward) (list aim (+ horizontal value) depth))))

(define (part/2 aim horizontal depth direction value)
 (case direction
   ((down)    (list (+ aim value) horizontal depth))
   ((up)      (list (- aim value) horizontal depth))
   ((forward) (list aim (+ horizontal value) (+ depth (* aim value))))))

(define (solve input proc)
  (apply * (cdr (foldl
                  (match-lambda*
                    (((aim horizontal depth) (direction value))
                     (proc aim horizontal depth direction value)))
                  '(0 0 0) input))))

(let ((input (import-input)))
  (print (solve input part/1))
  (print (solve input part/2)))
