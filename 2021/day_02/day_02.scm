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

(define (solve/1 input)
  (let ((res (foldl
               (match-lambda*
                 (((horizontal depth) (direction value))
                  (case direction
                    ((forward) (list (+ horizontal value) depth))
                    ((down)    (list horizontal (+ depth value)))
                    ((up)      (list horizontal (- depth value))))))
               '(0 0) input)))
    (apply * res)))

(define (solve/2 input)
  (let ((res (foldl
               (match-lambda*
                 (((aim horizontal depth) (direction value))
                  (case direction
                    ((down)    (list (+ aim value) horizontal depth))
                    ((up)      (list (- aim value) horizontal depth))
                    ((forward) (list aim (+ horizontal value) (+ depth (* aim value)))))))
               '(0 0 0) input)))
    (apply * (cdr res))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
