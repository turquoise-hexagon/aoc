(import
  (chicken io)
  (chicken string)
  (matchable))

(define (parse-command str)
  (receive (direction value) (apply values (string-split str " "))
    (list (string->symbol direction) (string->number value))))

(define (import-input)
  (map parse-command (read-lines)))

(define (solve input)
  (foldl
    (match-lambda*
      (((position depth/1 depth/2) (direction value))
       (case direction
         ((down)    (list position (+ depth/1 value) depth/2))
         ((up)      (list position (- depth/1 value) depth/2))
         ((forward) (list (+ position value) depth/1 (+ depth/2 (* depth/1 value)))))))
    '(0 0 0) input))

(let ((input (import-input)))
  (receive (position depth/1 depth/2) (apply values (solve input))
    (print (* position depth/1))
    (print (* position depth/2))))
