(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 1))

(define (parse-instruction str)
  (receive (head tail) (split-at (string->list str) 1)
    `(,(string->symbol (list->string head))
      ,(string->number (list->string tail)))))

(define moveset
  `((N . ,(lambda (position object value) (list (+ position (* value   0+1i)) object)))
    (S . ,(lambda (position object value) (list (+ position (* value   0-1i)) object)))
    (E . ,(lambda (position object value) (list (+ position (* value   1+0i)) object)))
    (W . ,(lambda (position object value) (list (+ position (* value  -1+0i)) object)))
    (F . ,(lambda (position object value) (list (+ position (* value object)) object)))
    (L . ,(lambda (position object value) (list position (* object (expt 0+1i (/ value 90))))))
    (R . ,(lambda (position object value) (list position (/ object (expt 0+1i (/ value 90))))))))

(define (proc/1 lst)
  (foldl
    (match-lambda*
      (((position facing) (action value))
       ((cdr (assoc action moveset)) position facing value)))
    '(0+0i 1+0i) lst))

(define (proc/2 lst)
  (foldl
    (match-lambda*
      (((position waypoint) (action value))
       (let ((proc (cdr (assoc action moveset))))
         (case action
           ((N S E W) (list position (car  (proc waypoint waypoint value))))
           ((L R)     (list position (cadr (proc waypoint waypoint value))))
           ((F)       (list (car (proc position waypoint value)) waypoint))))))
    '(0+0i 10+1i) lst))

(define (solve input proc)
  (receive (position _) (apply values (proc input))
    (+ (abs (real-part position))
       (abs (imag-part position)))))

(define (import-input)
  (map parse-instruction (read-lines)))

(let ((input (import-input)))
  (print (solve input proc/1))
  (print (solve input proc/2)))
