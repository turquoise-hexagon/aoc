(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (let ((lst (string->list str)))
        (list (string->symbol (list->string (take lst 1)))
              (string->number (list->string (drop lst 1))))))
    (read-lines (open-input-file path))))

(define moveset
  `((N . ,(lambda (position object value) (cons (+ position (* value   0+1i)) object)))
    (S . ,(lambda (position object value) (cons (+ position (* value   0-1i)) object)))
    (E . ,(lambda (position object value) (cons (+ position (* value   1+0i)) object)))
    (W . ,(lambda (position object value) (cons (+ position (* value  -1+0i)) object)))
    (F . ,(lambda (position object value) (cons (+ position (* value object)) object)))
    (L . ,(lambda (position object value) (cons position (* object (expt 0+1i (/ value 90))))))
    (R . ,(lambda (position object value) (cons position (/ object (expt 0+1i (/ value 90))))))))

(define (solve/1 position facing input)
  (for-each
    (match-lambda
      ((key value)
       (match ((cdr (assoc key moveset)) position facing value)
         ((a . b)
          (set! position a)
          (set! facing   b)))))
    input)
  (print (apply + (map abs (list (real-part position)
                                 (imag-part position))))))

(define (solve/2 position waypoint input)
  (for-each
    (match-lambda
      ((key value)
       (let ((proc (cdr (assoc key moveset))))
         (case key
           ((N S E W) (set! waypoint (car (proc waypoint waypoint value))))
           ((L R)     (set! waypoint (cdr (proc waypoint waypoint value))))
           ((F)       (set! position (car (proc position waypoint value))))))))
    input)
  (print (apply + (map abs (list (real-part position)
                                 (imag-part position))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 0+0i  1+0i input)
    (solve/2 0+0i 10+1i input)))
