(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (matchable)
        (srfi 1))

(define (import-input path)
  (read-lines (open-input-file path)))

(define (replace str a b)
  (string-translate* str (list (cons a b))))

(define (evaluate/1 str)
  (let ((lst (string-split str " ")))
    (let evaluate/1/h ((lst (cdr lst)) (acc (string->number (car lst))))
      (match lst
        (("+" n . tail) (evaluate/1/h tail (+ acc (string->number n))))
        (("*" n . tail) (evaluate/1/h tail (* acc (string->number n))))
        (_ acc)))))

(define (evaluate/2 str)
  (match (irregex-extract "[0-9]+ \\+ [0-9]+" str)
    ((match . _)
     (evaluate/2 (replace str match (number->string (apply + (map string->number (string-split match " +")))))))
    (_ (evaluate/1 str))))

(define (solve proc input)
  (define (solve/h str)
    (match (irregex-extract "\\([^)(]+\\)" str)
      ((match . _)
       (solve/h (replace str match (number->string (proc (apply string-append (string-split match ")(")))))))
      (_ str)))
  (print (apply + (map proc (map solve/h input)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve evaluate/1 input)
    (solve evaluate/2 input)))
