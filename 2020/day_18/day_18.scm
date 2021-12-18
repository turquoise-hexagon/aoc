(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (matchable))

(define (import-input)
  (read-lines))

(define (replace str a b)
  (string-translate* str `((,a . ,b))))

(define (eval/1 str)
  (let ((lst (string-split str " ")))
    (let loop ((lst (cdr lst)) (acc (string->number (car lst))))
      (match lst
        (("+" n . tail) (loop tail (+ acc (string->number n))))
        (("*" n . tail) (loop tail (* acc (string->number n))))
        (_ acc)))))

(define (eval/2 str)
  (match (irregex-extract "[0-9]+ \\+ [0-9]+" str)
    ((match . _)
     (eval/2 (replace str match (number->string (foldl + 0 (map string->number (string-split match "+ ")))))))
    (_ (eval/1 str))))

(define (solve input proc)
  (define (loop str)
    (match (irregex-extract "\\([^)(]+\\)" str)
      ((match . _)
       (loop (replace str match (number->string (proc (apply string-append (string-split match ")(")))))))
      (_ str)))
  (foldl + 0 (map proc (map loop input))))

(let ((input (import-input)))
  (print (solve input eval/1))
  (print (solve input eval/2)))
