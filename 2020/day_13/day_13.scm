(import (chicken io)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 1))

(define (import-input path)
  (match (read-lines (open-input-file path))
    ((earliest schedule)
     (list (string->number earliest)
           (map string->number (string-split schedule ","))))))

(define (solve/1 input)
  (match input
    ((earliest schedule)
     (let ((lst (map
                  (lambda (n)
                    (let solve/1/h ((current 0))
                      (if (> current earliest)
                          (cons (- current earliest) n)
                          (solve/1/h (+ current n)))))
                  (filter (cut number? <>) schedule))))
       (let ((lst (assoc (apply min (map car lst)) lst)))
         (print (* (car lst)
                   (cdr lst))))))))

(define (solve/2 input)
  (match input
   ((earliest schedule)
    (let ((offsets (fold
                     (lambda (a b acc)
                       (if (number? a)
                           (cons (cons a b) acc)
                           acc))
                     (list) schedule (iota (length schedule)))))
      (print (let solve/2/h ((current 0))
               (let ((lst (filter
                            (lambda (lst)
                              (match lst
                                ((id . offset)
                                 (= (modulo (+ current offset) id) 0))))
                            offsets)))
                 (if (= (length lst) (length offsets))
                     current
                     (solve/2/h (+ current (apply lcm (map car lst))))))))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
