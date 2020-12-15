(import (chicken io)
        (chicken process-context)
        (matchable))

(define (import-input path)
  (list->vector (map list->vector (map string->list (read-lines (open-input-file path))))))

(define (encountered-trees input right down)
  (let ((h (vector-length input))
        (w (vector-length (vector-ref input 0))))
    (do ((i 0 (+ i down))
         (j 0 (+ j right))
         (acc 0 (if (char=? #\# (vector-ref (vector-ref input (remainder i h)) (remainder j w)))
                    (+ acc 1)
                    acc)))
      ((>= i h) acc))))

(define (solve input lst)
  (print (apply * (map
                    (lambda (lst)
                      (match lst ((right down) (encountered-trees input right down))))
                    lst))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input '((3 1)))
    (solve input '((1 1) (3 1) (5 1) (7 1) (1 2)))))
