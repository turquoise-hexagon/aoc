(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 113)
        (srfi 128))

(define comparator
  (make-comparator char? char=? char<? char->integer))

(define (import-input path)
  (map
    (lambda (str)
      (map
        (lambda (lst)
          (list->set comparator lst))
        (map string->list (irregex-split "\n" str))))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (solve proc input)
  (display (apply + (map set-size (map (lambda (sets) (apply proc sets)) input))))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve set-union        input)
    (solve set-intersection input)))
